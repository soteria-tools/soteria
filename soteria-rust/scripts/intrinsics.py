#!/usr/bin/env python3

import subprocess
import json
from typing import Any, Generic, Sequence, TypeVar
from common import *


class PathElem(TypedDict):
    Ident: tuple[str]


class AttributeDocComment(TypedDict):
    DocComment: str


class AttributeUnknown(TypedDict):
    Unknown: Optional[Any]


class AttribInfo(TypedDict):
    attributes: list[AttributeDocComment | AttributeUnknown]


class ItemMeta(TypedDict):
    name: list[PathElem]
    attr_info: AttribInfo


class TypeParam(TypedDict):
    index: int
    name: str


class GenericParams(TypedDict):
    types: list[TypeParam]


class Signature(TypedDict):
    generics: GenericParams
    inputs: list[Any]


T = TypeVar("T")


class ResultOk(TypedDict, Generic[T]):
    Ok: T


class ResultError(TypedDict):
    Err: None


Result = ResultOk[T] | ResultError


class Local(TypedDict):
    index: int
    name: Optional[str]
    ty: Any


class Locals(TypedDict):
    arg_count: int
    locals: list[Local]


class UnstructuredBody(TypedDict):
    locals: Locals


class Body(TypedDict):
    Unstructured: UnstructuredBody


class FunDecl(TypedDict):
    item_meta: ItemMeta
    signature: Signature
    body: Result[Body]


# Parse the intrinsics via Charon, and return them
def get_intrinsics() -> dict[str, FunDecl]:
    file_json = (PWD / "intrinsics.ullbc.temp").resolve()
    if "--cached" in sys.argv:
        pprint(f"Using cached intrinsics", inc=True)
    else:
        pprint(f"Loading intrinsics...", inc=True)
        file_rs = (PWD / "intrinsics.rs").resolve()
        with open(file_rs, "w") as f:
            f.write(
                """
                #![feature(core_intrinsics)]
                pub use std::intrinsics::*;
                """
            )

        charon_cmd = f"charon rustc --ullbc \
            --dest-file {file_json} \
            --start-from core::intrinsics \
            --include core::intrinsics \
            --exclude core::intrinsics::const_allocate \
            --exclude core \
            -- {file_rs} --crate-type=lib > /dev/null 2>&1"
        subprocess.run(charon_cmd, shell=True, check=True)
        file_rs.unlink(missing_ok=True)

    ullbc = json.loads(file_json.read_text())
    intrinsics: dict[str, FunDecl] = {
        "::".join(i["Ident"][0] for i in fun["item_meta"]["name"][2:]): fun
        for fun in ullbc["translated"]["fun_decls"]
        if fun is not None
        and fun["item_meta"]["name"][0]["Ident"][0] == "core"
        and fun["item_meta"]["name"][1]["Ident"][0] == "intrinsics"
        # for now, we ignore intrinsics in submodules (mir, simd, fallbacks)
        and len(fun["item_meta"]["name"]) == 3
    }
    pprint(f"Found {BOLD}{len(intrinsics)}{RESET} intrinsics", inc=True)
    return intrinsics


# Generate the OCaml interface for the intrinsics stubs, along with the stub file itself;
# returns [interface string, stubs string]
def generate_interface(intrinsics: dict[str, FunDecl]) -> tuple[str, str]:
    class IntrinsicInfo(TypedDict):
        name: str
        path: str
        doc: list[str]
        args: list[str]
        types: list[str]

    pprint(f"Generating OCaml interface and stubs...", inc=True)

    def sanitize_comment(comment: str) -> str:
        return "{@markdown[\n" + comment.replace("(*", "( *").strip() + "\n]}"

    def sanitize_var_name(name: str) -> str:
        ocaml_names = ["val"]
        if name in ocaml_names:
            return f"{name}_"
        return name

    intrinsics_info: list[IntrinsicInfo] = []
    for fun in intrinsics.values():
        name = fun["item_meta"]["name"][-1]["Ident"][0]
        path = "::".join(i["Ident"][0] for i in fun["item_meta"]["name"])
        doc = "\n".join(
            attrib["DocComment"]
            for attrib in fun["item_meta"]["attr_info"]["attributes"]
            if "DocComment" in attrib
        )
        doc = sanitize_comment(doc)
        arg_count = len(fun["signature"]["inputs"])
        args = (
            [
                sanitize_var_name(param["name"] or "arg")
                for param in fun["body"]["Ok"]["Unstructured"]["locals"]["locals"][
                    1 : arg_count + 1
                ]
            ]
            if "Ok" in fun["body"]
            else [f"arg{i}" for i in range(1, arg_count + 1)]
        )
        types = [
            (
                param_l
                if not (param_l := param["name"].lower()) in args
                else "t_" + param_l
            )
            for param in fun["signature"]["generics"]["types"]
        ]
        intrinsics_info.append(
            {
                "name": name,
                "path": path,
                "doc": doc,
                "args": args,
                "types": types,
            }
        )
    intrinsics_info.sort(key=lambda x: x["name"])

    interface_str = """
        (** This file was generated with [scripts/intrinsics.py] -- do not edit it manually,
            instead modify the script and re-run it. *)

        open Charon
        open Rustsymex

        module M: (State: State_intf.S) -> sig
            type rust_val := State.Sptr.t Rust_val.t
            type ret := (rust_val * State.t, Error.t State.err * State.t, State.serialized) Result.t
            type fun_exec := (
                UllbcAst.fun_decl ->
                args:rust_val list ->
                State.t ->
                (rust_val * State.t,Error.t State.err * State.t, State.serialized) Result.t)
    """

    stubs_str = """
        (** This file was generated with [scripts/intrinsics.py] -- do not edit it manually,
            instead modify the script and re-run it. *)

        [@@@warning "-unused-value-declaration"]

        open Rustsymex

        module M (State: State_intf.S) = struct
    """

    for info in intrinsics_info:
        nl = "\n"
        args_and_tys: Sequence[tuple[Optional[str], str]] = []

        # special-case catch_unwind; it needs to also received the function-execution function
        if info["name"] == "catch_unwind":
            args_and_tys += [(None, "fun_exec")]

        args_and_tys += (
            [(type, "Types.ty") for type in info["types"]]
            + [(arg, "rust_val") for arg in info["args"]]
            + [(None, "State.t")]
        )
        interface_str += f"""
            (** {info["doc"]} *)
            val {info["name"]} : {' -> '.join([
                f"{arg}:{ty}" if arg is not None else ty
                for (arg, ty) in args_and_tys
            ] + ["ret"])}
        """

        stubs_str += f"""
            let {info['name']} {' '.join(
                f"~{arg}:_" if arg else "_"
                for (arg, _) in args_and_tys)} =
                not_impl "Unsupported intrinsic: {info['name']}"
        """

    interface_str += """

            val eval_fun : string -> fun_exec -> Types.generic_args -> args:rust_val list -> State.t -> ret
        end
    """

    stubs_str += f"""
            include Intrinsics_impl.M (State)

            let eval_fun name fun_exec (generics: Charon.Types.generic_args) ~args =
                match name, generics.types, args with
    """

    for info in intrinsics_info:
        types = "; ".join(info["types"])
        args = "; ".join(a if a != info["name"] else f"{a}_" for a in info["args"])

        call_args = [
            f"~{arg}" if arg != info["name"] else f"~{arg}:{arg}_"
            for arg in info["types"] + info["args"]
        ]
        if info["name"] == "catch_unwind":
            call_args.insert(0, "fun_exec")

        stubs_str += f"""
                    | "{info['name']}", [{types}], [{args}] ->
                        {info['name']} {' '.join(call_args)}
        """
    stubs_str += """
                | name, _, _ -> fun _ ->
                    Fmt.kstr not_impl "Intrinsic %s not found, or not called with the right arguments" name
        end
    """

    return interface_str, stubs_str


def write_ocaml_file(filename: Path, content: str) -> None:
    filename = filename.resolve()
    with open(filename, "w") as f:
        f.write(content)
    try:
        subprocess.run(
            f"ocamlformat --inplace {filename} > /dev/null 2>&1",
            shell=True,
            check=True,
        )
    except subprocess.CalledProcessError:
        pprint(
            f"{YELLOW}{BOLD}Warning{RESET}: Failed to format {filename} with ocamlformat.",
            inc=True,
        )
    pprint(f"Wrote OCaml file {BOLD}{filename}{RESET}", inc=True)


if __name__ == "__main__":
    intrinsics = get_intrinsics()
    interface, stubs = generate_interface(intrinsics)

    ml_folder = PWD / ".." / "lib" / "builtins"
    write_ocaml_file(ml_folder / "intrinsics.mli", interface)
    write_ocaml_file(ml_folder / "intrinsics.ml", stubs)

    # generate the "intrinsincs_impl.ml" file if it doesn't exist
    impl_file = ml_folder / "intrinsics_impl.ml"
    if not impl_file.exists():
        impl_content = """
        module M (State: State_intf.S) = struct
        end
        """
        write_ocaml_file(impl_file, impl_content)
