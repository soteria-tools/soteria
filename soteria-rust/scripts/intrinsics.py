#!/usr/bin/env python3

import json
import subprocess
from typing import Any, Generic, Literal, Sequence, TypedDict, TypeVar

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


class AdtId(TypedDict):
    Adt: int


class InnerTypeAdt(TypedDict):
    id: str | AdtId
    generics: GenericParams


class TypeAdt(TypedDict):
    Adt: InnerTypeAdt


class TypeLiteralUInt(TypedDict):
    UInt: str


class TypeLiteralInt(TypedDict):
    Int: str


class TypeLiteralFloat(TypedDict):
    Float: str


TypeLiteralBool = Literal["Bool"]


class TypeLiteral(TypedDict):
    Literal: TypeLiteralUInt | TypeLiteralInt | TypeLiteralFloat | TypeLiteralBool


class TypeTypeVar(TypedDict):
    TypeVar: Any


class TypeRef(TypedDict):
    Ref: Any


class TypePtr(TypedDict):
    RawPtr: Any


TypeNever = Literal["Never"]


Type = TypeAdt | TypeLiteral | TypeTypeVar | TypeRef | TypePtr | TypeNever


class DedupedType(TypedDict):
    Deduplicated: int


class HashConsedType(TypedDict):
    HashConsedValue: tuple[int, Type]


UniqueType = DedupedType | HashConsedType


class Signature(TypedDict):
    generics: GenericParams
    inputs: list[UniqueType]
    output: UniqueType


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
    body: Body


InterpTypeBase = Literal[
    "unit", "int", "bool", "float", "ptr", "unknown", "fun_exec", "meta_ty"
]
InterpTypeMeta = Optional[str]
InterpType = tuple[InterpTypeBase, InterpTypeMeta]

type_cache: dict[int, Type] = {}


def type_of(unique_ty: UniqueType) -> InterpType:
    ty: Type
    if "Deduplicated" in unique_ty:
        type_id = unique_ty["Deduplicated"]
        if type_id not in type_cache:
            raise ValueError(f"Unknown deduplicated type id: {type_id}")
        ty = type_cache[type_id]
    elif "HashConsedValue" in unique_ty:
        type_id, inner_ty = unique_ty["HashConsedValue"]
        type_cache[type_id] = inner_ty
        ty = inner_ty

    if ty == "Never":
        return "unit", None

    if "Literal" in ty:
        if ty["Literal"] == "Bool":
            return "bool", None
        if "Int" in ty["Literal"] or "UInt" in ty["Literal"]:
            return "int", (
                ty["Literal"]["Int"]
                if "Int" in ty["Literal"]
                else ty["Literal"]["UInt"]
            )
        if "Float" in ty["Literal"]:
            return "float", ty["Literal"]["Float"]

    if "RawPtr" in ty or "Ref" in ty:
        return "ptr", None

    if "Adt" in ty:
        if ty["Adt"]["id"] == "Tuple" and len(ty["Adt"]["generics"]["types"]) == 0:
            return "unit", None

    if "TypeVar" in ty:
        return "unknown", None

    # raise RuntimeError(f"Unhandled type: {ty}")
    return "unknown", None


# try traversing the whole JSON to cache all types
def traverse_types(x: Any, prev_key: Optional[str] = None) -> None:
    if isinstance(x, list):
        for v in x:
            traverse_types(v, prev_key)
        return

    if not isinstance(x, dict):
        return

    if "HashConsedValue" in x and prev_key != "tref" and prev_key != "trait_refs":
        type_id, inner_ty = x["HashConsedValue"]
        type_cache[type_id] = inner_ty

    for k, v in x.items():
        traverse_types(v, k)


input_type: dict[InterpTypeBase, str] = {
    "unit": "unit",
    "int": "[< Typed.T.sint ] Typed.t",
    "float": "[< Typed.T.sfloat ] Typed.t",
    "bool": "[< Typed.T.sbool ] Typed.t",
    "ptr": "full_ptr",
    "unknown": "rust_val",
    "fun_exec": "fun_exec",
    "meta_ty": "Types.ty",
}


output_type: dict[InterpTypeBase, str] = {
    "unit": "unit",
    "int": "Typed.T.sint Typed.t",
    "float": "Typed.T.sfloat Typed.t",
    "bool": "Typed.T.sbool Typed.t",
    "ptr": "full_ptr",
    "unknown": "rust_val",
    "fun_exec": "fun_exec",
    "meta_ty": "Types.ty",
}


def input_type_cast(arg: str, ty: InterpType) -> str:
    if ty[0] == "int":
        int_ty = cast(str, ty[1]).replace("I", "U")
        return f"let {arg} = as_base_i {int_ty} {arg} in "
    if ty[0] == "float":
        return f"let {arg} = as_base_f {ty[1]} {arg} in "
    if ty[0] == "ptr":
        return f"let {arg} = as_ptr {arg} in "
    if ty[0] == "bool":
        return f"let {arg} = Typed.BitVec.to_bool (as_base TBool {arg}) in "
    return ""


def output_type_cast(ty: InterpType) -> tuple[str, str]:
    if ty[0] == "int":
        return ("let+ ret = ", " in Int ret")
    if ty[0] == "float":
        return ("let+ ret = ", " in Float ret")
    if ty[0] == "bool":
        return "let+ ret = ", " in Int (Typed.BitVec.of_bool ret)"
    if ty[0] == "ptr":
        return "let+ ret = ", " in Ptr ret"
    if ty[0] == "unit":
        return "let+ () = ", " in Tuple []"
    return "", ""


# Parse the intrinsics via Charon, and return them
def get_intrinsics() -> dict[str, FunDecl]:
    file_json = (PWD / "intrinsics.ullbc.temp").resolve()
    if "--cached" in sys.argv:
        pprint("Using cached intrinsics")
    else:
        pprint("Loading intrinsics...")
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
    traverse_types(ullbc)
    intrinsics: dict[str, FunDecl] = {
        "::".join(i["Ident"][0] for i in fun["item_meta"]["name"][2:]): fun
        for fun in ullbc["translated"]["fun_decls"]
        if fun is not None
        and fun["item_meta"]["name"][0]["Ident"][0] == "core"
        and fun["item_meta"]["name"][1]["Ident"][0] == "intrinsics"
        # for now, we ignore intrinsics in submodules (mir, simd, fallbacks)
        and len(fun["item_meta"]["name"]) == 3
    }
    pprint(f"Found {BOLD}{len(intrinsics)}{RESET} intrinsics")
    return intrinsics


# Generate the OCaml interface for the intrinsics stubs, along with the stub file itself;
# returns [interfaces string, stubs string, main string]
def generate_interface(intrinsics: dict[str, FunDecl]) -> tuple[str, str, str]:
    class IntrinsicInfo(TypedDict):
        name: str
        path: str
        doc: str
        args: list[tuple[str, InterpType]]
        types: list[str]
        ret: InterpType

    pprint("Generating OCaml interface and stubs...")

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
        args: list[tuple[str, InterpType]] = [
            (sanitize_var_name(param["name"] or "arg"), type_of(param["ty"]))
            for param in fun["body"]["Unstructured"]["locals"]["locals"][
                1 : arg_count + 1
            ]
        ]
        arg_names = [arg for (arg, _) in args]
        types = [
            (
                param_l
                if (param_l := param["name"].lower()) not in arg_names
                else "t_" + param_l
            )
            for param in fun["signature"]["generics"]["types"]
        ]
        ret = type_of(fun["signature"]["output"])

        intrinsics_info.append(
            {
                "name": name,
                "path": path,
                "doc": doc,
                "args": args,
                "types": types,
                "ret": ret,
            }
        )
    intrinsics_info.sort(key=lambda x: x["name"])

    prelude = """
    (** This file was generated with [scripts/intrinsics.py] -- do not edit it manually,
        instead modify the script and re-run it. *)
    """

    type_utils = """
        type rust_val := State_monad.Sptr.t Rust_val.t
        type 'a ret := ('a, unit) State_monad.t
        type fun_exec :=
            UllbcAst.fun_decl ->
            rust_val list -> (rust_val, unit) State_monad.t
    """

    interface_str = f"""
        {prelude}

        open Charon

        module M (State_monad: State_monad.S) = struct
          module type Impl = sig
            {type_utils}
            type full_ptr := State_monad.Sptr.t Rust_val.full_ptr

    """

    stubs_str = f"""
        {prelude}


        [@@@warning "-unused-value-declaration"]

        open Rust_val

        module M (State_monad: State_monad.S): Intrinsics_intf.M(State_monad).Impl = struct
          open State_monad

          type rust_val = State_monad.Sptr.t Rust_val.t

          let[@inline] as_ptr (v : rust_val) =
            match v with
            | Ptr ptr -> ptr
            | Int v ->
                let v = Typed.cast_i Usize v in
                let ptr = Sptr.null_ptr_of v in
                (ptr, Thin)
            | _ -> failwith "expected pointer"

          let as_base ty (v : rust_val) = Rust_val.as_base ty v
          let as_base_i ty (v : rust_val) = Rust_val.as_base_i ty v
          let as_base_f ty (v : rust_val) = Rust_val.as_base_f ty v

    """

    main_str = f"""
        {prelude}

        open Rust_val

        module M (State_monad: State_monad.S): Intrinsics_intf.M(State_monad).S = struct
            open State_monad
            open Syntax

            type rust_val = Sptr.t Rust_val.t

            let[@inline] as_ptr (v : rust_val) =
            match v with
            | Ptr ptr -> ptr
            | Int v ->
                let v = Typed.cast_i Usize v in
                let ptr = Sptr.null_ptr_of v in
                (ptr, Thin)
            | _ -> failwith "expected pointer"

            let as_base ty (v : rust_val) = Rust_val.as_base ty v
            let as_base_i ty (v : rust_val) = Rust_val.as_base_i ty v
            let as_base_f ty (v : rust_val) = Rust_val.as_base_f ty v

    """

    for info in intrinsics_info:
        args_and_tys: Sequence[tuple[Optional[str], InterpType]] = []

        # special-case catch_unwind; it needs to also received the function-execution function
        if info["name"] == "catch_unwind":
            args_and_tys.append((None, ("fun_exec", None)))

        args_and_tys.extend((type, ("meta_ty", None)) for type in info["types"])
        args_and_tys.extend(info["args"])
        interface_str += f"""
            (** {info["doc"]} *)
            val {info["name"]} : {' -> '.join([
                f"{arg}:{input_type[ty[0]]}" if arg is not None else input_type[ty[0]]
                for (arg, ty) in args_and_tys
            ] + [output_type[info["ret"][0]] + " ret"])}
        """

        stubs_str += f"""
            let {info['name']} {' '.join(
                f"~{arg}:_" if arg else "_"
                for (arg, _) in args_and_tys)} =
                not_impl "Unsupported intrinsic: {info['name']}"
        """

    interface_str += f"""
        end

        module type S = sig
            include Impl
            {type_utils}
            val eval_fun : string -> fun_exec -> Types.generic_args -> rust_val list -> rust_val ret
        end
    end
    """

    stubs_str += """
        end
    """

    main_str += """
            include Intrinsics_impl.M (State_monad)

            let eval_fun name fun_exec (generics: Charon.Types.generic_args) args =
                match name, generics.types, args with
    """

    for info in intrinsics_info:
        types = "; ".join(info["types"])
        args_match = "; ".join(
            a if a != info["name"] else f"{a}_" for (a, _) in info["args"]
        )

        call_args = [
            f"~{arg}" if arg != info["name"] else f"~{arg}:{arg}_"
            for arg in info["types"] + [a for (a, _) in info["args"]]
        ]
        if info["name"] == "catch_unwind":
            call_args.insert(0, "fun_exec")

        main_str += f"""
            | "{info['name']}", [{types}], [{args_match}] ->
        """

        for arg, ty in info["args"]:
            main_str += input_type_cast(arg, ty)

        prefix, postfix = output_type_cast(info["ret"])
        main_str += f"""
            {prefix} {info['name']} {' '.join(call_args)} {postfix}
        """

    main_str += """
                | name, _, _ ->
                    Fmt.kstr not_impl "Intrinsic %s not found, or not called with the right arguments" name
        end
    """

    return interface_str, stubs_str, main_str


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
        )
    pprint(f"Wrote OCaml file {BOLD}{filename}{RESET}")


if __name__ == "__main__":
    intrinsics = get_intrinsics()
    interface, stubs, main = generate_interface(intrinsics)

    ml_folder = PWD / ".." / "lib" / "builtins"
    write_ocaml_file(ml_folder / "intrinsics_intf.ml", interface)
    write_ocaml_file(ml_folder / "intrinsics_stubs.ml", stubs)
    write_ocaml_file(ml_folder / "intrinsics.ml", main)

    # generate the "intrinsincs_impl.ml" file if it doesn't exist
    impl_file = ml_folder / "intrinsics_impl.ml"
    if not impl_file.exists():
        impl_content = """
        module M (State_monad: State_monad.S) = struct
        end
        """
        write_ocaml_file(impl_file, impl_content)
