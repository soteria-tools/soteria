#!/usr/bin/env python3


import json
import re
import subprocess
from typing import Any, Literal, TypedDict

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


class ConstParam(TypedDict):
    index: int
    name: str
    ty: "UniqueType"


class GenericParams(TypedDict):
    types: list[TypeParam]
    const_generics: list[ConstParam]


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


class TypeArray(TypedDict):
    Array: tuple[UniqueType, UniqueType]


Type = TypeAdt | TypeLiteral | TypeTypeVar | TypeRef | TypePtr | TypeNever | TypeArray


class DedupedType(TypedDict):
    Deduplicated: int


class HashConsedType(TypedDict):
    HashConsedValue: tuple[int, Type]


UniqueType = DedupedType | HashConsedType


class Signature(TypedDict):
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


class BodyUnstructuredBody(TypedDict):
    Unstructured: UnstructuredBody


class IntrinsicBody(TypedDict):
    name: str
    arg_names: list[str]


class BodyIntrinsicBody(TypedDict):
    Intrinsic: IntrinsicBody


Body = BodyUnstructuredBody | BodyIntrinsicBody


class FunDecl(TypedDict):
    item_meta: ItemMeta
    signature: Signature
    generics: GenericParams
    body: Body


InterpTypeBase = Literal[
    "unit",
    "int",
    "bool",
    "float",
    "ptr",
    "unknown",
    "fun_exec",
    "meta_ty",
    "meta_const",
]
InterpTypeMeta = Optional[str]
InterpType = tuple[InterpTypeBase, InterpTypeMeta]

type_cache: dict[int, Type] = {}

USER_IMPL_START = "(* BEGIN USER IMPLEMENTATION *)"
USER_IMPL_END = "(* END USER IMPLEMENTATION *)"

GENERATED_WARNING = "(** This file was generated with [scripts/stubs.py] -- do not edit it manually, instead modify the script and re-run it. *)"

OCAML_HELPERS = """\
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
let as_base_f ty (v : rust_val) = Rust_val.as_base_f ty v"""


def type_of(unique_ty: UniqueType) -> InterpType:
    ty: Type
    type_id: int = -1
    if "Deduplicated" in unique_ty:
        type_id = unique_ty["Deduplicated"]
        if type_id not in type_cache:
            raise ValueError(f"Unknown deduplicated type id: {type_id}")
        ty = type_cache[type_id]
        # print(f"Read {type_id} -> {ty}")
    elif "HashConsedValue" in unique_ty:
        type_id, inner_ty = unique_ty["HashConsedValue"]
        type_cache[type_id] = inner_ty
        # print("Cache <-", type_id, inner_ty)
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

    if "RawPtr" in ty or "Ref" in ty or "FnPtr" in ty:
        return "ptr", None

    if "Adt" in ty:
        if ty["Adt"]["id"] == "Tuple" and len(ty["Adt"]["generics"]["types"]) == 0:
            return "unit", None

    ignored = ["TypeVar", "Adt", "TraitType", "Array"]
    if any(kind in ty for kind in ignored):
        return "unknown", None

    raise RuntimeError(f"Unhandled type {type_id}: {ty}")
    return "unknown", None


# try traversing the whole JSON to cache all types
def traverse_types(x: Any, prev_key: Optional[str] = None) -> None:
    if isinstance(x, list):
        for v in x:
            traverse_types(v, prev_key)
        return

    if not isinstance(x, dict):
        return

    invalid_prev_keys = [
        "tref",
        "trait_ref",
        "trait_refs",
        "parent_trait_refs",
        "implied_trait_refs",
        "Trait",
        "ParentClause",
        "TraitType",
        "TraitConst",
    ]
    if "HashConsedValue" in x and prev_key not in invalid_prev_keys:
        type_id, inner_ty = x["HashConsedValue"]
        type_cache[type_id] = inner_ty
        # print(f"Cache ({prev_key}) <-", type_id, inner_ty)

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
    "meta_const": "Types.constant_expr",
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
    "meta_const": "Types.constant_expr",
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


def sanitize_var_name(name: str) -> str:
    ocaml_names = ["val"]
    if name in ocaml_names:
        return f"{name}_"
    return name


def sanitize_ocaml_ident(name: str) -> str:
    sanitized = re.sub(r"[^a-zA-Z0-9_]", "_", name)
    if not sanitized:
        return "f"
    if sanitized[0].isdigit():
        sanitized = "f_" + sanitized
    return sanitize_var_name(sanitized)


def sanitize_variant_name(name: str) -> str:
    words = [w for w in re.split(r"[^a-zA-Z0-9]+", name) if w]
    if not words:
        return "Stub"
    variant = "".join(w[:1].upper() + w[1:] for w in words)
    if variant[0].isdigit():
        variant = "Stub" + variant
    return variant


def read_user_impl_block(filename: Path) -> Optional[str]:
    if not filename.exists():
        return None
    content = filename.read_text()
    start = content.find(USER_IMPL_START)
    end = content.find(USER_IMPL_END)
    if start == -1 or end == -1 or end <= start:
        return None
    return content[start : end + len(USER_IMPL_END)]


def has_user_lets(impl_block: str) -> bool:
    return re.search(r"\blet\s+[a-zA-Z_][a-zA-Z0-9_']*", impl_block) is not None


def arg_value(prefix: str) -> Optional[str]:
    for arg in sys.argv:
        if arg.startswith(prefix):
            return arg[len(prefix) :]
    return None


def sanitize_comment(comment: str) -> str:
    return "{@markdown[\n" + comment.replace("(*", "( *").strip() + "\n]}"


def extract_doc(fun: FunDecl) -> Optional[str]:
    raw = "\n".join(
        attrib["DocComment"]
        for attrib in fun["item_meta"]["attr_info"]["attributes"]
        if "DocComment" in attrib
    )
    if not raw:
        return None
    return sanitize_comment(raw)


# ---------------------------------------------------------------------------
# Shared item info types and code-generation helpers
# ---------------------------------------------------------------------------


class ItemInfo(TypedDict):
    """Base information shared between intrinsics and custom stubs."""

    ocaml_name: str
    rust_path: str
    doc: Optional[str]
    args: list[tuple[str, InterpType]]
    types: list[str]
    consts: list[str]
    ret: InterpType


class StubInfo(ItemInfo):
    variant_name: str
    is_dummy: bool


def make_args_and_tys(
    info: ItemInfo,
    extra_prefix: Optional[list[tuple[Optional[str], InterpType]]] = None,
) -> list[tuple[Optional[str], InterpType]]:
    """Build the full argument list: [extra_prefix] + [type generics] + [const generics] + [args]."""
    result: list[tuple[Optional[str], InterpType]] = list(extra_prefix or [])
    result.extend((t, ("meta_ty", None)) for t in info["types"])
    result.extend((c, ("meta_const", None)) for c in info["consts"])
    result.extend(info["args"])
    return result


def make_ocaml_signature(
    args_and_tys: list[tuple[Optional[str], InterpType]],
    ret: InterpType,
) -> str:
    """Generate an OCaml type signature string: `arg:type -> ... -> ret_type ret`.
    When there are no arguments, emits `unit -> ret_type ret` so it compiles as
    a function rather than a value."""
    ret_part = output_type[ret[0]] + " ret"
    if not args_and_tys:
        return f"unit -> {ret_part}"
    parts = [
        f"{arg}:{input_type[ty[0]]}" if arg is not None else input_type[ty[0]]
        for arg, ty in args_and_tys
    ]
    parts.append(ret_part)
    return " -> ".join(parts)


def make_not_impl_stub(
    name: str,
    args_and_tys: list[tuple[Optional[str], InterpType]],
    error_msg: str,
) -> str:
    """Generate a `let name ~arg:_ ... = not_impl "error_msg"` OCaml expression.
    When there are no arguments, emits `let name () = ...` to enforce function
    syntax."""
    if not args_and_tys:
        return f'let {name} () = not_impl "{error_msg}"'
    stub_args = " ".join(
        f"~{arg}:_" if arg is not None else "_" for arg, _ in args_and_tys
    )
    return f'let {name} {stub_args} = not_impl "{error_msg}"'


def make_match_branch(
    info: ItemInfo, *, fist_pat: str, extra_arg: Optional[str] = None
) -> str:
    types_pat = "; ".join(info["types"])
    consts_pat = "; ".join(info["consts"])
    args_match = "; ".join(
        a if a != info["ocaml_name"] else f"{a}_" for (a, _) in info["args"]
    )
    call_args = [
        f"~{arg}" if arg != info["ocaml_name"] else f"~{arg}:{arg}_"
        for arg in info["types"] + info["consts"] + [a for (a, _) in info["args"]]
    ]

    if extra_arg:
        call_args.insert(0, extra_arg)

    if not call_args:
        call_args.append("()")

    call_expr = f"{info['ocaml_name']} {' '.join(call_args)}"
    return f"| {fist_pat}, [{types_pat}], [{consts_pat}], [{args_match}] -> {make_match_body(info, call_expr)}"


def make_match_body(info: ItemInfo, call_expr: str) -> str:
    """Generate input casts followed by a function call wrapped with an output cast."""
    arg_casts = ""
    for arg, ty in info["args"]:
        arg_casts += input_type_cast(arg, ty)
    prefix, postfix = output_type_cast(info["ret"])
    return f"{arg_casts} {prefix} {call_expr} {postfix}"


def make_doc_comment(doc: Optional[str]) -> str:
    """Generate an OCaml odoc comment line, or empty string if no doc."""
    return f"(** {doc} *) " if doc else ""


def make_val_entry(
    info: ItemInfo, args_and_tys: list[tuple[Optional[str], InterpType]]
) -> str:
    """Generate a `val name : sig` declaration with an optional leading doc comment."""
    sig = make_ocaml_signature(args_and_tys, info["ret"])
    return f"{make_doc_comment(info['doc'])}val {info['ocaml_name']} : {sig}"


def fun_decl_to_item_info(fun: FunDecl, ocaml_name: str) -> ItemInfo:
    """Extract an ItemInfo from a FunDecl, handling both Intrinsic and Unstructured
    bodies for parameter-name extraction. Deduplicates clashing argument names."""
    rust_path = "::".join(
        elem["Ident"][0] if "Ident" in elem else "_"
        for elem in fun["item_meta"]["name"]
    )
    doc = extract_doc(fun)

    arg_count = len(fun["signature"]["inputs"])
    body = fun.get("body")
    if isinstance(body, dict) and "Intrinsic" in body:
        raw_names = body["Intrinsic"]["arg_names"]
        param_names: list[Optional[str]] = [n or None for n in raw_names]
    elif isinstance(body, dict) and "Unstructured" in body:
        raw_params = body["Unstructured"]["locals"]["locals"][1 : arg_count + 1]
        param_names = [cast(Optional[str], param["name"]) for param in raw_params]
    else:
        param_names = [None] * arg_count

    used_names: dict[str, int] = {}
    args: list[tuple[str, InterpType]] = []
    for i, ty in enumerate(fun["signature"]["inputs"]):
        base_name = param_names[i] or "arg"
        if base_name.startswith("_"):
            base_name = base_name[1:]
        base_name = sanitize_ocaml_ident(base_name)
        nb = used_names.get(base_name, 0)
        used_names[base_name] = nb + 1
        arg_name = base_name if nb == 0 else f"{base_name}_{nb + 1}"
        args.append((arg_name, type_of(ty)))

    def mk_arg(name: str, prefix: str) -> str:
        sanitized = sanitize_ocaml_ident(name.lower())
        if any(sanitized == arg for (arg, _) in args):
            return f"{prefix}_{sanitized}"
        return sanitized

    types = [mk_arg(param["name"], "t") for param in fun["generics"]["types"]]
    consts = [mk_arg(param["name"], "c") for param in fun["generics"]["const_generics"]]
    ret = type_of(fun["signature"]["output"])

    return {
        "ocaml_name": ocaml_name,
        "rust_path": rust_path,
        "doc": doc,
        "args": args,
        "types": types,
        "consts": consts,
        "ret": ret,
    }


# ---------------------------------------------------------------------------
# Charon invocations
# ---------------------------------------------------------------------------


# Parse the intrinsics via Charon, and return them
def get_intrinsics() -> dict[str, FunDecl]:
    file_json = (PWD / "intrinsics.ullbc.temp").resolve()
    if "--cached" in sys.argv:
        pprint("Using cached intrinsics")
    else:
        pprint("Loading intrinsics...")
        file_rs = (PWD / "intrinsics.rs").resolve()
        file_rs.touch(exist_ok=True)

        toolchain = get_toolchain()
        sysroot = get_sysroot(toolchain)

        charon_cmd = f"charon rustc --ullbc \
            --dest-file {file_json} \
            --start-from core::intrinsics \
            --include core::intrinsics \
            --exclude core::intrinsics::const_allocate \
            --exclude core \
            -- {file_rs} --cfg miri --crate-type=lib --sysroot={sysroot}"

        proc = subprocess.run(charon_cmd, shell=True, stderr=subprocess.STDOUT)
        if proc.returncode != 0:
            raise RuntimeError(
                f"{YELLOW}{BOLD}Warning{RESET}: Charon returned exit code {proc.returncode}"
            )

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


def get_stubs(patterns: list[str]) -> list[FunDecl]:
    file_json = (PWD / "custom_stubs.ullbc.temp").resolve()
    if "--cached" in sys.argv:
        pprint("Using cached stubs")
    else:
        file_rs = (PWD / "custom_stubs.rs").resolve()
        features = ["f16", "f128", "const_index"]
        file_rs.write_text(f"#![feature({', '.join(features)})]\n")
        toolchain = get_toolchain()
        sysroot = get_sysroot(toolchain)
        charon_cmd = [
            "charon",
            "rustc",
            "--ullbc",
            "--dest-file",
            str(file_json),
            "--extract-opaque-bodies",
        ]
        for pattern in patterns:
            # substs we must apply because of inherent impls
            substs = [
                (r".+::f16::_::(.+)", r"f16::\1"),
                (r".+::f32::_::(.+)", r"f32::\1"),
                (r".+::f64::_::(.+)", r"f64::\1"),
                (r".+::f128::_::(.+)", r"f128::\1"),
            ]
            for pat, replacement in substs:
                if re.match(pat, pattern):
                    pattern = re.sub(pat, replacement, pattern)
            charon_cmd.extend(["--start-from-if-exists", pattern, "--include", pattern])

        charon_cmd.extend(
            [
                "--opaque",
                "std",
                "--opaque",
                "core",
                "--",
                str(file_rs),
                "--crate-type=lib",
                "--sysroot",
                sysroot,
            ]
        )

        proc = subprocess.run(charon_cmd, check=False, stderr=subprocess.STDOUT)
        file_rs.unlink(missing_ok=True)
        if proc.returncode != 0:
            raise RuntimeError(
                f"{YELLOW}{BOLD}Warning{RESET}: Charon returned exit code {proc.returncode}"
            )

    ullbc = json.loads(file_json.read_text())
    traverse_types(ullbc)
    return [fun for fun in ullbc["translated"]["fun_decls"] if fun is not None]


# ---------------------------------------------------------------------------
# Code generators
# ---------------------------------------------------------------------------


# Generate the OCaml interface for the intrinsics stubs, along with the stub
# file itself; returns [interfaces string, stubs string, main string]
def generate_interface(intrinsics: dict[str, FunDecl]) -> tuple[str, str, str]:
    pprint("Generating OCaml interface and stubs...")

    intrinsics_info: list[ItemInfo] = []
    for fun in intrinsics.values():
        if "Intrinsic" not in fun["body"]:
            continue
        ocaml_name = fun["item_meta"]["name"][-1]["Ident"][0]
        intrinsics_info.append(fun_decl_to_item_info(fun, ocaml_name))
    intrinsics_info.sort(key=lambda x: x["ocaml_name"])

    type_utils = """
        type rust_val := StateM.Sptr.t Rust_val.t
        type 'a ret := ('a, unit) StateM.t
        type fun_exec :=
            Fun_kind.t -> rust_val list -> (rust_val, unit) StateM.t
    """

    interface_entries = ""
    stubs_entries = ""
    match_arm_entries = ""

    for info in intrinsics_info:
        # catch_unwind additionally receives the function-execution callback
        extra_prefix: Optional[list[tuple[Optional[str], InterpType]]] = None
        if info["ocaml_name"] == "catch_unwind":
            extra_prefix = [(None, ("fun_exec", None))]
        args_and_tys = make_args_and_tys(info, extra_prefix)

        # Interface val entry
        interface_entries += f"\n{make_val_entry(info, args_and_tys)}\n"

        # Stub entry
        stubs_entries += make_not_impl_stub(
            info["ocaml_name"],
            args_and_tys,
            f"Unsupported intrinsic: {info['ocaml_name']}",
        )

        # Match arms
        extra_arg = "fun_exec" if info["ocaml_name"] == "catch_unwind" else None
        match_arm_entries += make_match_branch(
            info, fist_pat=f'"{info['ocaml_name']}"', extra_arg=extra_arg
        )

    interface_str = f"""
        {GENERATED_WARNING}

        open Charon
        open Common

        module M (StateM : State.StateM.S) = struct
          module type Impl = sig
            {type_utils}
            type full_ptr := StateM.Sptr.t Rust_val.full_ptr

        {interface_entries}
        end

        module type S = sig
            include Impl
            {type_utils}
            val eval_fun : string -> fun_exec -> Types.generic_args -> rust_val list -> rust_val ret
        end
    end
    """

    stubs_str = f"""
        {GENERATED_WARNING}

        [@@@warning "-unused-value-declaration"]

        module M (StateM : State.StateM.S): Intf.M(StateM).Impl = struct
          open StateM
          {stubs_entries}
        end
    """

    main_str = f"""
        {GENERATED_WARNING}

        open Rust_val
        open Common

        module M (StateM : State.StateM.S): Intf.M(StateM).S = struct
            open StateM
            open Syntax

            type rust_val = Sptr.t Rust_val.t

            {OCAML_HELPERS}

            include Impl.M (StateM)

            let[@inline] eval_fun name fun_exec (generics: Charon.Types.generic_args) args =
                match name, generics.types, generics.const_generics, args with
                {match_arm_entries}
                | name, tys, cs, args ->
                    Fmt.kstr not_impl
                        "Intrinsic %s not found, or not called with the right arguments; got:@.Types: %a@.Consts: %a@.Args: %a"
                        name
                        Fmt.(list ~sep:comma Charon_util.pp_ty) tys
                        Fmt.(list ~sep:comma Crate.pp_constant_expr) cs
                        Fmt.(list ~sep:comma pp_rust_val) args
        end
    """

    return interface_str, stubs_str, main_str


def path_of(fun: FunDecl) -> str:
    names: list[str] = []
    for elem in fun["item_meta"]["name"]:
        if "Ident" in elem:
            names.append(elem["Ident"][0])
        else:
            names.append("_")
    return "::".join(names)


def generate_custom_stubs() -> None:
    config_path = (PWD / ".." / "lib" / "builtins" / "stubs.json").resolve()
    if not config_path.exists():
        raise FileNotFoundError(
            "Could not find stubs.json, expected at: " + str(config_path)
        )

    raw_config = json.loads(config_path.read_text())
    if not isinstance(raw_config, dict):
        raise ValueError("stubs.json must contain a JSON object")

    config: dict[str, list[str]] = {}
    for category, patterns in raw_config.items():
        if not isinstance(category, str):
            raise ValueError("stubs.json keys must be strings")
        if not isinstance(patterns, list) or not all(
            isinstance(p, str) for p in patterns
        ):
            raise ValueError(f"Category '{category}' must map to a list of strings")
        config[category] = patterns

    patterns: list[str] = list(set(p for ps in config.values() for p in ps))

    if len(patterns) == 0:
        pprint("No custom stub patterns found, skipping")
        return

    pprint("Loading custom stubs declarations...")
    fun_decls = get_stubs(patterns)
    categories: dict[str, list[FunDecl]] = {k: [] for k in config.keys()}

    def matches_pattern(path: list[PathElem], pattern: str) -> bool:
        pattern_parts = pattern.split("::")
        if not (len(path) == len(pattern_parts)):
            return False

        for elem, pat in zip(path, pattern_parts):
            if pat == "_":
                continue
            if "Ident" not in elem or elem["Ident"][0] != pat:
                return False
        return True

    missing_patterns = [pat for patterns in config.values() for pat in patterns]
    for fun in fun_decls:
        name = fun["item_meta"]["name"]
        for category, patterns in config.items():
            for pattern in patterns:
                if matches_pattern(name, pattern):
                    missing_patterns.remove(pattern)
                    categories[category].append(fun)
                    break
            else:
                continue
            break

    if missing_patterns:
        pprint(
            f"{YELLOW}{BOLD}Warning{RESET}: {len(missing_patterns)} stubs unresolved; they won't have their signature parsed"
        )
        for p in set(missing_patterns):
            last_part = p.split("::")[-1]
            potential_matches = [
                path for fdef in fun_decls if last_part in (path := path_of(fdef))
            ]
            if potential_matches:
                pprint(f"- {p} (maybe you meant: {', '.join(potential_matches)})")
            else:
                pprint(f"- {p}")

    missing_by_category: dict[str, list[str]] = {
        category: [p for p in patterns if p in missing_patterns]
        for category, patterns in config.items()
    }

    ml_folder = (PWD / ".." / "lib" / "builtins").resolve()

    for category, functions in categories.items():
        unresolved = missing_by_category.get(category, [])
        if len(functions) == 0 and len(unresolved) == 0:
            pprint(f"Category {BOLD}{category}{RESET}: no matching declarations")
            continue

        variant_occ: dict[str, int] = {}
        infos: list[StubInfo] = []

        short_names: list[str] = [
            fun["item_meta"]["name"][-1]["Ident"][0] for fun in functions
        ] + [
            pattern.split("::")[-1] if "::" in pattern else pattern
            for pattern in unresolved
        ]

        def unique_name(name: list[PathElem]) -> str:
            rust_name = name[-1]["Ident"][0]
            ocaml_name = sanitize_ocaml_ident(rust_name)
            if short_names.count(ocaml_name) > 1:
                name_parts = [item["Ident"][0] for item in name if "Ident" in item]
                # last two parts
                return sanitize_ocaml_ident("_".join(name_parts[-2:]))
            return ocaml_name

        for fun in functions:
            ocaml_name = unique_name(fun["item_meta"]["name"])
            info = fun_decl_to_item_info(fun, ocaml_name)

            base_variant = sanitize_variant_name(info["rust_path"])
            vcount = variant_occ.get(base_variant, 0)
            variant_occ[base_variant] = vcount + 1
            variant_name = (
                base_variant if vcount == 0 else f"{base_variant}{vcount + 1}"
            )

            infos.append({**info, "variant_name": variant_name, "is_dummy": False})

        for pattern in unresolved:
            # Strip anonymous impl-block '_' components so unique_name can use a
            # meaningful prefix (e.g. "f16" from "core::f16::_::is_normal").
            path_parts = [p for p in pattern.split("::") if p != "_"]
            rust_name: list[PathElem] = [{"Ident": (p,)} for p in path_parts]
            ocaml_name = unique_name(rust_name)
            base_variant = sanitize_variant_name(pattern)
            vcount = variant_occ.get(base_variant, 0)
            variant_occ[base_variant] = vcount + 1
            variant_name = (
                base_variant if vcount == 0 else f"{base_variant}{vcount + 1}"
            )

            infos.append(
                {
                    "ocaml_name": ocaml_name,
                    "variant_name": variant_name,
                    "rust_path": pattern,
                    "doc": None,
                    "is_dummy": True,
                    "args": [],
                    "types": [],
                    "consts": [],
                    "ret": ("unknown", None),
                }
            )

        infos.sort(key=lambda x: x["rust_path"])

        category_name = sanitize_ocaml_ident(category.lower())
        category_dir = ml_folder / category_name
        category_dir.mkdir(exist_ok=True)

        intf_file = category_dir / "intf.ml"
        impl_file = category_dir / "impl.ml"
        entry_file = category_dir / f"{category_name}.ml"

        intf_entries = ""
        default_impl_entries = ""
        fn_variants = ""
        fn_map_entries = ""
        eval_entries = ""

        for info in infos:
            fn_variants += f" | {info['variant_name']}"
            fn_map_entries += f"(\"{info['rust_path']}\", {info['variant_name']});"

            if info["is_dummy"]:
                dummy_args: list[tuple[str | None, InterpType]] = [
                    ("fun_exec", ("fun_exec", None)),
                    ("types", ("meta_ty", None)),
                    ("consts", ("meta_const", None)),
                    ("args", ("unknown", None)),
                ]
                dummy_sig = "fun_exec:fun_exec -> types:Types.ty list -> consts:Types.constant_expr list -> args:rust_val list -> rust_val ret"
                intf_entries += f"\n{make_doc_comment(info['doc'])}val {info['ocaml_name']} : {dummy_sig}\n"
                default_impl_entries += make_not_impl_stub(
                    info["ocaml_name"],
                    dummy_args,
                    f"Unsupported custom stub (dummy): {info['rust_path']}",
                )
                eval_entries += f"""
                    | {info['variant_name']}, _, _, _ ->
                      {info['ocaml_name']} ~fun_exec:_fun_exec ~types:generics.types ~consts:generics.const_generics ~args
                """
            else:
                args_and_tys = make_args_and_tys(info)
                intf_entries += f"\n{make_val_entry(info, args_and_tys)}\n"

                default_impl_entries += make_not_impl_stub(
                    info["ocaml_name"],
                    args_and_tys,
                    f"Unsupported custom stub: {info['rust_path']}",
                )

                eval_entries += make_match_branch(info, fist_pat=info["variant_name"])

        # intf.ml — generated module type that impl.ml must satisfy
        intf_generated = f"""
            {GENERATED_WARNING}

            open Charon
            open Common

            module M (StateM : State.StateM.S) = struct
              open StateM

              type rust_val = Sptr.t Rust_val.t
              type 'a ret = ('a, unit) StateM.t
              type fun_exec = Fun_kind.t -> rust_val list -> (rust_val, unit) StateM.t
              type full_ptr = StateM.Sptr.t Rust_val.full_ptr

              module type S = sig {intf_entries} end
            end
        """
        write_ocaml_file(intf_file, intf_generated)

        # impl.ml — hand-written; only regenerate when it has no user code yet
        if not impl_file.exists():
            pprint(f"Created dummy impl file {BOLD}{impl_file}{RESET}")
            impl_generated = f"""
                module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
                    open StateM
                    open Syntax

                    {default_impl_entries}
                end
            """
            write_ocaml_file(impl_file, impl_generated)

        # {category_name}.ml — generated entry point: fn_map + dispatch + eval_fun
        entry_generated = f"""
            {GENERATED_WARNING}

            [@@@warning "-unused-open"]

            open Common
            open Rust_val

            type fn = {fn_variants}
            let fn_pats : (string * fn) list = [ {fn_map_entries} ]

            module M (StateM : State.StateM.S) = struct
                open StateM
                open Syntax

                type rust_val = Sptr.t Rust_val.t
                type 'a ret = ('a, unit) StateM.t
                type fun_exec = Fun_kind.t -> rust_val list -> (rust_val, unit) StateM.t
                type full_ptr = StateM.Sptr.t Rust_val.full_ptr

                {OCAML_HELPERS}

                include Impl.M(StateM)

                let[@inline] fn_to_stub stub _fun_exec (generics : Charon.Types.generic_args) args =
                    match[@warning "-redundant-case"] (stub, generics.types, generics.const_generics, args) with
                    {eval_entries}
                    | _, tys, cs, args ->
                        Fmt.kstr not_impl
                            "Custom stub found but called with the wrong arguments; got:@.Types: %a@.Consts: %a@.Args: %a"
                            Fmt.(list ~sep:comma Charon_util.pp_ty) tys
                            Fmt.(list ~sep:comma Crate.pp_constant_expr) cs
                            Fmt.(list ~sep:comma pp_rust_val)args
            end
        """
        write_ocaml_file(entry_file, entry_generated)
        pprint(
            f"Category {BOLD}{category}{RESET}: generated {BOLD}{len(infos)}{RESET} stubs"
        )


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


def generate_intrinsics():
    intrinsics = get_intrinsics()
    interface, stubs, main = generate_interface(intrinsics)

    ml_folder = PWD / ".." / "lib" / "builtins" / "intrinsics"
    if not ml_folder.exists():
        ml_folder.mkdir()
    write_ocaml_file(ml_folder / "intf.ml", interface)
    write_ocaml_file(ml_folder / "stubs.ml", stubs)
    write_ocaml_file(ml_folder / "intrinsics.ml", main)


if __name__ == "__main__":
    run_intrinsics = "--stubs" not in sys.argv or "--intrinsics" in sys.argv
    run_custom_stubs = "--stubs" in sys.argv

    if run_intrinsics:
        generate_intrinsics()
    if run_custom_stubs:
        generate_custom_stubs()
