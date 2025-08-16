import sys
import os
import subprocess
import re
from pathlib import Path
from typing import Optional, cast

PURPLE = "\033[0;35m"
RED = "\033[0;31m"
ORANGE = "\033[38;5;208m"
YELLOW = "\033[38;5;220m"
GREEN = "\033[0;32m"
CYAN = "\033[0;36m"
BLUE = "\033[0;34m"
MAGENTA = "\033[0;95m"
GRAY = "\033[0;90m"
BOLD = "\033[1m"
RESET = "\033[0m"

# if piping output, remove colors:
NO_COLOR = not sys.stdout.isatty()
if NO_COLOR:
    PURPLE = RED = ORANGE = YELLOW = GREEN = CYAN = BLUE = GRAY = BOLD = RESET = ""


rainbow_ = 0


def inc_rainbow():
    global rainbow_
    rainbow_ += 1


def rainbow():
    if NO_COLOR:
        return ""
    return [
        "\033[38;5;197m",
        "\033[38;5;208m",
        "\033[38;5;220m",
        "\033[38;5;70m",
        "\033[38;5;74m",
        "\033[38;5;33m",
        "\033[38;5;127m",
    ][rainbow_ % 7]


def pprint(*args, inc: bool = False, end="\n", **kwargs):
    if NO_COLOR:
        print("| ", end="")
    else:
        clr = rainbow()
        print(f"{clr}|{RESET} ", end="")
    print(*args, **kwargs, end=end + RESET)
    if inc:
        inc_rainbow()


# displays a list of rows -- each cell is text and an optional color
def pptable(rows: list[list[tuple[str, Optional[str]]]]):
    cols = len(rows[0])
    col_len = [max(len(row[i][0]) for row in rows) for i in range(cols)]
    pad = "  "
    for row in rows:
        pprint(
            pad.join(
                (clr or "") + cell + RESET + " " * (col_len[i] - len(cell))
                for i, (cell, clr) in enumerate(row)
            ),
            inc=True,
        )


pass_ = lambda x: ("Success", GREEN, x)
fail_ = lambda x: ("Failure", RED, x)
unkn_ = lambda x: ("Unknown", YELLOW, x)

SKIPPED_TESTS: dict[str, tuple[str, str, str]] = {
    # Kani
    "ArithOperators/rem_float_fixme.rs": fail_("Complicated float expression"),
    "ConstEval/limit.rs": unkn_("Slow because of an array of size 131072"),
    "FloatingPoint/main.rs": pass_("Slow floating operation operations"),
    "BitwiseShiftOperators/shift_neg_vals.rs": unkn_(
        "Wrapping operations without loop unrolling branch too much"
    ),
    "Intrinsics/bswap.rs": unkn_("Hard to reduce binary operations"),
    "Intrinsics/Count/ctpop.rs": pass_("The test requires 2^N branches"),
    "Intrinsics/FastMath/add_f64.rs": pass_("Slow floating point operation"),
    "Intrinsics/FastMath/div_f64.rs": pass_("Slow floating point operation"),
    "Intrinsics/FastMath/mul_f64.rs": pass_("Slow floating point operation"),
    "Intrinsics/FastMath/sub_f64.rs": pass_("Slow floating point operation"),
    "Intrinsics/Math/Rounding/Ceil/ceilf32.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Ceil/ceilf64.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Floor/floorf32.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Floor/floorf64.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Ceil/floorf32.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Ceil/floorf64.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/RInt/rintf32.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/RInt/rintf64.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Round/roundf32.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Round/roundf64.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/RoundTiesEven/round_ties_even_f32.rs": pass_(
        "Slow floating point rounding"
    ),
    "Intrinsics/Math/Rounding/RoundTiesEven/round_ties_even_f64.rs": pass_(
        "Slow floating point rounding"
    ),
    "Intrinsics/Math/Rounding/Trunc/truncf32.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Trunc/truncf64.rs": pass_("Slow floating point rounding"),
    # Miri
    "fail/layout_cycle.rs": fail_("We don't check for cycles in layout"),
    "pass/issues/issue-17877.rs": unkn_("Makes an array of size 16384, too slow"),
    "pass/issues/issue-20575.rs": unkn_("Very slow compilation"),
    "pass/issues/issue-29746.rs": unkn_("Very slow compilation"),
    "pass/arrays.rs": unkn_("Makes an array [(), usize::MAX], which we try evaluating"),
    "pass/tag-align-dyn-u64.rs": unkn_("Slow due to symbolic checks on the pointer"),
}

KNOWN_ISSUES = {
    # Kani
    "Arbitrary/arbitrary_structs.rs": "any_array isn't correctly implemented, due to mono issues",
    "ArithOperators/unsafe_add_fail.rs": "The main function takes a parameter? Kani crashes too",
    "ArithOperators/unsafe_mul_fail.rs": "The main function takes a parameter? Kani crashes too",
    "ArithOperators/unsafe_sub_fail.rs": "The main function takes a parameter? Kani crashes too",
    "Cleanup/unwind_fixme.rs": "The main function takes a paramter? Kani crashes too",
    "Enum/niche_many_variants.rs": "We don't handle enum niches yet",
    "FunctionCall/Variadic/fixme_main.rs": "We don't handle functions with spread arguments (not in Charon)",
    "FunctionCall/Variadic/main.rs": "We don't handle functions with spread arguments (not in Charon)",
    "Intrinsics/Compiler/variant_count.rs": "Kani doesn't handle variant_count yet -- we do!",
    "Intrinsics/ConstEval/pref_align_of.rs": "Requires support for custom target architectures",
    "Intrinsics/CopySign/copysignf32.rs": "SMT-lib limitations around NaN mean we can't model this",
    "Intrinsics/CopySign/copysignf64.rs": "SMT-lib limitations around NaN mean we can't model this",
    "LayoutRandomization/should_fail.rs": "We don't handle layout randomization yet",
    "Static/anon_static.rs": "We don't handle pointers derived from globals properly, freeing referenced values",
    "Str/raw_ptr.rs": "We don't handle #[safety_constraint(...)] yet",
    "Uninit/access-padding-enum-diverging-variants.rs": "Kani can't handle variants with different paddings",
    "Unwind-Attribute/fixme_lib.rs": "We don't have a flag to not check for unwinding",
    "ValidValues/write_bytes.rs": "Kani checks for validity on write, whereas Miri does on read; we copy Miri.",
    # Miri
    "fail/dangling_pointers/dangling_pointer_project_underscore_let.rs": "let _ = ... assignments get optimized out",
    "fail/dangling_pointers/dangling_pointer_project_underscore_let_type_annotation.rs": "let _ = ... assignments get optimized out",
    "fail/dangling_pointers/dangling_pointer_project_underscore_match.rs": "let _ = ... assignments get optimized out",
    "fail/dangling_pointers/deref_dangling_box.rs": "We don't check for dangling pointers for boxes",
    "fail/intrinsics/typed-swap-invalid-scalar.rs": "Uses weird CFGs, technically we pass it",
    "fail/erroneous_const.rs": "We lazily load constants, so the panic never triggers",
    "fail/function_calls/arg_inplace_mutate.rs": "We don't check that arguments aren't mutated in place",
    "fail/function_calls/return_pointer_aliasing_read.rs": "We don't check arguments don't alias with the return place",
    "fail/function_calls/return_pointer_aliasing_write.rs": "We don't check arguments don't alias with the return place",
    "fail/overlapping_assignment.rs": "MIR-only check for assignment overlap (we don't do this atm)",
    "fail/provenance/strict_provenance_cast.rs": "Miri has a strict provenance flag, we don't",
    "fail/unaligned_pointers/field_requires_parent_struct_alignment2.rs": "We don't handle repr(packed/align)",
    "fail/unaligned_pointers/reference_to_packed.rs": "We don't handle repr(packed/align)",
    "fail/uninit/uninit_alloc_diagnostic.rs": "We don't detected an uninit access that.. doesn't seem to exist?",
    "fail/validity/nonzero.rs": "The valid_range_start attribute isn't parsed by Charon?",
    "fail/validity/ref_to_uninhabited1.rs": "We don't check Boxes have an inhabited value",
    "fail/validity/uninit_float.rs": "A uninit mitigation doesn't get compiled away despite flags set?",
    "pass/align.rs": "We don't allow ptr-int-ptr conversions, Miri does (under a flag)",
    "pass/const-vec-of-fns.rs": "We don't handle pointers derived from globals properly, freeing referenced values",
    "pass/drop_type_without_drop_glue.rs": "How is this not a null deref error?",
    "pass/integer-ops.rs": "Miri allows negative bit shifts, we don't (like Kani)",
    "pass/disable-alignment-check.rs": "We don't provide a way to disable alignment checks",
    "pass/enum_discriminant_ptr_value.rs": "We don't handle the niche for Option<&T>",
    "pass/extern_types.rs": "We don't handle extern types",
    "pass/function_calls/abi_compat.rs": "We are too restrictive on fn pointer casts",
    "pass/issues/issue-120337-irrefutable-let-ice.rs": "Weird ! type in a union?",
    "pass/issues/issue-3200-packed-field-offset.rs": "We don't handle repr(packed)",
    "pass/issues/issue-3200-packed2-field-offset.rs": "We don't handle repr(packed)",
    "pass/issues/issue-5917.rs": "We don't handle pointers derived from globals properly, freeing referenced values",
    "pass/issues/issue-miri-1075.rs": "We don't check the status code on process::exit(N) -- 0 is ok!",
    "pass/observed_local_mut.rs": "We don't provide a way to disable aliasing checks",
    "pass/option_box_transmute_ptr.rs": "We don't handle the null pointer optimization for Option<Box<T>>",
    "pass/overflow_checks_off.rs": "We don't provide a way to disable overflow checks",
    "pass/partially-uninit.rs": "We don't handle unions properly, and lose data on transmutes",
    "pass/provenance.rs": "It is unclear how to properly do ptr-int-ptr conversions",
    "pass/ptr_int_from_exposed.rs": "It is unclear how to properly do ptr-int-ptr conversions",
    "pass/ptr_offset.rs": "It is unclear how to properly do ptr-int-ptr conversions",
    "pass/slices.rs": "We shouldn't dereference pointers to ZSTs...?",
    "pass/u128.rs": "We don't do int-float-int conversions properly",
    "panic/mir-validation.rs": "We don't validate the MIR for projections",
}

PWD = Path(os.path.dirname(os.path.abspath(__file__)))


def build_rusteria():
    env = (
        subprocess.check_output("opam exec -- dune exec -- env", shell=True)
        .decode()
        .split("\n")
    )
    for line in env:
        if not "=" in line:
            continue
        name, value = line.split("=", 1)
        os.environ[name] = value
    os.environ["RUSTERIA_PLUGINS"] = str((PWD / ".." / "plugins").resolve())
    try:
        subprocess.check_call("dune build", shell=True)
    except subprocess.CalledProcessError:
        print(f"{RED}Rusteria couldn't build")
        exit(1)


def determine_failure_expect(filepath: str) -> bool:
    if "kani" in filepath:
        try:
            with open(filepath, "r") as f:
                content = f.read()
                return "kani-verify-fail" in content
        except:
            ...
    elif "miri" in filepath:
        return "/tests/fail/" in filepath or "/tests/panic/" in filepath
    return False


def subprocess_run(
    *popenargs, input=None, capture_output=False, timeout=None, check=False, **kwargs
):
    """
    Patched version of subprocess.run, that uses a SIGTERM rather than a SIGKILL
    to terminate on timeout.
    """
    from subprocess import (
        PIPE,
        Popen,
        TimeoutExpired,
        CalledProcessError,
        CompletedProcess,
    )

    if input is not None:
        if kwargs.get("stdin") is not None:
            raise ValueError("stdin and input arguments may not both be used.")
        kwargs["stdin"] = PIPE

    if capture_output:
        if kwargs.get("stdout") is not None or kwargs.get("stderr") is not None:
            raise ValueError(
                "stdout and stderr arguments may not be used " "with capture_output."
            )
        kwargs["stdout"] = PIPE
        kwargs["stderr"] = PIPE

    with Popen(*popenargs, **kwargs) as process:
        try:
            stdout, stderr = process.communicate(input, timeout=timeout)
        except TimeoutExpired as exc:
            process.terminate()
            # NOTE!!! HERE WE WINDOWS SHOULD BE HANDLED DIFFERENTLY
            # POSIX _communicate already populated the output so
            # far into the TimeoutExpired exception.
            process.wait()
            raise
        except:  # Including KeyboardInterrupt, communicate handled that.
            process.terminate()
            # We don't call process.wait() as .__exit__ does that for us.
            raise
        retcode = cast(int, process.poll())
        if check and retcode:
            raise CalledProcessError(
                retcode, process.args, output=stdout, stderr=stderr
            )
    return CompletedProcess(process.args, retcode, stdout, stderr)
