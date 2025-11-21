#!/usr/bin/env python3

import os, json, contextlib, argparse
from pathlib import Path
from typing import Optional
from dataclasses import dataclass, field
from utils import *
import shutil


class ExperimentException(Exception):
    pass


####### GLOBAL CONFIGURATION ########

default_experiment_folder: Path = Path("../soteria-c-experiments")
default_solver_timeout = 2  # ms
default_experiments_to_run: Optional[list[str]] = None

# Later modified by the script to include available experiments
available_experiments: list[str] = []


parser = argparse.ArgumentParser(
    description="Run Soteria-C experiments with configurable options."
)

parser.add_argument(
    "--experiment-folder",
    type=Path,
    default=default_experiment_folder,
    help=f"Path to the experiments folder (default: {default_experiment_folder})",
)


parser.add_argument(
    "--solver-timeout",
    type=int,
    default=default_solver_timeout,
    help=f"Solver timeout in milliseconds (default: {default_solver_timeout})",
)

parser.add_argument(
    "--experiment",
    type=str,
    nargs="*",
    default=default_experiments_to_run,
    help=f"List of experiment names to run (default: all, available: {', '.join(available_experiments)})",
)

parser.add_argument(
    "--cleanup-results-first",
    action="store_true",
    help="Remove existing results folder before running experiments",
)

parser.add_argument(
    "--cleanup-builds-first",
    action="store_true",
    help="Remove existing build folders before running experiments",
)

parser.add_argument(
    "--cleanup-first",
    action="store_true",
    help=(
        "Remove existing results and build folders before running experiments "
        "(equivalent to --cleanup-results-first and --cleanup-builds-first)"
    ),
)

parser.add_argument(
    "--benchmark",
    action="store_true",
    help="Run experiments using hyperfine for benchmarking",
)


def validate_args(args):
    if args.experiment is None:
        return args

    for experiment_name in args.experiment:
        if experiment_name not in available_experiments:
            parser.error(
                f"Experiment '{experiment_name}' not found. Available experiments are: {', '.join(available_experiments)}"
            )
    return args


class GlobalConfig:
    experiment_folder: Path
    solver_timeout: int
    experiments_to_run: Optional[list[str]]
    cleanup_results_first: bool
    cleanup_builds_first: bool
    benchmark: bool

    def __init__(self):
        self.experiment_folder = default_experiment_folder.resolve()
        self.solver_timeout = default_solver_timeout
        self.experiments_to_run = default_experiments_to_run
        self.cleanup_results_first = False
        self.cleanup_builds_first = False
        self.benchmark = False

    def result_folder(self) -> Path:
        return self.experiment_folder / "results"

    def set_from_args(self, args):
        self.experiment_folder = args.experiment_folder.resolve()
        self.solver_timeout = args.solver_timeout
        self.experiments_to_run = args.experiment
        self.cleanup_results_first = args.cleanup_results_first or args.cleanup_first
        self.cleanup_builds_first = args.cleanup_builds_first or args.cleanup_first
        self.benchmark = args.benchmark

    def to_dict(self):
        return {
            "experiment_folder": str(self.experiment_folder),
            "solver_timeout": self.solver_timeout,
            "experiments_to_run": self.experiments_to_run,
            "cleanup_results_first": self.cleanup_results_first,
            "cleanup_builds_first": self.cleanup_builds_first,
            "benchmark": self.benchmark,
        }


global_config = GlobalConfig()


######### Normalising current folder to project root #########


def current_folder_is_root():
    """Check if the current folder is the root of the Soteria project."""
    if not Path("dune-project").exists():
        return False

    with open("dune-project", "r") as f:
        content = f.read()
        return "(name soteria)" in content


def go_to_dune_root():
    previous = None
    cwd = Path.cwd()
    while cwd != previous and not current_folder_is_root():
        previous = cwd
        cwd = Path.cwd().parent
        os.chdir(cwd)
    if previous == cwd:
        print(
            f"{RED}Error: This script must be run from within the Soteria project directory.{RESET}"
        )
        exit(2)


########## Experiment running ##########


@dataclass(frozen=True)
class ExperimentConfig:
    """Relative path to the experiment folder."""

    name: str
    path: Path
    soteria_args: list[str] = field(default_factory=list)
    """Location relative to the experiment folder where CMake should be run."""
    cmake_build_path: Path = field(default=Path("build"))

    cmake_args: list[str] = field(default_factory=list)

    def __post_init__(self):
        available_experiments.append(self.name)


class Experiment(PrintersMixin):
    config: ExperimentConfig
    compile_commands: Path
    result_folder: Path

    def __init__(self, config: ExperimentConfig):
        self.result_folder = global_config.result_folder() / config.name
        os.makedirs(self.result_folder, exist_ok=True)
        self.config = config
        self.compile_commands = (
            global_config.experiment_folder
            / config.path
            / config.cmake_build_path
            / "compile_commands.json"
        ).resolve()

    def print_prefix(self) -> str:
        return f"{BOLD}{CYAN}{self.config.name}{RESET} - "

    def run_command(self, cmd: str):
        self.print_message(f"{MAGENTA}Running:\n{cmd}{RESET}")
        os.system(cmd)

    def cleanup_build(self):
        new_dir = global_config.experiment_folder / self.config.path
        ask_and_remove((new_dir / self.config.cmake_build_path).resolve())

    def make_compile_commands(self):
        new_dir = global_config.experiment_folder / self.config.path
        if self.compile_commands.exists():
            self.print_info(
                f"compile_commands.json already exists, skipping generation."
            )
            return

        with contextlib.chdir(new_dir):
            cmd = (
                "cmake -S . -B "
                f"{self.config.cmake_build_path} "
                "-DCMAKE_EXPORT_COMPILE_COMMANDS=1 "
                f"{' '.join(self.config.cmake_args)}"
            )
            self.run_command(cmd)
            if self.compile_commands.exists():
                self.print_success(f"Successfully generated compile_commands.json")
            else:
                msg = f"Failed to generate {self.compile_commands}"
                self.print_error(msg)
                raise ExperimentException(msg)
        return

    def run_soteria_c(self):
        if self.compile_commands is None:
            self.print_error("Script error: compile_commands.json not generated.")
            exit(3)
        self.print_info(
            f"Running Soteria-C with compile_commands: {self.compile_commands}"
        )
        os.makedirs(self.result_folder, exist_ok=True)
        cmd = (
            "dune exec -- time soteria-c capture-db "
            f"{self.compile_commands} "
            f"--solver-timeout {global_config.solver_timeout} "
            "--dump-stats "
            f"{self.result_folder / 'stats.json'} "
            f"{' '.join(self.config.soteria_args)}"
        )
        if global_config.benchmark:
            if shutil.which("hyperfine") is None:
                self.print_error(
                    "Hyperfine is required for benchmarking but was not found in PATH. Install it and try again."
                )
                exit(4)
            cmd = f"hyperfine '{cmd}' --warmup 1 --runs 10"
        self.run_command(cmd)

    def run(self):
        print(f"{BOLD}{CYAN}Running experiment: {self.config.name}{RESET}")
        self.make_compile_commands()
        self.run_soteria_c()


########## List experiments ##########

simple_config = lambda name: ExperimentConfig(name=name, path=Path(name))
configs = [
    ExperimentConfig(
        name="Collections-C",
        path=Path("Collections-C"),
        soteria_args=["--use-cerb-headers", "--havoc-undef"],
    ),
    simple_config("zlib"),
    simple_config("libgit2"),
    simple_config("nghttp2"),
    simple_config("mbedtls"),
    simple_config("libtommath"),
    simple_config("c-ares"),
    simple_config("zlib-ng"),
    simple_config("aws-c-common"),
]


def run_experiments(experiments: list[Experiment]):
    for experiment in experiments:
        try:
            experiment.run()
        except ExperimentException as e:
            experiment.print_error(f"Experiment Failed")
        print("\n")


def selected_experiments():
    experiments_to_run = (
        global_config.experiments_to_run
        if global_config.experiments_to_run
        else available_experiments
    )
    return [Experiment(c) for c in configs if c.name in experiments_to_run]


def at_start():
    go_to_dune_root()
    args = parser.parse_args()
    validate_args(args)
    global_config.set_from_args(args)
    if global_config.cleanup_results_first:
        ask_and_remove(global_config.result_folder())


def aggregate_results():
    result_folder = global_config.result_folder()
    if not result_folder.exists():
        print(f"{RED}No results folder found: {result_folder}{RESET}")
        return

    all_stats = Stats.empty()
    for subfolder in result_folder.iterdir():
        file = subfolder / "stats.json"
        if not subfolder.is_dir() or not file.exists():
            continue
        with open(file, "r") as f:
            try:
                j = json.load(f)
            except:
                global_printer.print_error(f"Failed to read json from {file}")
                continue
            try:
                j = Stats.from_dict(j)
            except:
                global_printer.print_error(f"{file}'s json doesn't match expectations")
                continue
            all_stats = merge_stats(all_stats, j)
    to_remove = "Unsupported: Could not resolve function"
    if to_remove in all_stats.give_up_reasons:
        del all_stats.give_up_reasons[to_remove]
    all_stats.give_up_reasons = dict(
        sorted(all_stats.give_up_reasons.items(), key=lambda kv: kv[1], reverse=True)
    )
    with open(result_folder / "all_stats.json", "w") as f:
        json.dump(all_stats.as_dict(), f, indent=2)
        global_printer.print_success(
            f"Aggregated unsupported features written to {result_folder / 'all_stats.json'}"
        )


if __name__ == "__main__":
    at_start()
    experiments = selected_experiments()
    if global_config.cleanup_builds_first:
        for experiment in experiments:
            experiment.cleanup_build()
    run_experiments(experiments)
    aggregate_results()
