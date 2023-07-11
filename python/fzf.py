import subprocess
from pathlib import Path
from typing import Iterable, List, Optional, Union

# from https://github.com/AceofSpades5757/fzflib

ENCODING: str = "utf-8"
MULTI_FLAG: str = "--multi"
FZFInputValues = Union[bytes, str, Iterable[str], Iterable[bytes]]
PathLike = Union[bytes, str, Path]


def resolve_input(input_values: Optional[FZFInputValues]) -> bytes:
    """Resolve input values, for FZF, to bytes."""

    if not input_values:
        return b""

    if isinstance(input_values, bytes):
        return input_values

    if isinstance(input_values, str):
        return input_values.encode(ENCODING)

    if isinstance(input_values, Iterable):
        first_value: Union[str, bytes] = input_values[0]  # type: ignore

        if isinstance(first_value, bytes):
            return b"\n".join(input_values)  # type: ignore
        elif isinstance(first_value, str):
            return "\n".join(input_values).encode(ENCODING)  # type: ignore

    raise TypeError(f"Unsupported input type: {type(input_values)}")


class FZF:
    """Class abstracting FZF command."""

    def __init__(
        self,
        executable: Optional[str] = None,
        input: Optional[FZFInputValues] = None,
        cwd: Optional[PathLike] = None,
        multi: bool = False,
        fzf_extras: Optional[List[str]] = None,
    ) -> None:
        if executable is None:
            executable = "fzf"
        if fzf_extras is None:
            fzf_extras = []

        self.fzf = executable
        self.input = input
        self.cwd: Optional[PathLike] = cwd
        self.multi: bool = multi

        # Extra Arguments
        self.fzf_args: List = fzf_extras

    def prompt(self, *args, **kwargs) -> Union[str, List[str]]:
        """Given current configuration, run fzf and return selection."""

        # Buid Command
        command: List[str] = [self.fzf]
        if self.multi:
            command.append(MULTI_FLAG)

        command += self.fzf_args

        input_: bytes = resolve_input(self.input)
        if input_:
            kwargs["stdin"] = subprocess.PIPE

        # Run FZF as Process
        process: subprocess.Popen = subprocess.Popen(
            command,
            *args,
            stdout=subprocess.PIPE,
            encoding=ENCODING,
            cwd=self.cwd,
            **kwargs,
        )  # type: ignore

        # Process - Input
        if input_:
            stdout, _ = process.communicate(input_.decode(ENCODING))
        else:
            stdout, _ = process.communicate()

        # Process - Output
        if self.multi:
            return stdout.splitlines()
        else:
            return stdout.strip()
