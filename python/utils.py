import fileinput
from typing import Iterator


def edit_file(filename: str) -> Iterator[str]:
    """Edit the file in a loop, e.g.:

    .. code-block:: python

        for line in edit_file('PKGBUILD'):
          if line.startswith('_name='):
            line = '_name=newname'
          print(line)
    """
    with fileinput.input(files=(filename,), inplace=True) as f:
        for line in f:
            yield line.rstrip("\n")
