import os
import sys
from pathlib import Path

for root, dirs, files in os.walk(sys.argv[1]):
    file_paths = [Path(root) / file for file in files]
    if len(file_paths) == 0:
        continue
    has_cue = any([True for p in file_paths if p.suffix == ".cue"])
    flac_cnt = len([p for p in file_paths if p.suffix in (".flac", ".m4a")])

    if has_cue and flac_cnt < 2:
        print(root)
