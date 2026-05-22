#!/usr/bin/env python3

from pathlib import Path
import time

WATCH_LATER_DIR = Path("./.state/watch_later/")


now = time.time()
for p in WATCH_LATER_DIR.iterdir():
    with open(p) as f:
        line = f.readline().strip()
    if (
        line == "# -"
        or line == "# redirect entry"
        or (now - p.stat().st_mtime > 3600 * 24 * 365)
    ):
        p.unlink()
        continue

    video = line[2:]
    if video.startswith("C:\\") and not Path(video).exists():
        p.unlink()
        continue

    print(p, video)
    # if not (video.startswith("C:\\") or video.startswith("http")):
    #     print(p, video)
