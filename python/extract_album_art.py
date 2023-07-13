import os
import sys
from pathlib import Path
from subprocess import DEVNULL, Popen

rootpath = sys.argv[1]
audio_suffices = [".mp3", ".flac", ".m4a", ".aac"]
for root, dirs, files in os.walk(rootpath):
    if len(files) == 0:
        continue

    if "cover.jpg" in files:
        continue

    thefile = None
    for x in files:
        if Path(x).suffix in audio_suffices:
            thefile = x
            break
    if thefile is None:
        continue

    ffmpeg_args = [
        "ffmpeg",
        "-i",
        os.path.join(root, thefile),
        "-an",
        "-vcodec",
        "copy",
        os.path.join(root, "cover.jpg"),
    ]

    process = Popen(ffmpeg_args, stdout=DEVNULL, stderr=DEVNULL)
    (output, err) = process.communicate()
    exit_code = process.wait()

    if exit_code != 0:
        print(f"{root} extract album art failed")
