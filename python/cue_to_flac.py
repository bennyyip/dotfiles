import argparse
import os
import subprocess
import unicodedata
from pathlib import Path


def sanitize_filename(filename: str):
    blacklist = ["\\", "/", "?", '"', "<", ">", "|", "\0", "*", ":"]
    filename = "".join(c for c in filename if c not in blacklist)
    filename = unicodedata.normalize("NFKC", filename)
    return filename


def run_cmds(cmds):
    for cmd in cmds:
        subprocess.run(cmd)


def get_cut_cmds(cue_file):
    p = Path(cue_file)
    if not (p.is_file() and p.suffix == ".cue"):
        raise ValueError("input is not cue file")
    os.chdir(p.parent)

    with open(cue_file) as fp:
        lines = iter(fp.readlines())

    general = {}

    tracks = []

    current_file = None

    files_to_delete = set()
    files_to_delete.add(cue_file)

    ret = []

    for line in lines:
        if line.startswith("REM GENRE "):
            general["genre"] = (
                " ".join(line.split(" ")[2:]).replace('"', "").replace("\n", "")
            )
        if line.startswith("REM DATE "):
            general["date"] = " ".join(line.split(" ")[2:]).replace("\n", "")
        if line.startswith("PERFORMER "):
            general["artist"] = (
                " ".join(line.split(" ")[1:]).replace('"', "").replace("\n", "")
            )
            general["album_artist"] = general["artist"]
        if line.startswith("TITLE "):
            general["album"] = (
                " ".join(line.split(" ")[1:]).replace('"', "").replace("\n", "")
            )
        if line.startswith("FILE "):
            current_file = " ".join(line.split(" ")[1:-1]).replace('"', "")

        if line.startswith("  TRACK "):
            track = general.copy()
            track["track"] = int(line.strip().split(" ")[1], 10)
            track["file"] = current_file

            tracks.append(track)

        if line.startswith("    TITLE "):
            tracks[-1]["title"] = (
                " ".join(line.strip().split(" ")[1:]).replace('"', "").replace("\n", "")
            )
        if line.startswith("    PERFORMER "):
            tracks[-1]["artist"] = (
                " ".join(line.strip().split(" ")[1:]).replace('"', "").replace("\n", "")
            )
        if line.strip().startswith("INDEX 01 "):
            t = list(
                map(
                    int,
                    " ".join(line.strip().split(" ")[2:]).replace('"', "").split(":"),
                )
            )
            tracks[-1]["start"] = 60 * t[0] + t[1] + t[2] / 100.0

    for i in range(len(tracks)):
        if i != len(tracks) - 1 and tracks[i]["file"] == tracks[i + 1]["file"]:
            tracks[i]["duration"] = tracks[i + 1]["start"] - tracks[i]["start"]

    for track in tracks:
        metadata = {
            "artist": track["artist"],
            "album_artist": track["album_artist"],
            "title": track["title"],
            "album": track["album"],
            "track": track["track"],
            # "TOTALTRACKS": len([track for x in tracks if track["file"] == x["file"]]),
            "TOTALTRACKS": len(tracks),
        }

        if "genre" in track:
            metadata["genre"] = track["genre"]
        if "date" in track:
            metadata["date"] = track["date"]

        cmd = [
            "ffmpeg",
            "-y",
            "-i",
            track["file"],
            "-ss",
            "%.2d:%.2d:%09.6f"
            % (
                track["start"] / 60 / 60,
                track["start"] / 60 % 60,
                track["start"] % 60,
            ),
        ]

        if "duration" in track:
            cmd.append("-t")

            cmd.append(
                "%.2d:%.2d:%09.6f"
                % (
                    track["duration"] / 60 / 60,
                    track["duration"] / 60 % 60,
                    track["duration"] % 60,
                )
            )

        for k, v in metadata.items():
            cmd.append("-metadata")
            cmd.append("%s=%s" % (k, v))

        filename = "%.2d. %s.flac" % (
            track["track"],
            track["title"],
        )

        if filename != sanitize_filename(filename):
            print(filename, sanitize_filename(filename))

        cmd.append(sanitize_filename(filename))

        ret.append(cmd)

        files_to_delete.add(track["file"])

    return ret, files_to_delete


def cut_cue(cue_file: Path, do_delete: bool):
    working_dir = cue_file.parent.absolute()
    os.chdir(working_dir)
    cut_cmds, files_to_delete = get_cut_cmds(cue_file)
    run_cmds(cut_cmds)
    if do_delete:
        for f in files_to_delete:
            os.remove(f)
    print(f"{cue_file} done")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="cue_to_flac",
        description="convert cue to flacs",
    )
    parser.add_argument(
        "-d", "--delete", action="store_true", help="delete cue and wav files"
    )
    parser.add_argument(
        "cue_files",
        nargs="*",
        help="cue files, use the only cue file in cwd if omitted.",
    )
    args = parser.parse_args()

    cwd_cue_files = [f for f in os.listdir() if f.endswith(".cue")]
    if len(args.cue_files) == 0:
        if len(cwd_cue_files) == 1:
            cue_files = [cwd_cue_files[0]]
        else:
            raise ValueError("No cue file")
    else:
        cue_files = args.cue_files

    for cue_file in cue_files:
        cut_cue(Path(cue_file), args.delete)
