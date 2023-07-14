import os
import secrets
import subprocess
import sys
import traceback
from dataclasses import dataclass
from pathlib import Path
from typing_extensions import reveal_type

import ffmpeg

audio_suffices = [".mp3", ".flac", ".m4a", ".aac"]


@dataclass
class Track:
    artist: str
    album: str
    trackno: str
    title: str
    duration: float
    fname: Path


def format_time(t: float):
    return f"{int(t/60):02d}:{int(t%60):02d}"


def probe(fname: Path):
    info = ffmpeg.probe(fname)
    fmt = info["format"]
    duration = float(fmt["duration"])
    tags = fmt["tags"]
    album = tags.get("ALBUM", tags.get("album"))
    artist = tags.get("album_artist")
    title = tags.get("TITLE", tags.get("title"))
    if artist is None:
        artist = tags.get("ARTIST")
    trackno = tags["track"]

    return Track(
        artist=artist,
        album=album,
        duration=duration,
        fname=fname,
        trackno=trackno,
        title=title,
    )


def to_mp4(tracks: list[Track], cover: Path):
    tracks.sort(key=lambda x: x.trackno)
    artist = tracks[0].artist
    album = tracks[0].album
    ext = tracks[0].fname.suffix

    audio_fname = f"{artist} - {album}{ext}"
    video_fname = f"{artist} - {album}.mp4"

    tmp_fname = f"filelist_{secrets.token_hex(5)}.txt"

    with open(tmp_fname, "w") as fp:
        for t in tracks:
            line = f"file '{t.fname.as_posix()}'\n"
            fp.write(line)
    cmd = [
        "ffmpeg",
        "-safe",
        "0",
        "-f",
        "concat",
        "-i",
        fp.name,
        "-c",
        "copy",
        audio_fname,
    ]
    exit_code = subprocess.call(cmd)
    os.remove(tmp_fname)
    assert exit_code == 0

    audio = ffmpeg.input(audio_fname)
    video = ffmpeg.input(cover, loop=1)
    try:
        ffmpeg.output(
            audio,
            video,
            video_fname,
            shortest=None,
            vcodec="libx264",
            acodec="copy",
        ).overwrite_output().run()
    except:
        traceback.print_exc()
    finally:
        os.remove(audio_fname)


def get_track_list(tracks):
    track_list = ""
    duration = 0
    for t in tracks:
        track_list += f"{format_time(duration)} {t.title}\n"
        duration += t.duration
    return track_list


if __name__ == "__main__":
    reveal_type(to_mp4)
    d = Path(sys.argv[1])
    cover = Path(sys.argv[2])
    os.chdir(d)

    audios = [d / f for f in os.listdir(d) if (d / f).suffix in audio_suffices]
    assert len(audios) > 0

    tracks = list(map(probe, audios))
    to_mp4(tracks, cover)
    print("Tracklist:")
    print(get_track_list(tracks))
