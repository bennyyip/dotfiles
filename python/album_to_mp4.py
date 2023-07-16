import argparse
import json
import os
import secrets
import subprocess
import traceback
from dataclasses import dataclass
from pathlib import Path

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
    info = ffmpeg_probe(fname)
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

    # Concat audios
    tmp_fname = f"filelist_{secrets.token_hex(5)}.txt"

    with open(tmp_fname, "w") as fp:
        for t in tracks:
            line = f"""file {t.fname.as_posix().replace(' ','\\ ').replace("'", "\\'")}\n"""
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

    # Attach cover and make a video
    cmd = [
        "ffmpeg",
        "-i",
        audio_fname,
        "-loop",
        "1",
        "-i",
        cover,
        "-map",
        "0",
        "-map",
        "1",
        "-acodec",
        "copy",
        "-shortest",
        "-vcodec",
        "libx264",
        video_fname,
        "-y",
    ]

    try:
        exit_code = subprocess.call(cmd)
        assert exit_code == 0
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


def ffmpeg_probe(filename, cmd="ffprobe"):
    """Run ffprobe on the specified file and return a JSON representation of the output.

    Raises:
        :class:`ffmpeg.Error`: if ffprobe returns a non-zero exit code,
            an :class:`Error` is returned with a generic error message.
            The stderr output can be retrieved by accessing the
            ``stderr`` property of the exception.
    """
    args = [cmd, "-show_format", "-show_streams", "-of", "json"]
    args += [filename]

    p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = p.communicate()
    if p.returncode != 0:
        raise OSError("ffprobe", out, err)
    return json.loads(out.decode("utf-8"))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="album_to_mp4")
    parser.add_argument("folder", help="folder contains audio files")
    parser.add_argument("cover", help="path to album art")
    args = parser.parse_args()

    d = Path(args.folder)
    cover = Path(args.cover)
    os.chdir(d)

    audios = [d / f for f in os.listdir(d) if (d / f).suffix in audio_suffices]
    assert len(audios) > 0

    tracks = list(map(probe, audios))
    to_mp4(tracks, cover)
    print("Tracklist:")
    print(get_track_list(tracks))
