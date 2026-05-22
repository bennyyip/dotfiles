#!/usr/bin/env python3
from typing import Iterable
import re
import subprocess
import sys
from datetime import timedelta
from pathlib import Path

import av
import srt
from faster_whisper import WhisperModel
from faster_whisper.transcribe import Segment

# model_size = "distil-large-v3"
MODEL_PATH = str(
    Path("~/ghq/huggingface.co/Systran/faster-distil-whisper-large-v3").expanduser()
)

# https://github.com/peterk/srt_equalizer {{{1
def split_subtitle(
    sub: srt.Subtitle,
    target_chars: int = 42,
    start_from_index: int = 1,
    method: str = "greedy",
) -> list[srt.Subtitle]:
    """If the subtitle length is > target_chars, split it into a list of subtitles within the same
    time span. If not, return the subtitle as is. The time code is adjusted proportionally
    to the length of the subtitle.

    Args:
        sub: A srt.Subtitle object.
        target_chars: The max number of characters for a subtitle line.
        start_from_index: The start index of the subtitle item.
        method: algorithm for splitting - either "greedy" (default), "halving" or "punctuation".

    Returns:
        An array of one or more subtitle items.
    """

    if len(sub.content) <= target_chars:
        # keep this item as is, only adjust the start index if necessary.
        sub.index = start_from_index + 1
        return [sub]

    elif method == "greedy":
        text_chunks = split_greedy(sub.content, target_chars)
    elif method == "halving":
        text_chunks = split_at_half(sub.content, target_chars)
    else:
        assert method == "punctuation"
        text_chunks = split_by_punctuation(sub.content, target_chars)

    # Create a new subtitle item for each text chunk, proportional to its length.
    split_subs = []
    total_length = len(sub.content)
    current_time = sub.start

    for i, chunk in enumerate(text_chunks):
        chunk_length = len(chunk)
        chunk_duration = chunk_length / total_length * (sub.end - sub.start)
        start_time = current_time

        # set end time to original sub item end time if this is the last chunk.
        if i == len(text_chunks) - 1:
            end_time = sub.end
        else:
            end_time = current_time + chunk_duration

        new_subtitle = srt.Subtitle(
            index=start_from_index + i, start=start_time, end=end_time, content=chunk
        )
        split_subs.append(new_subtitle)
        current_time = end_time

    return split_subs


def equalize_srt_file(
    srt_path: str, output_srt_path: str, target_chars: int, method="greedy"
):
    """Load subs from an SRT file and output equalized subtitles to a new SRT file."""
    assert method in {"greedy", "halving", "punctuation"}, method
    subs = load_srt(srt_path)

    adjusted_subs = []
    last_index = 0

    # Limit each subtitle to a maximum character length, splitting into
    # multiple subtitle items if necessary.
    for sub in subs:
        new_subs = split_subtitle(
            sub=sub,
            target_chars=target_chars,
            start_from_index=last_index,
            method=method,
        )
        last_index = new_subs[-1].index
        adjusted_subs.extend(new_subs)

    # Write the result to a new file
    write_srt(filepath=output_srt_path, subs=adjusted_subs)


def split_greedy(sentence: str, target_chars: int) -> list[srt.Subtitle]:
    """Split subtitles into chunks of target_chars length as soon as possible."""

    text_chunks = []
    current_chunk = ""
    words = sentence.split()
    for word in words:
        if len(current_chunk) + len(word) + 1 > target_chars:
            text_chunks.append(current_chunk.strip())
            current_chunk = word + " "
        else:
            current_chunk += word + " "
    if current_chunk:
        text_chunks.append(current_chunk.strip())

    return text_chunks


def split_at_half(sentence, target_chars):
    """Try to split subtitles into similar line lengths takign commas into account."""

    if len(sentence) <= target_chars or " " not in sentence:
        return [sentence]

    # find the central space
    center = len(sentence) // 2
    space_indices = []
    for ix, char in enumerate(sentence):
        if char == " ":
            distance_to_center = abs(ix - center)
            if (
                distance_to_center < target_chars / 4
                and ix > 0
                and sentence[ix - 1] == ","
            ):
                # boost splitting on commas that are not exacly in the middle, but close to
                distance_to_center /= 10

            space_indices.append((ix, distance_to_center))

    closest_space_to_center = sorted(space_indices, key=lambda x: x[1])[0][0]

    # recursively call this function until the length is bellow limit
    left = sentence[:closest_space_to_center]
    right = sentence[closest_space_to_center + 1 :]
    return split_at_half(left, target_chars) + split_at_half(right, target_chars)


def split_by_punctuation(sentence: str, target_chars: int) -> list[str]:
    """Split subtitles into chunks of target_chars length by punctuation."""

    if len(sentence) <= target_chars:
        return [sentence]

    # use regex to split the sentence by punctuation
    # Quote characters: ASCII '\" curly ""'' low-9 „‚ guillemets «»‹›
    chunks = re.split(
        r'([.,!?]+[\'""\u201C\u201D\u2018\u2019\u201E\u201A\u00AB\u00BB\u2039\u203A]*)',
        sentence,
    )
    normalized_chunks = []
    for chunk in chunks:
        # strip whitespace
        chunk = chunk.strip()

        # if this chunk is an empty one, skip it
        if not chunk:
            continue

        if len(chunk) > target_chars:
            normalized_chunks.extend(split_greedy(chunk, target_chars))
            continue

        if normalized_chunks:
            if re.fullmatch(
                r'[.,!?]+[\'""\u201C\u201D\u2018\u2019\u201E\u201A\u00AB\u00BB\u2039\u203A]*',
                chunk,
            ):
                # add punctuation to the last chunk
                chunk = normalized_chunks.pop() + chunk
            elif len(chunk) + len(normalized_chunks[-1]) <= target_chars:
                # add this chunk to the last one since they still under the limit allowed
                chunk = normalized_chunks.pop() + " " + chunk

        normalized_chunks.append(chunk)

    return normalized_chunks


def whisper_result_to_srt(segments: Iterable[Segment]) -> Iterable[srt.Subtitle]:
    """Convert Whisper ASR result segments to a list of srt.Subtitle items."""

    for i, segment in enumerate(segments, start=1):
        start_time = timedelta(seconds=float(segment.start))
        end_time = timedelta(seconds=float(segment.end))
        content = segment.text.strip()
        yield srt.Subtitle(index=i, content=content, start=start_time, end=end_time)


def load_srt(filepath: Path) -> list[srt.Subtitle]:
    """Load SRT file with path validation"""
    with open(filepath, "r") as f:
        return list(srt.parse(f.read()))


def write_srt(filepath: Path, subs: list[srt.Subtitle]):
    with open(filepath, "w") as f:
        f.write(srt.compose(subs))

# 1}}}

def extract_audio(video_file: Path):
    """
    ffmpeg -hide_banner -i 'file.mkv' -vn -c:a copy out.opus
    """
    video_container = av.open(video_file)
    audio_stream = video_container.streams.audio[0]
    codec_name = audio_stream.codec.name

    codec_to_ext = {
        "aac": "aac",
        "mp3": "mp3",
        "libvorbis": "oga",
        "vorbis": "oga",
        "flac": "flac",
        "ac3": "ac3",
        "opus": "opus",
        "alac": "m4a",
    }

    extension = codec_to_ext.get(codec_name)
    video_container.close()

    if extension is None:
        raise ValueError("Unknown codec: %s", codec_name)

    output_file = video_file.with_suffix("." + extension)

    subprocess.run(
        [
            "ffmpeg",
            "-hide_banner",
            "-y",
            "-i",
            str(video_file),
            "-vn",
            "-c:a",
            "copy",
            str(output_file),
        ],
        capture_output=True,
        text=True,
        check=True,
    )

    return output_file


def transcribe(file_or_dir: Path):
    model = WhisperModel(MODEL_PATH, device="cuda", compute_type="float16")
    if file_or_dir.is_file():
        transcribe_one_file(model, file_or_dir)
    elif file_or_dir.is_dir():
        for root, dirs, files in file_or_dir.walk():
            for file in files:
                f = root / file
                if f.suffix in [".mp4", ".mkv"] and not f.with_suffix(".srt").exists():
                    transcribe_one_file(model, f)


def transcribe_one_file(model: WhisperModel, video_file: Path):
    print(f"Transcribing '{video_file}'")
    audio_file = extract_audio(video_file)

    segments, _info = model.transcribe(
        audio_file,
        beam_size=5,
        language="en",
        condition_on_previous_text=False,
        vad_filter=True,
        vad_parameters={"min_silence_duration_ms": 1000},
        length_penalty=1.0,
        log_progress=True,
    )

    equalized = []
    for sub in whisper_result_to_srt(segments):
        equalized.extend(split_subtitle(sub, 78, method="greedy"))

    write_srt(video_file.with_suffix(".srt"), equalized)

    audio_file.unlink()


if __name__ == "__main__":
    transcribe(Path(sys.argv[1]))

# vim:fdm=marker
