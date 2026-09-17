#!/usr/bin/env python

import argparse
import os
import shlex
import subprocess
import webbrowser

from fzf import FZF

"""
~/streamers.txt
https://www.twitch.tv/xqc,xqc
https://www.douyu.com/9999,yyf
"""

is_termux = "com.termux" in os.environ.get("PREFIX", "")

if is_termux:
    http_proxy = ""
else:
    http_proxy = "socks5h://127.0.0.1:10808"


def get_streamer_urls():
    streamer_urls: dict[str, str] = {}
    with open(os.path.expanduser("~/streamers.txt")) as fp:
        for line in fp:
            line = line.strip()
            if line == "":
                continue
            url, streamer = line.split(",")
            streamer_urls[streamer] = url

    return streamer_urls


def run_detached_process(args, **kwargs):
    # https://learn.microsoft.com/en-us/windows/win32/procthread/process-creation-flags
    if "nt" == os.name:
        creationflags = (
            subprocess.CREATE_NO_WINDOW
            | subprocess.CREATE_NEW_PROCESS_GROUP
            | subprocess.DETACHED_PROCESS
        )

        pkwargs = {
            "close_fds": True,  # close stdin/stdout/stderr on child
            "creationflags": creationflags,
        }
    else:
        pkwargs = {}

    subprocess.Popen(args, **pkwargs, **kwargs)


def main():
    parser = argparse.ArgumentParser(prog="OpenLivestream")
    parser.add_argument("-b", "--open-in-browser", action="store_true")
    parser.add_argument("-r", "--record", action="store_true")
    parser.add_argument("-q", "--no-danmu", action="store_true")
    parser.add_argument("-d", "--danmu", action="store_true")
    parser.add_argument("url", nargs="?", default="")
    args = parser.parse_args()

    if args.url.isdigit():
        args.url = f"https://douyu.com/{args.url}"

    if not (args.url.startswith("http")):
        # select the only match
        fzf_extras = None
        if args.url is not None:
            fzf_extras = ["-e", "-1", "-q", args.url]

        streamer_urls = get_streamer_urls()
        fzf = FZF(fzf_extras=fzf_extras)
        fzf.input = list(streamer_urls.keys())
        streamer: str = fzf.prompt()
        if streamer == "":
            return
        url = streamer_urls[streamer]

    else:
        url: str = args.url
        streamer = url

    if args.open_in_browser:
        webbrowser.open(url)
    elif args.danmu:
        danmu_cmd = ["danmu.exe", url]
        subprocess.call(danmu_cmd)
    else:
        streamlink_cmd = [
            "streamlink",
            "--http-no-ssl-verify",
            "--retry-streams",
            "10",
            "--retry-max",
            "10",
            "--retry-open",
            "10",
            url,
            "best",
        ]
        if is_termux:
            player_args = [
                "--player-external-http",
                "--player-external-http-port",
                "4567",
            ]

        else:
            player_args = [
                "--player",
                "mpv.exe",
                "--title",
                "{author} - {title}",
            ]

        streamlink_cmd.extend(player_args)

        if args.record:
            d = os.path.expanduser("~/recordings")
            os.makedirs(d, exist_ok=True)
            streamlink_cmd.extend(["--record", f"{d}/{streamer}.ts"])

        if not is_termux and ("twitch" in url or "youtube" in url):
            streamlink_cmd.extend(["--http-proxy", http_proxy])
        if "twitch" in url:
            streamlink_cmd.extend(["--twitch-disable-ads"])
            args.no_danmu = True

        if "bilibili" in url:
            streamlink_cmd = ["mpv.exe", url]

        if is_termux:
            subprocess.call(streamlink_cmd)
        else:
            if not args.no_danmu:
                danmu_cmd = ["danmu.exe", url]
                run_detached_process(danmu_cmd)

            print(shlex.join(streamlink_cmd))
            subprocess.Popen(
                streamlink_cmd,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                creationflags=subprocess.CREATE_NEW_PROCESS_GROUP,
            )


if __name__ == "__main__":
    main()
