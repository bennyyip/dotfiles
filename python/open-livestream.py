import argparse
import asyncio
import os
import re
import subprocess
import webbrowser

import httpx

from fzf import FZF

"""
~/streamers.csv
https://www.twitch.tv/xqc,xqc
https://www.douyu.com/9999,yyf
"""


is_termux = "com.termux" in os.environ.get("PREFIX", "")

client = httpx.AsyncClient()
if is_termux:
    http_proxy = ""
else:
    http_proxy = "http://127.0.0.1:10809"


class TwitchAPI:
    CLIENT_ID = "kimne78kx3ncx6brgo4mv6wki5h1ko"

    def __init__(self) -> None:
        self.headers = {
            "Client-ID": self.CLIENT_ID,
        }
        self.client = httpx.AsyncClient(proxies=http_proxy, headers=self.headers)

    async def call(self, data, **kwargs):
        res = await self.client.post(
            "https://gql.twitch.tv/gql",
            json=data,
            headers={**self.headers, **kwargs.pop("headers", {})},
            **kwargs,
        )

        return res.json()

    @staticmethod
    def _gql_persisted_query(operationname, sha256hash, **variables):
        return {
            "operationName": operationname,
            "extensions": {
                "persistedQuery": {
                    "version": 1,
                    "sha256Hash": sha256hash,
                },
            },
            "variables": dict(**variables),
        }

    async def metadata_channel(self, channel):
        queries = [
            self._gql_persisted_query(
                "StreamMetadata",
                "059c4653b788f5bdb2f5a2d2a24b0ddc3831a15079001a3d927556a96fb0517f",
                channelLogin=channel,
            ),
        ]
        return await self.call(queries)

    async def is_online(self, channel):
        resp = await self.metadata_channel(channel)
        return resp[0]["data"]["user"]["stream"] is not None


async def is_online(url: str) -> bool:
    room_id = url.split("/")[-1]
    twitch = TwitchAPI()
    try:
        if "douyu.com" in url:
            resp = await client.get(
                f"https://open.douyucdn.cn/api/RoomApi/room/{room_id}"
            )
            return resp.json()["data"]["room_status"] == "1"
        elif "bilibili.com" in url:
            resp = await client.get(
                f"https://api.live.bilibili.com/room/v1/Room/room_init?id={room_id}"
            )
            return resp.json()["data"]["live_status"] == 1
        elif "huya.com" in url:
            resp = await client.get(url)
            return (
                re.search(r'var TT_ROOM_DATA = {"type":".+","state":"ON"', resp.text)
                is not None
            )
        elif "twitch.tv" in url:
            return await twitch.is_online(room_id)
    except:
        print(f"Failed to get online status for {url}")
        return False

    return True


async def get_streamer_urls(filter_online):
    streamer_urls: dict[str, str] = {}
    with open(os.path.expanduser("~/streamers.csv")) as fp:
        for line in fp:
            line = line.strip()
            if line == "":
                continue
            url, streamer = line.split(",")
            streamer_urls[streamer] = url

    if filter_online:
        keys = list(streamer_urls.keys())

        tasks = [is_online(streamer_urls[k]) for k in keys]

        result = await asyncio.gather(*tasks)

        online = dict(zip(keys, result))
        streamer_urls = {k: v for k, v in streamer_urls.items() if online[k]}

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

    print(" ".join(args))

    subprocess.Popen(args, **pkwargs, **kwargs)


async def main():
    parser = argparse.ArgumentParser(prog="OpenLivestream")
    parser.add_argument("-o", "--filter-online", action="store_true")
    parser.add_argument("-b", "--open-in-browser", action="store_true")
    parser.add_argument("-r", "--record", action="store_true")
    parser.add_argument("url", nargs="?")
    args = parser.parse_args()

    if args.url is None or not (args.url.startswith("http")):
        streamer_urls = await get_streamer_urls(args.filter_online)
        fzf = FZF()
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
    else:
        streamlink_cmd = [
            "streamlink",
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
            player_args = ["--player", "mpv"]

        streamlink_cmd.extend(player_args)

        if args.record:
            d = os.path.expanduser("~/recordings")
            os.makedirs(d, exist_ok=True)
            streamlink_cmd.extend(["--record", f"{d}/{streamer}.ts"])

        if not is_termux and ("twitch" in url or "youtube" in url):
            streamlink_cmd.extend(["--http-proxy", http_proxy])
        if "twitch" in url:
            streamlink_cmd.extend(["--twitch-disable-ads"])

        if is_termux:
            subprocess.call(streamlink_cmd)
        else:
            run_detached_process(streamlink_cmd)

            danmu_cmd = ["danmu.CMD", url]
            run_detached_process(danmu_cmd)


if __name__ == "__main__":
    asyncio.run(main())
