import argparse
import asyncio
import json
import os
import subprocess
import webbrowser

import httpx

from fzf import FZF

"""
~/streamers.csv
https://www.twitch.tv/xqc,xqc
https://www.douyu.com/9999,yyf
"""


client = httpx.AsyncClient()
http_proxy = "http://127.0.0.1:10809"


class TwitchAPI:
    CLIENT_ID = "kimne78kx3ncx6brgo4mv6wki5h1ko"

    def __init__(self) -> None:
        proxies = {
            "all://": http_proxy,
        }
        self.headers = {
            "Client-ID": self.CLIENT_ID,
        }
        self.client = httpx.AsyncClient(proxies=proxies, headers=self.headers)

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
        return resp[0]["data"]["user"]["stream"] != None


async def is_online(url: str) -> bool:
    room_id = url.split("/")[-1]
    twitch = TwitchAPI()
    if "douyu.com" in url:
        resp = await client.get(f"https://open.douyucdn.cn/api/RoomApi/room/{room_id}")
        return resp.json()["data"]["room_status"] == 1
    elif "bilibili.com" in url:
        resp = await client.get(
            f"https://api.live.bilibili.com/room/v1/Room/room_init?id={room_id}"
        )
        return resp.json()["data"]["live_status"] == 1
    elif "huya.com" in url:
        resp = await client.get(url)
        return 'var TT_ROOM_DATA = {"type":"NORMAL","state":"ON"' in resp.text
    elif "twitch.tv" in url:
        return await twitch.is_online(room_id)

    return True


async def get_streamer_urls(filter_online):
    streamer_urls: dict[str, str] = {}
    with open(os.path.expanduser("~/streamers.csv")) as fp:
        for line in fp:
            url, streamer = line.strip().split(",")
            streamer_urls[streamer] = url

    if filter_online:
        keys = list(streamer_urls.keys())

        tasks = [is_online(streamer_urls[k]) for k in keys]

        result = await asyncio.gather(*tasks)

        online = dict(zip(keys, result))
        streamer_urls = {k: v for k, v in streamer_urls.items() if online[k]}

    return streamer_urls


async def main():
    parser = argparse.ArgumentParser(prog="OpenLivestream")
    parser.add_argument("-o", "--filter-online", action="store_true")
    parser.add_argument("-b", "--open-in-browser", action="store_true")
    parser.add_argument("url", nargs="?")
    args = parser.parse_args()

    if args.url is None:
        streamer_urls = await get_streamer_urls(args.filter_online)
        fzf = FZF()
        fzf.input = list(streamer_urls.keys())
        streamer = fzf.prompt()
        url = streamer_urls[streamer]
    else:
        url = args.url

    if args.open_in_browser:
        webbrowser.open(url)
    else:
        streamlink_cmd = [
            "python.exe",
            "-m",
            "streamlink",
            url,
            "best",
            "--player",
            "mpv",
        ]
        if "twitch" in url:
            streamlink_cmd.extend(["--http-proxy", http_proxy, "--twitch-disable-ads"])
        subprocess.Popen(streamlink_cmd, stdout=subprocess.DEVNULL)

        danmu_cmd = ["danmu.CMD", url]

        try:
            subprocess.call(danmu_cmd)
        except:
            pass


if __name__ == "__main__":
    asyncio.run(main())
