import httpx
import time
from pathlib import Path
import argparse
import bz2

import rich.progress

PROXY = "socks5://127.0.0.1:10808"


def progressed_download(url, **kwargs):
    ret = b""
    with httpx.stream("GET", url, **kwargs) as response:
        total = int(response.headers["Content-Length"])

        with rich.progress.Progress(
            "[progress.percentage]{task.percentage:>3.0f}%",
            rich.progress.BarColumn(bar_width=None),
            rich.progress.DownloadColumn(),
            rich.progress.TransferSpeedColumn(),
        ) as progress:
            download_task = progress.add_task("Download", total=total)
            for chunk in response.iter_bytes():
                ret += chunk
                progress.update(download_task, completed=response.num_bytes_downloaded)
        return ret


def get_replay_path(match_id):
    dem_file = Path(
        f"C:/Program Files (x86)/Steam/steamapps/common/dota 2 beta/game/dota/replays/{match_id}.dem"
    )
    return dem_file


def delete_replay(match_id):
    get_replay_path(match_id).unlink()


def download_replay(match_ids):

    retries = 5

    for i in range(retries):
        if i > 0:
            print("retrying", i)
        failed = dota2_download_replay_batch(match_ids)
        if len(failed) == 0:
            break
        print("failed replays:", " ".join(failed))
        match_ids = failed
        time.sleep(2 ** (i + 1))

    if len(failed) > 0:
        print("failed replays:", " ".join(failed))


def dota2_download_replay_batch(match_ids, proxy=PROXY):
    failed = []

    for match_id in match_ids:
        if not dota2_download_replay(match_id, proxy):
            failed.append(match_id)

    return failed


def dota2_download_replay(match_id, proxy=None):
    dem_file = get_replay_path(match_id)

    if dem_file.exists() and dem_file.stat().st_size > 10000000:
        print(f"replay {match_id} exists.")
        return True

    odota_api_base = "https://api.opendota.com/api"
    odota_url = f"{odota_api_base}/matches/{match_id}"
    odota_response = httpx.get(odota_url, proxy=proxy).json()
    replay_url = odota_response.get("replay_url")
    if replay_url is None:
        requst_replay_url = f"{odota_api_base}/request/{match_id}"
        job_id = httpx.post(requst_replay_url).json()["job"]["jobId"]

        return False

    print(replay_url)
    bz2data = progressed_download(replay_url, proxy=proxy)
    d = bz2.decompress(bz2data)
    with open(dem_file, "wb") as f:
        f.write(d)
    print(f"{match_id} done.")

    return True


def to_steam_id(dota_id):
    return dota_id + 76561197960265728


def to_dota_id(steam_id):
    return steam_id - 76561197960265728


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    # subparsers = parser.add_subparsers()
    # download = subparsers.add_parser('download')
    parser.add_argument("replay_id", nargs="+")
    args = parser.parse_args()
    download_replay(args.replay_id)
