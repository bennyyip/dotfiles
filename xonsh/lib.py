import bz2

import rich.progress


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
