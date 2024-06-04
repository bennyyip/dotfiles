import httpx
import time
from pathlib import Path

from xonsh.tools import unthreadable
from xonsh.platform import ON_WINDOWS



def get_replay_path(match_id):
    dem_file = Path(f'C:/Program Files (x86)/Steam/steamapps/common/dota 2 beta/game/dota/replays/{match_id}.dem')
    return dem_file

def delete_replay(match_id):
    get_replay_path(match_id).unlink()

@aliases.register("download_replay")
def __download_replay(args):
    proxy = ${...}.get('MY_PROXY')

    retries = 5

    match_ids = args

    for i in range(retries):
        if i > 0:
            print('retrying', i)
        failed = dota2_download_replay_batch(match_ids, proxy)
        if len(failed) == 0:
            break
        print('failed replays:', ' '.join(failed))
        match_ids = failed
        time.sleep(2 ** (i + 1))


    if len(failed) > 0:
        print('failed replays:', ' '.join(failed))

def dota2_download_replay_batch(match_ids, proxy=None):
    failed = []

    for match_id in match_ids:
        if not dota2_download_replay(match_id, proxy):
            failed.append(match_id)

    return failed

def dota2_download_replay(match_id, proxy=None):
    dem_file = get_replay_path(match_id)

    if dem_file.exists() and dem_file.stat().st_size > 10000000:
        print(f'replay {match_id} exists.')
        return True

    odota_api_base = 'https://api.opendota.com/api'
    odota_url = f'{odota_api_base}/matches/{match_id}'
    odota_response = httpx.get(odota_url, proxy=proxy).json()
    replay_url = odota_response.get('replay_url')
    if replay_url is None:
        # print(f'replay {match_id} not found.')

        requst_replay_url = f'{odota_api_base}/request/{match_id}'
        # print(requst_replay_url)
        job_id = (httpx.post(requst_replay_url).json()['job']['jobId'])

        # print(f'request job url {odota_api_base}/request/{job_id}')

        return False

    print(replay_url)
    bz2data = progressed_download(replay_url, proxy=proxy)
    d = bz2.decompress(bz2data)
    with open(dem_file, 'wb') as f:
        f.write(d)
    print (f'{match_id} done.')

    return True

def to_steam_id(dota_id):
    return dota_id + 76561197960265728

def to_dota_id(steam_id):
    return steam_id - 76561197960265728


@aliases.register("st")
def __stratz(args):
    for player_id in args:
        webbrowser.open(f'https://stratz.com/players/{player_id}')

if ON_WINDOWS:
    import pytesseract
    import win32gui
    from PIL import ImageGrab
    import re
    # winget install Tesseract-OCR
    pytesseract.pytesseract.tesseract_cmd = r"C:\Program Files\Tesseract-OCR\tesseract.exe"

    @aliases.register("stocr")
    def __stocr(args):
        hwnd = win32gui.FindWindow("SDL_app", "Dota 2")

        win32gui.SetForegroundWindow(hwnd)
        while win32gui.GetForegroundWindow() != hwnd:
            time.sleep(0.05)

        bbox = (1350, 370, 1650, 410)
        img = ImageGrab.grab(bbox)
        ocr_result = pytesseract.image_to_string(img)
        try:
            __stratz([re.search(r"\d\d\d\d\d+", ocr_result)[0]])
        except:
            print('No valid player id.')
