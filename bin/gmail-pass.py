#!/usr/bin/env python3

import os
from getpass import getpass
from pathlib import Path

from pykeepass import PyKeePass


def get_from_keepass():
    password = getpass()
    kp = PyKeePass(Path("~/keepass.kdbx").expanduser(), password=password)
    entry = kp.find_entries_by_title("gmail-keepass", first=True)
    return entry.password


def get_pass():
    ret = os.environ.get("GMAIL_PASS")
    return ret if ret is not None else get_from_keepass()


if __name__ == "__main__":
    print(get_pass(), end="")
