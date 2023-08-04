import os
import re
import shutil

from utils import edit_file

ifile = "C:\\Users\\bennyyip\\Dropbox\\rime-sync\\Windows-Desktop\\my_dict.userdb.txt"
ofile = os.path.expanduser("~/vmwaresharefolders/ben.pyim")
shutil.copyfile(ifile, ofile)


is_begining = True
for line in edit_file(ofile):
    if line.startswith("#"):
        continue
    if is_begining:
        print(";; -*- coding: utf-8 -*--")
        is_begining = False
    parts = line.split("\t")
    if re.match(r"^[a-zA-Z\s]+$", parts[1]):
        continue
    newline = parts[0].strip().replace(" ", "-") + " " + parts[1]
    print(newline)
