# https://redlib.pussthecat.org/r/Windows11/comments/vm046d/is_there_any_way_to_remove_the_xbox_game_bar/
## AveYo: fix ms-gamebar annoyance after uninstalling Xbox
reg add HKCR\ms-gamebar /f /ve /d URL:ms-gamebar
reg add HKCR\ms-gamebar /f /v "URL Protocol" /d ""
reg add HKCR\ms-gamebar /f /v "NoOpenWith" /d ""
reg add HKCR\ms-gamebar\shell\open\command /f /ve /d "\`"$env:SystemRoot\System32\systray.exe\`""
reg add HKCR\ms-gamebarservices /f /ve /d URL:ms-gamebarservices
reg add HKCR\ms-gamebarservices /f /v "URL Protocol" /d ""
reg add HKCR\ms-gamebarservices /f /v "NoOpenWith" /d ""
reg add HKCR\ms-gamebarservices\shell\open\command /f /ve /d "\`"$env:SystemRoot\System32\systray.exe\`""
