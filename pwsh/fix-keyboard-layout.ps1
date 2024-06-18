$langlist = Get-WinUserLanguageList

$WarningPreference = 'SilentlyContinue'
for ($i = 0; $i -le $langlist.count; $i++) {
    if ($langlist[$i].LanguageTag -eq "en-US") {
        $langList[0].InputMethodTips.Clear()
        $langList[0].InputMethodTips.Add('0409:A0000409') # eurkey
        $langList[0].InputMethodTips.Add('0409:00000409') # US
        Set-WinUserLanguageList -Force $langList
        $langList[0].InputMethodTips.Clear()
        $langList[0].InputMethodTips.Add('0409:A0000409') # eurkey
        Set-WinUserLanguageList -Force $langList
    }
}
