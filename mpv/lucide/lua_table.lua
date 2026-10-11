#!/usr/bin/env lua

function print_lua_table(tbl, name)
    name = name or "local icons"
    print(name .. " = {")

    for key, value in pairs(tbl) do
        print(string.format('    %s = "%s",', key, value))
    end

    print("}")
end

symbols = {
    subtitle = "\u{E68A}",
    window_minimize = "\u{E11C}",
    pause = "\u{E12E}",
    play = "\u{E13C}",
    audio = "\u{E142}",
    replay = "\u{E148}",
    volume_low = "\u{E1AA}",
    volume_high = "\u{E1AB}",
    volume_mute = "\u{E626}",
    volume_quiet = "\u{E1A9}",
    window_close = "\u{0078}",
    window_maximize = "\u{E167}",
    playlist = "\u{E2E2}"
}

result = {}

for button, glyph in pairs(symbols) do
    output = ""
    for i = 1, #glyph do
        output = output .. "\\" .. string.byte(glyph, i)
    end
    result[button] = output
end

print(print_lua_table(result))
