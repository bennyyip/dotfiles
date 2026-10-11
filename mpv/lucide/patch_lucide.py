#!/usr/bin/env ffpython
import fontforge

def to_lua_string(hex_string):
    ret = hex_string.replace("U+", "\\u{")
    ret += "}"
    return ret


button_to_glyphname = {
    "window_maximize": "square",
    # "window_unmaximize": "square",
    "window_minimize": "minus",
    "window_close": "x",
    "play": "play",
    "pause": "pause",
    "replay": "rotate-ccw",
    "audio": "radio",
    "playlist": "list-video",
    "subtitle": "closed-caption",
    "volume_mute": "volume-off",
    "volume_quiet": "volume",
    "volume_low": "volume-1",
    "volume_high": "volume-2",
}

glyphname_to_button = {v: k for k, v in button_to_glyphname.items()}


font = fontforge.open("lucide.ttf")

keep_icons = [x for x in button_to_glyphname.values()]

for glyph in font.glyphs():
    if glyph.glyphname not in keep_icons:
        font.removeGlyph(glyph)

button_to_codepoint = {}
for glyph in font.glyphs():
    if glyph.glyphname in keep_icons:
        button_to_codepoint[glyphname_to_button[glyph.glyphname]] = to_lua_string(
            glyph.codepoint
        )

for k, v in button_to_codepoint.items():
    print(f'{k} = "{v}",')
# print({k for k in button_to_glyphname if k not in button_to_codepoint})

# Export
font.generate("lucide-subset.ttf")
