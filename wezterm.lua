-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
    config = wezterm.config_builder()
end

config.color_scheme = 'GruvboxDark'
-- config.color_scheme = 'Gruvbox Dark (Gogh)'
-- config.color_scheme = 'flexoki-dark'

if wezterm.target_triple == "x86_64-pc-windows-msvc" then
    -- config.default_prog = { 'pwsh', '--nologo', '-Command', 'ssh arch' }
    config.default_prog = { 'xonsh' }
    config.font_size = 14.0
else
    config.font_size = 18.0
end

config.hide_tab_bar_if_only_one_tab = true

-- AlwaysPrompt|NeverPrompt
config.window_close_confirmation = 'NeverPrompt'

config.font = wezterm.font 'Sarasa Term CL Nerd'
-- config.freetype_load_target = "Light"
-- config.freetype_render_target = "HorizontalLcd"

config.window_padding = {
    top    = "0px",
    left   = "0px",
    right  = "0px",
    bottom = "0px",
}

config.disable_default_key_bindings = true
config.keys = {
    {mods="CTRL|SHIFT", key="F", action=wezterm.action{Search={CaseSensitiveString=""}}},
    {mods="CTRL|SHIFT", key="C", action=wezterm.action{CopyTo="Clipboard"}},
    {mods="CTRL|SHIFT", key="V", action=wezterm.action{PasteFrom="Clipboard"}},
    {mods="SHIFT", key="Insert", action=wezterm.action{PasteFrom="PrimarySelection"}},
    {mods="CTRL", key="-", action="DecreaseFontSize"},
    {mods="CTRL", key="=", action="IncreaseFontSize"},
    {mods="CTRL", key="0", action="ResetFontSize"},
    {key="F11", action="ToggleFullScreen"},
}

config.initial_cols = 130
config.initial_rows = 40

config.default_cursor_style = 'BlinkingBar'
config.animation_fps = 60
config.cursor_blink_ease_in = 'Constant'
config.cursor_blink_ease_out = 'Constant'

return config

