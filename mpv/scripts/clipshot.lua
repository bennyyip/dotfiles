---https://github.com/ObserverOfTime/mpv-scripts/blob/master/clipshot.lua
---Screenshot the video and copy it to the clipboard
---@author ObserverOfTime
---@license 0BSD

---@class ClipshotOptions
---@field name string
---@field type string
local utils = require 'mp.utils'
local o = {
    name = 'mpv-screenshot.png',
    type = 'image/png' -- defaults to jpeg
}
require('mp.options').read_options(o, 'clipshot')

local file, cmd

local platform = mp.get_property_native('platform')
if platform == 'windows' then
    file = os.getenv('TEMP')..'\\'..o.name
    cmd = {
        'powershell', '-NoProfile', '-Command',
        'Add-Type -Assembly System.Windows.Forms, System.Drawing;',
        string.format(
        "[Windows.Forms.Clipboard]::SetImage([Drawing.Image]::FromFile('%s'))",
        file:gsub("'", "''")
        )
    }
elseif platform == 'darwin' then
    file = os.getenv('TMPDIR')..'/'..o.name
    -- png: «class PNGf»
    local type = o.type ~= '' and o.type or 'JPEG picture'
    cmd = {
        'osascript', '-e', string.format(
        'set the clipboard to (read (POSIX file %q) as %s)',
        file, type
        )
    }
else
    file = '/tmp/'..o.name
    if os.getenv('XDG_SESSION_TYPE') == 'wayland' then
        cmd = {'sh', '-c', ('wl-copy < %q'):format(file)}
    else
        local type = o.type ~= '' and o.type or 'image/jpeg'
        cmd = {'xclip', '-sel', 'c', '-t', type, '-i', file}
    end
end

local function copyTime()
    local function divmod (a, b)
        return math.floor(a / b), a % b
    end
    local time_pos = mp.get_property_number("time-pos")
    local minutes, remainder = divmod(time_pos, 60)
    local hours, minutes = divmod(minutes, 60)
    local seconds = math.floor(remainder)
    -- local milliseconds = math.floor((remainder - seconds) * 1000)
    -- local time = string.format("%02d:%02d:%02d.%03d", hours, minutes, seconds, milliseconds)
    local time = string.format("%02d:%02d:%02d", hours, minutes, seconds)
    mp.set_property('clipboard/text', time)
    mp.osd_message(time)
end

---@param arg string
---@return fun()
local function clipshot(arg)
    return function()
        copyTime()
        mp.commandv('screenshot-to-file', file, arg)
        mp.command_native_async({'run', unpack(cmd)}, function(suc, _, err)
            mp.osd_message(suc and 'Copied screenshot to clipboard' or err, 1)
        end)
    end
end

-- mp.add_key_binding('c',     'clipshot-subs',   clipshot('subtitles'))
-- mp.add_key_binding('C',     'clipshot-video',  clipshot('video'))
-- mp.add_key_binding('Alt+c', 'clipshot-window', clipshot('window'))

mp.add_key_binding('Y',     'clipshot-video',  clipshot('video'))
mp.add_key_binding('p',     'clipshot-time',  copyTime)
