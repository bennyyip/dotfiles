local translate = true

local function copy_subtitle(prop)
    local result = mp.get_property(prop)

    if result ~= '' and result ~= nil then
        result = result:gsub("\n", " ")
        mp.set_property('clipboard/text', result)
        if translate then
            mp.osd_message('translate')
            mp.command_native_async({
                'run', 'cmd.exe', '/c', 'start', '', '/b', string.format("goldendict://%s", result)
            })
        end
    else
        mp.osd_message('No `' .. prop  .. '` to copy')
    end
end

local function toggle_translate()
    if translate then
        translate = false
        mp.osd_message('Translate disabled.')
    else
        translate = true
        mp.osd_message('Translate enabled.')
    end
end

local function copy_path()
    local ret = mp.get_property('filename')

    if ret:match("https://") then
        local time_pos = mp.get_property('time-pos')
        if ret:match('twitter') or ret:match('bilibili.com') then
            ret = ret .. '?t=' .. time_pos
        elseif ret:match('youtube') then
            ret = ret .. '?t=' .. time_pos .. 's'
        end
    end

    mp.osd_message('Copied:\n' .. ret)
    mp.set_property('clipboard/text', ret)
end


mp.add_key_binding('Ctrl+c', 'copy-subtitle-primary', function() copy_subtitle('sub-text') end)
mp.add_key_binding('Alt+c', 'copy-subtitle-secondary', function() copy_subtitle('secondary-sub-text') end)
mp.add_key_binding('Ctrl+Shift+c', "toggle-translate", toggle_translate)
mp.add_key_binding('y', 'copy-path', copy_path)
