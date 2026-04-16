local utils = require "mp.utils"

-- Check if path is a protocol, such as `http://...`.
---@param path string
function is_protocol(path)
    return type(path) == 'string' and (path:find('^%a[%w.+-]-://') ~= nil or path:find('^%a[%w.+-]-:%?') ~= nil)
end

local platform = mp.get_property_native('platform')

mp.add_key_binding('CTRL+ENTER', 'show-in-directory', function()
    -- Ignore URLs
    local path = mp.get_property('path')

    if not path or is_protocol(path) then return end

    if platform == 'windows' then
        utils.subprocess_detached({args = {'explorer', '/select,', path .. ' '}, cancellable = false})
    elseif platform == 'darwin' then
        utils.subprocess_detached({args = {'open', '-R', path}, cancellable = false})
    elseif platform == 'linux' then
        local result = utils.subprocess({args = {'nautilus', path}, cancellable = false})

        -- Fallback opens the folder with xdg-open instead
        if result.status ~= 0 then
            utils.subprocess({args = {'xdg-open', serialize_path(path).dirname}, cancellable = false})
        end
    end
end)
