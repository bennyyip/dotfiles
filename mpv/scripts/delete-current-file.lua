local input = require("mp.input")

local function is_file(path)
    return type(path) == "string" and path ~= '-' and path:find("^%a[%w.+-]-://") == nil and path:find("^%a[%w.+-]-:%?") == nil
end

local function do_delete()
    local path = mp.get_property("path")

    if not is_file(path) then
        mp.osd_message("Current playing is not a file.")
        return
    end
    error_message = select(2, os.remove(path))
    if error_message then
        mp.msg.error(message)
        mp.osd_message(message)
    else
        mp.osd_message("File deleted.")
    end

    mp.commandv("playlist-next")
end

local function confirm()
    input.select({
        prompt = "Are you sure you want to delete current file?",
        items = { "No", "Yes" },
        submit = function(j)
            if j == 2 then
                do_delete()
            end
        end,
    })
end


mp.add_key_binding("Shift+DEL", "delete-current-file-no-confirm", do_delete)
mp.add_key_binding("DEL", "delete-current-file", confirm)
