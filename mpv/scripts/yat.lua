mp.utils = require "mp.utils"

mp.observe_property("idle-active", "bool", function(_, v)
    if v then
        mp.commandv("script-binding", "select/select-watch-history")
    end
end)


function dedup_history()
    function dedup(lines)
        local seen = {}
        local ret = {}
        for i = #lines, 1, -1 do
            local line = lines[i]
            if line:match("\\\\BT\\\\A\\\\") then
                goto continue
            end
            local path = line:sub(26)
            if not seen[path] then
                ret[#ret+1] = line
                seen[path] = true
            end
            ::continue::
        end
        return ret
    end

    local path = mp.command_native({"expand-path", mp.get_property("watch-history-path")})

    local lines = {}

    local infile, err = io.open(path, "r")
    if not infile then
        mp.msg.error("open read failed: "..(err or "unknown"))
        return
    end

    for line in infile:lines() do
        lines[#lines+1] = line
    end
    infile:close()

    lines = dedup(lines)

    local tmp_path = path .. ".tmp"
    local outfile, err2 = io.open(tmp_path, "w")
    if not outfile then
        infile:close()
        mp.msg.error("open write failed: "..(err2 or "unknown"))
        return
    end

    for i = #lines, 1, -1 do
        outfile:write(lines[i], "\n")
    end

    outfile:close()

    local ok, renerr = os.remove(path) and os.rename(tmp_path, path) or os.rename(tmp_path, path)
    if not ok then
        mp.msg.error("replace failed: "..(renerr or "unknown"))
    end
end

dedup_history()
