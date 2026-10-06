local function check_bili(path)
    return path ~= nil and path:match("^https://www%.bilibili%.com/video/")
end

local function parse_params(url)
    local params = {}

    local query = url:match("%?([^#]*)")
    if not query then
        return params
    end

    for key, value in query:gmatch("([^&=]+)=?([^&]*)") do
        params[key] = value
    end

    return params
end

local function build_url(url, params)
    local base = url:gsub("%?.*$", "")

    local query_parts = {}
    for key, value in pairs(params) do
        table.insert(query_parts, key .. "=" .. value)
    end

    if #query_parts > 0 then
        return base .. "?" .. table.concat(query_parts, "&")
    else
        return base
    end
end

local function next_p()
    local path = mp.get_property("path")
    if check_bili(path) then
        local params = parse_params(path)
        local p = tonumber(params.p) or 1

        params.p = p + 1
        local new_url = build_url(path, params)

        mp.commandv("loadfile", new_url, "replace")
    else
        mp.commandv("playlist-next")
    end
end

local function prev_p()
    local path = mp.get_property("path")
    if check_bili(path) then
        local params = parse_params(path)
        local p = tonumber(params.p) or 1

        if p > 1 then
            params.p = p - 1
            local new_url = build_url(path, params)

            mp.commandv("loadfile", new_url, "replace")
        end
    else
        mp.commandv("playlist-prev")
    end
end

mp.add_key_binding("PGDWN", "next-p", next_p)
mp.add_key_binding("PGUP", "prev-p", prev_p)
