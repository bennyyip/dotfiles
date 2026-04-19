--makes mpv disable ontop when pausing and re-enable it again when resuming playback
--please note that this won't do anything if ontop was not enabled before pausing

local was_ontop = false
local disabled = false

local script_name = mp.get_script_name()

local function handler(name, value)
    local ontop = mp.get_property_native("ontop")
    if value then
        if ontop then
            mp.set_property_native("ontop", false)
            was_ontop = true
        end
    else
        if was_ontop and not ontop then
            mp.set_property_native("ontop", true)
        end
        was_ontop = false
    end
end

local function toggle()
    disabled = not disabled

    if disabled then
        mp.unobserve_property(handler)
        mp.osd_message(script_name .. ": disable")
    else
        mp.observe_property("pause", "bool", handler)
        mp.osd_message(script_name .. ": enable")
    end
end

mp.observe_property("pause", "bool", handler)
mp.add_key_binding("T", "toggle", toggle)
