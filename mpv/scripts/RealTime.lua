-- http://www.tcax.org/docs/ass-specs.htm
-- https://aegisub.org/docs/latest/ass_tags/

local ov = mp.create_osd_overlay("ass-events")

mp.add_periodic_timer(1, function()
    display_time = os.date("%H:%M")
    -- display_time = os.date("%H:%M:%S")
    ov.data =
        "{\\fnLXGW WenKai Mono TC}{\\fs30}{\\an7}{\\b1}{\\1c&H9C9078}{\\2c&H000000}{\\3c&H000000}{\\4c&H000000}{\\1a&H80}{\\2a&H80}{\\3a&HD0}{\\4a&H80}" ..
            display_time
    ov:update()
end)
