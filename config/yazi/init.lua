-- dedukun/relative-motions [[[1
require("relative-motions"):setup({ show_numbers="relative", show_motion = true })
-- linemode [[[1
function Linemode:size_and_mtime()
    local time = math.floor(self._file.cha.mtime or 0)
    if time == 0 then
        time = ""
    elseif os.date("%Y", time) == os.date("%Y") then
        time = os.date("%m-%d %H:%M", time)
    else
        time = os.date("%Y  %m %d", time)
    end

    local size = self._file:size()
    return string.format("%s %s", size and ya.readable_size(size) or "", time)
end

-- ]]]
-- vim:fdm=marker:fmr=[[[,]]]
