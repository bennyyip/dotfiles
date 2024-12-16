-- dedukun/relative-motions [[[1
require("relative-motions"):setup({ show_numbers="relative", show_motion = true })

-- h-hg/yamb.yazi [[[1
local bookmarks = {}

local path_sep = package.config:sub(1, 1)
local home_path = ya.target_family() == "windows" and os.getenv("USERPROFILE") or os.getenv("HOME")
table.insert(bookmarks, {
    tag = "Dotfiles",
    path = home_path .. path_sep .. "dotfiles" .. path_sep,
    key = "d"
})
-- if ya.target_family() == "windows" then
-- end

require("yamb"):setup {
    -- Optional, the path ending with path seperator represents folder.
    bookmarks = bookmarks,
    -- Optional, recieve notification everytime you jump.
    jump_notify = true,
    -- Optional, the cli of fzf.
    cli = "fzf",
    -- Optional, a string used for randomly generating keys, where the preceding characters have higher priority.
    keys = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    -- Optional, the path of bookmarks
    path = (ya.target_family() == "windows" and os.getenv("APPDATA") .. "\\yazi\\config\\bookmark") or
    (os.getenv("HOME") .. "/.config/yazi/bookmark"),
}
-- ]]]
-- vim:fdm=marker:fmr=[[[,]]]