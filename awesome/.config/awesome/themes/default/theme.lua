local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
local themes_path = os.getenv("HOME") .. "/.config/awesome/themes/default"

local theme = {}

theme.font = "Roboto Regular 10"

-- Base16 Tomorrow Night
theme.base00 = "#1d1f21"
theme.base01 = "#282a2e"
theme.base02 = "#373b41"
theme.base03 = "#969896"
theme.base04 = "#b4b7b4"
theme.base05 = "#c5c8c6"
theme.base06 = "#e0e0e0"
theme.base07 = "#ffffff"
theme.base08 = "#cc6666"
theme.base09 = "#de935f"
theme.base0A = "#f0c674"
theme.base0B = "#b5bd68"
theme.base0C = "#8abeb7"
theme.base0D = "#81a2be"
theme.base0E = "#b294bb"
theme.base0F = "#a3685a"

theme.bg_normal     = theme.base00
theme.bg_focus      = theme.base01
theme.bg_urgent     = theme.base09
theme.bg_minimize   = theme.bg_normal
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = theme.base03
theme.fg_focus      = theme.base04
theme.fg_urgent     = theme.base04
theme.fg_minimize   = theme.base04

theme.useless_gap   = dpi(8)
theme.border_width  = dpi(0)
theme.border_color  = theme.base01
theme.border_normal = theme.base01
theme.border_focus  = theme.base0D

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- Example:
--theme.taglist_bg_focus = "#ff0000"
theme.hotkeys_bg = theme.base00
theme.hotkeys_fg = theme.base05
theme.hotkeys_modifiers_fg = theme.base03
theme.hotkeys_label_bg = theme.base0D
theme.hotkeys_label_fg = theme.base00
theme.hotkeys_group_margin = dpi(36)

theme.tasklist_disable_icon = true

-- Generate taglist squares:
local taglist_square_size = dpi(8)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)

-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]
theme.notification_bg = theme.base02
theme.notification_fg = theme.base04
theme.notification_icon_size = dpi(48)
theme.notification_padding = 32
theme.notification_spacing = 16
theme.notification_border_width = dpi(0)

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path .. "/submenu.png"
theme.menu_height = dpi(32)
theme.menu_width  = dpi(200)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
theme.titlebar_size = dpi(35)
theme.tags_empty = {"","","","","","","","",""}
theme.tags_occupied = {"","","","","","","","","",}
theme.tags_focused = {"","","","","","","","","",}

-- Define the image to load
theme.titlebar_close_button_normal = themes_path .. "/titlebar/close_normal.svg"
theme.titlebar_close_button_focus  = themes_path .. "/titlebar/close_focus.svg"

theme.titlebar_minimize_button_normal = themes_path .. "/titlebar/minimize_normal.svg"
theme.titlebar_minimize_button_focus  = themes_path .. "/titlebar/minimize_focus.svg"

theme.titlebar_ontop_button_normal_inactive = themes_path .. "/titlebar/ontop_normal_inactive.svg"
theme.titlebar_ontop_button_focus_inactive  = themes_path .. "/titlebar/ontop_focus_inactive.svg"
theme.titlebar_ontop_button_normal_active = themes_path .. "/titlebar/ontop_normal_active.svg"
theme.titlebar_ontop_button_focus_active  = themes_path .. "/titlebar/ontop_focus_active.svg"

theme.titlebar_sticky_button_normal_inactive = themes_path .. "/titlebar/sticky_normal_inactive.svg"
theme.titlebar_sticky_button_focus_inactive  = themes_path .. "/titlebar/sticky_focus_inactive.svg"
theme.titlebar_sticky_button_normal_active = themes_path .. "/titlebar/sticky_normal_active.svg"
theme.titlebar_sticky_button_focus_active  = themes_path .. "/titlebar/sticky_focus_active.svg"

theme.titlebar_floating_button_normal_inactive = themes_path .. "/titlebar/floating_normal_inactive.svg"
theme.titlebar_floating_button_focus_inactive  = themes_path .. "/titlebar/floating_focus_inactive.svg"
theme.titlebar_floating_button_normal_active = themes_path .. "/titlebar/floating_normal_active.svg"
theme.titlebar_floating_button_focus_active  = themes_path .. "/titlebar/floating_focus_active.svg"

theme.titlebar_maximized_button_normal_inactive = themes_path .. "/titlebar/maximized_normal_inactive.svg"
theme.titlebar_maximized_button_focus_inactive  = themes_path .. "/titlebar/maximized_focus_inactive.svg"
theme.titlebar_maximized_button_normal_active = themes_path .. "/titlebar/maximized_normal_active.svg"
theme.titlebar_maximized_button_focus_active  = themes_path .. "/titlebar/maximized_focus_active.svg"

theme.wallpaper = themes_path .. "/background.png"

-- You can use your own layout icons like this:
theme.layout_fairh = themes_path .. "/layouts/fairhw.png"
theme.layout_fairv = themes_path .. "/layouts/fairvw.png"
theme.layout_floating  = themes_path .. "/layouts/floatingw.png"
theme.layout_magnifier = themes_path .. "/layouts/magnifierw.png"
theme.layout_max = themes_path .. "/layouts/maxw.png"
theme.layout_fullscreen = themes_path .. "/layouts/fullscreenw.png"
theme.layout_tilebottom = themes_path .. "/layouts/tilebottomw.png"
theme.layout_tileleft   = themes_path .. "/layouts/tileleftw.png"
theme.layout_tile = themes_path .. "/layouts/tilew.png"
theme.layout_tiletop = themes_path .. "/layouts/tiletopw.png"
theme.layout_spiral  = themes_path .. "/layouts/spiralw.png"
theme.layout_dwindle = themes_path .. "/layouts/dwindlew.png"
theme.layout_cornernw = themes_path .. "/layouts/cornernww.png"
theme.layout_cornerne = themes_path .. "/layouts/cornernew.png"
theme.layout_cornersw = themes_path .. "/layouts/cornersww.png"
theme.layout_cornerse = themes_path .. "/layouts/cornersew.png"

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = "/usr/share/icons/Paper"

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
