local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
local themes_path = os.getenv("HOME") .. "/.config/awesome/themes/default"

local theme = {}

theme.font = "Roboto Regular 10"

-- Base16 Theme
theme.base00 = "#282c34"
theme.base01 = "#353b45"
theme.base02 = "#3e4451"
theme.base03 = "#545862"
theme.base04 = "#565c64"
theme.base05 = "#abb2bf"
theme.base06 = "#b6bdca"
theme.base07 = "#c8ccd4"
theme.base08 = "#e06c75"
theme.base09 = "#d19a66"
theme.base0A = "#e5c07b"
theme.base0B = "#98c379"
theme.base0C = "#56b6c2"
theme.base0D = "#61afef"
theme.base0E = "#c678dd"
theme.base0F = "#be5046"

theme.bg_normal     = theme.base00
theme.bg_focus      = theme.base01
theme.bg_urgent     = theme.base09
theme.bg_minimize   = theme.bg_normal
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = theme.base04
theme.fg_focus      = theme.base05
theme.fg_urgent     = theme.base05
theme.fg_minimize   = theme.base05

theme.useless_gap   = dpi(0)
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
theme.hotkeys_group_margin = dpi(25)

theme.taglist_bg_focus = theme.base00
theme.taglist_bg_urgent = theme.base02

theme.tasklist_disable_icon = true
theme.tasklist_bg = theme.base00
theme.tasklist_bg_focus = theme.base00

theme.wibar_height = 35

-- Generate taglist squares:
-- theme.taglist_squares_sel = themes_path .. "/taglist/tag-sel.png"
-- theme.taglist_squares_unsel = themes_path .. "/taglist/tag-unsel.png"

-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]
theme.notification_bg = theme.base02
theme.notification_fg = theme.base05
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
theme.titlebar_font = "Roboto Condensed Regular 9"
theme.tasklist_item_width = 9999
theme.tags = {
   {
      empty_icon = themes_path .. "/taglist/1_empty.png",
      focused_icon = themes_path .. "/taglist/1_focused.png",
      occupied_icon = themes_path .. "/taglist/1_occupied.png",
      urgent_icon = themes_path .. "/taglist/1_urgent.png"
   },
   {
      empty_icon = themes_path .. "/taglist/4_empty.png",
      focused_icon = themes_path .. "/taglist/4_focused.png",
      occupied_icon = themes_path .. "/taglist/4_occupied.png",
      urgent_icon = themes_path .. "/taglist/4_urgent.png"
   },
   {
      empty_icon = themes_path .. "/taglist/2_empty.png",
      focused_icon = themes_path .. "/taglist/2_focused.png",
      occupied_icon = themes_path .. "/taglist/2_occupied.png",
      urgent_icon = themes_path .. "/taglist/2_urgent.png"
   },
   {
      empty_icon = themes_path .. "/taglist/3_empty.png",
      focused_icon = themes_path .. "/taglist/3_focused.png",
      occupied_icon = themes_path .. "/taglist/3_occupied.png",
      urgent_icon = themes_path .. "/taglist/3_urgent.png"
   },
   {
      empty_icon = themes_path .. "/taglist/10_empty.png",
      focused_icon = themes_path .. "/taglist/10_focused.png",
      occupied_icon = themes_path .. "/taglist/10_occupied.png",
      urgent_icon = themes_path .. "/taglist/10_urgent.png"
   },
}

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

-- theme.wallpaper = themes_path .. "/background.png"

-- You can use your own layout icons like this:
theme.layout_floating  = themes_path .. "/layouts/floatingw.png"
theme.layout_tile = themes_path .. "/layouts/tilew.png"
theme.layout_max = themes_path .. "/layouts/maxw.png"
--[[
theme.layout_fairh = themes_path .. "/layouts/fairhw.png"
theme.layout_fairv = themes_path .. "/layouts/fairvw.png"
theme.layout_magnifier = themes_path .. "/layouts/magnifierw.png"
theme.layout_fullscreen = themes_path .. "/layouts/fullscreenw.png"
theme.layout_tilebottom = themes_path .. "/layouts/tilebottomw.png"
theme.layout_tileleft   = themes_path .. "/layouts/tileleftw.png"
theme.layout_tiletop = themes_path .. "/layouts/tiletopw.png"
theme.layout_spiral  = themes_path .. "/layouts/spiralw.png"
theme.layout_dwindle = themes_path .. "/layouts/dwindlew.png"
theme.layout_cornernw = themes_path .. "/layouts/cornernww.png"
theme.layout_cornerne = themes_path .. "/layouts/cornernew.png"
theme.layout_cornersw = themes_path .. "/layouts/cornersww.png"
theme.layout_cornerse = themes_path .. "/layouts/cornersew.png"
--]]

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = "/usr/share/icons/Paper"

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
