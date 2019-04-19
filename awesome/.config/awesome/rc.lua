-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
-- require("awful.hotkeys_popup.keys")

-- Load Debian menu entries
local debian = require("debian.menu")
local has_fdo, freedesktop = pcall(require, "freedesktop")

local lain = require("lain")
local keys = require("keys")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
   naughty.notify({ preset = naughty.config.presets.critical,
                    title = "Oops, there were errors during startup!",
                    text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
   local in_error = false
   awesome.connect_signal(
      "debug::error",
      function (err)
         -- Make sure we don't go into an endless error loop
         if in_error then return end
         in_error = true

         naughty.notify({ preset = naughty.config.presets.critical,
                          title = "Oops, an error happened!",
                          text = tostring(err) })
         in_error = false
   end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(os.getenv("HOME") .. "/.config/awesome/themes/default/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "x-terminal-emulator"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
   awful.layout.suit.floating,
   awful.layout.suit.tile,
   awful.layout.suit.max,
   -- awful.layout.suit.tile.left,
   -- awful.layout.suit.tile.bottom,
   -- awful.layout.suit.tile.top,
   -- awful.layout.suit.fair,
   -- awful.layout.suit.fair.horizontal,
   -- awful.layout.suit.spiral,
   -- awful.layout.suit.spiral.dwindle,
   -- awful.layout.suit.max.fullscreen,
   -- awful.layout.suit.magnifier,
   -- awful.layout.suit.corner.nw,
   -- awful.layout.suit.corner.ne,
   -- awful.layout.suit.corner.sw,
   -- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Helper functions
function create_tags(tags, screen, layout)
    -- True if `layout` should be used as the layout of each created tag
    local have_single_layout = (not layout) or (layout.arrange and layout.name)
    local tag_widgets = {}

    for i, tag in ipairs(tags) do
        local l = layout

        if not have_single_layout then
            l = layout[i] or layout[1]
        end

        table.insert(
           tag_widgets,
           awful.tag.add(
              i,
              {
                 icon = tag.empty_icon,
                 icon_only = true,
                 screen = screen,
                 layout = l
              }
           )
        )
        -- Select the first tag.
        if i == 1 then
            tag_widgets[i].selected = true
        end
    end

    return tag_widgets
end

function update_tag_icons(tags)
   ---[[
   for i, tag in ipairs(tags) do
      if tag.selected then
         tag.icon = beautiful.tags[i].focused_icon
      elseif tag.urgent then
         tag.icon = beautiful.tags[i].urgent_icon
      elseif #tag:clients() > 0 then
         tag.icon = beautiful.tags[i].occupied_icon
      else
         tag.icon = beautiful.tags[i].empty_icon
      end
   end
   --]]
end
-- }}}

-- {{{ Notifications
naughty.config.defaults.timeout = 5
naughty.config.presets.low.timeout = 3
naughty.config.presets.critical.timeout = 12

naughty.config.defaults['icon_size'] = beautiful.notification_icon_size
naughty.config.padding = beautiful.notification_padding
naughty.config.spacing = beautiful.notification_spacing
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show_help end},
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end}
}

local menu_awesome = { "awesome", myawesomemenu, beautiful.awesome_icon }
local menu_terminal = { "open terminal", terminal }

if has_fdo then
   mymainmenu = freedesktop.menu.build({
         before = { menu_awesome },
         after =  { menu_terminal }
   })
else
   mymainmenu = awful.menu({
         items = {
            menu_awesome,
            { "Debian", debian.menu.Debian_menu.Debian },
            menu_terminal,
         }
   })
end

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibar
-- Battery widget
local lain_bat = lain.widget.bat {
   n_perc = {15, 25},
   full_notify = "off",
   settings = function()
      local bat_icon = ""
      local perc = bat_now.perc

      if perc < 15 then
         bat_icon = ""
      elseif perc < 40 then
         bat_icon = ""
      elseif perc < 60 then
         bat_icon = ""
      elseif perc < 80 then
         bat_icon = ""
      end

      widget:set_markup(lain.util.markup.fg.color(beautiful.base05, bat_icon .. "  " .. bat_now.perc .. "%"))
   end
}

-- Volume widget
local lain_vol = lain.widget.alsa {
   timeout = 5,
   settings = function()
      local display_text = "  mute"

      if volume_now.status == "on" then
         display_text = "  " .. volume_now.level .. "%"
      end

      widget:set_markup(lain.util.markup.fg.color(beautiful.base05, display_text))
   end
}

-- Create a textclock widget
local mytextcalendar = wibox.widget.textclock(
   lain.util.markup.fg.color(beautiful.base05, "  %B %d")
)
local mytextclock = wibox.widget.textclock(
   lain.util.markup.fg.color(beautiful.base05, "  %I:%M%P"),
   10
)

local function set_wallpaper(s)
   -- Wallpaper
   if beautiful.wallpaper then
      local wallpaper = beautiful.wallpaper
      -- If wallpaper is a function, call it with the screen
      if type(wallpaper) == "function" then
         wallpaper = wallpaper(s)
      end
      gears.wallpaper.maximized(wallpaper, s, true)
   end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
      -- Wallpaper
      set_wallpaper(s)

      -- Each screen has its own tag table.
      local l = awful.layout.suit
      local layouts = {l.floating, l.max}
      local tags = create_tags(beautiful.tags, s, layouts)
      -- update_tag_icons(awful.screen.focused().tags)
      update_tag_icons(tags)

      -- Create an imagebox widget which will contain an icon indicating which layout we're using.
      -- We need one layoutbox per screen.
      s.mylayoutbox = awful.widget.layoutbox(s)
      s.mylayoutbox:buttons(gears.table.join(
                               awful.button({ }, 1, function () awful.layout.inc( 1) end),
                               awful.button({ }, 3, function () awful.layout.inc(-1) end),
                               awful.button({ }, 4, function () awful.layout.inc( 1) end),
                               awful.button({ }, 5, function () awful.layout.inc(-1) end)))

      -- Create a taglist widget
      s.mytaglist = awful.widget.taglist(
         s,
         awful.widget.taglist.filter.all,
         keys.taglist_buttons
      )

      -- Create a tasklist widget
      -- https://www.reddit.com/r/awesomewm/comments/6cuuz8/awesome_fixed_width_tasklist_items/di9lkb5
      s.mytasklist = awful.widget.tasklist(
         s, -- screen
         awful.widget.tasklist.filter.currenttags, -- filter
         keys.tasklist_buttons, -- buttons
         { -- style
            align = "center",
            spacing = 4,
         },
         function (w, buttons, label, data, objects) -- update
            awful.widget.common.list_update(w, buttons, label, data, objects)
            w:set_max_widget_size(beautiful.tasklist_item_width)
         end,
         wibox.layout.flex.horizontal() -- base widget
      )

      -- Create the wibox
      s.mywibox = awful.wibar({ position = "bottom", screen = s })

      -- Add widgets to the wibox

      local function info_group(widgets, bg_color)
         local outer_padding = 10
         local inner_padding = 5

         for i = 1, #widgets do
            widgets[i] = wibox.container.margin(widgets[i], inner_padding, inner_padding)
         end

         widgets.layout = wibox.layout.fixed.horizontal

         return wibox.container.background(
            wibox.container.margin(wibox.widget(widgets), outer_padding, outer_padding), bg_color
         )
      end

      s.mywibox:setup {
         { -- Left widgets
            wibox.container.margin(s.mytaglist, 0, 4),
            layout = wibox.layout.fixed.horizontal
         },
         s.mytasklist, -- Middle widget
         { -- Right widgets
            wibox.container.margin(wibox.widget.systray(), 0, 0),
            info_group({ lain_bat.widget, lain_vol.widget }, beautiful.base00),
            info_group({ mytextcalendar, mytextclock }, beautiful.base01),
            s.mylayoutbox,
            layout = wibox.layout.fixed.horizontal
         },
         layout = wibox.layout.align.horizontal
      }

      awful.tag.attached_connect_signal(s, "property::selected", function () update_tag_icons(s.tags) end)
      awful.tag.attached_connect_signal(s, "property::hide", function () update_tag_icons(s.tags) end)
      awful.tag.attached_connect_signal(s, "property::activated", function () update_tag_icons(s.tags) end)
      awful.tag.attached_connect_signal(s, "property::screen", function () update_tag_icons(s.tags) end)
      awful.tag.attached_connect_signal(s, "property::index", function () update_tag_icons(s.tags) end)
      awful.tag.attached_connect_signal(s, "property::urgent", function () update_tag_icons(s.tags) end)
end)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = {
        border_width = beautiful.border_width,
        border_color = beautiful.border_normal,
        focus = awful.client.focus.filter,
        raise = true,
        keys = keys.clientkeys,
        buttons = keys.clientbuttons,
        screen = awful.screen.preferred,
        placement = awful.placement.no_overlap+awful.placement.no_offscreen,
        size_hints_honor = false
     },
     callback = awful.client.setslave
   },

   -- Floating clients.
   { rule_any = {
        instance = {
           "DTA",  -- Firefox addon DownThemAll.
           "copyq",  -- Includes session name in class.
        },
        class = {
           "Arandr",
           "Gpick",
           "Kruler",
           "MessageWin",  -- kalarm.
           "Sxiv",
           "Wpa_gui",
           "pinentry",
           "veromix",
           "xtightvncviewer"},

        name = {
           "Event Tester",  -- xev.
        },
        role = {
           "AlarmWindow",  -- Thunderbird's calendar.
           "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
   }, properties = { floating = true }},

   -- Add titlebars to normal clients and dialogs
   { rule_any = {type = { "normal", "dialog" }
                }, properties = { titlebars_enabled = true }
   },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal(
   "manage",
   function (c)
      -- Set the windows at the slave,
      -- i.e. put it at the end of others instead of setting it master.
      -- if not awesome.startup then awful.client.setslave(c) end

      if awesome.startup and
         not c.size_hints.user_position
      and not c.size_hints.program_position then
         -- Prevent clients from being unreachable after screen count changes.
         awful.placement.no_offscreen(c)
      end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal(
   "request::titlebars",
   function(c)
      -- buttons for the titlebar
      local buttons = gears.table.join(
         awful.button({ }, 1, function()
               client.focus = c
               c:raise()
               awful.mouse.client.move(c)
         end),
         awful.button({ }, 3, function()
               client.focus = c
               c:raise()
               awful.mouse.client.resize(c)
         end)
      )

      local titlewidget = awful.titlebar.widget.titlewidget(c)
      titlewidget.font = beautiful.titlebar_font

      awful.titlebar(c, { size = beautiful.titlebar_size }) : setup
      {
         { widget = titlewidget, align = "center" },
         buttons = buttons,
         layout = wibox.layout.stack()
      }
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

client.connect_signal("unmanage", function(c) update_tag_icons(awful.screen.focused().tags) end)
client.connect_signal("untagged", function(c) update_tag_icons(awful.screen.focused().tags) end)
client.connect_signal("tagged", function(c) update_tag_icons(awful.screen.focused().tags) end)
client.connect_signal("screen", function(c) update_tag_icons(awful.screen.focused().tags) end)

awesome.connect_signal("volume_update", function() lain_vol.update() end)

-- Startup
awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/startup.sh")
-- }}}
