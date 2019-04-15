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
require("awful.hotkeys_popup.keys")

-- Load Debian menu entries
local debian = require("debian.menu")
local has_fdo, freedesktop = pcall(require, "freedesktop")

local lain = require("lain")
local utils = require("utils")

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

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"
altkey = "Mod1"

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
local function client_menu_toggle_fn()
   local instance = nil

   return function ()
      if instance and instance.wibox.visible then
         instance:hide()
         instance = nil
      else
         instance = awful.menu.clients({ theme = { width = 250 } })
      end
   end
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

mylauncher = awful.widget.launcher({ menu = mymainmenu })

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

      widget:set_markup(
         utils.colorize_text(bat_icon .. " " .. bat_now.perc .. "%", beautiful.base04)
      )
   end
}

-- Volume widget
local lain_vol = lain.widget.alsa {
   timeout = 5,
   settings = function()
      local display_text = " mute"

      if volume_now.status == "on" then
         display_text = " " .. volume_now.level .. "%"
      end

      widget:set_markup(utils.colorize_text(display_text, beautiful.base04))
   end
}

-- Create a textclock widget
local mytextcalendar = wibox.widget.textclock(
   utils.colorize_text(" %a, %b %d", beautiful.base00)
)
local mytextclock = wibox.widget.textclock(
   utils.colorize_text(" %I:%M%P", beautiful.base00)
)

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
   awful.button({ }, 1, function(t) t:view_only() end),
   awful.button({ modkey }, 1, function(t)
         if client.focus then
            client.focus:move_to_tag(t)
         end
   end),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, function(t)
         if client.focus then
            client.focus:toggle_tag(t)
         end
   end),
   awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
   awful.button({ }, 1, function (c)
         if c == client.focus then
            c.minimized = true
         else
            -- Without this, the following
            -- :isvisible() makes no sense
            c.minimized = false
            if not c:isvisible() and c.first_tag then
               c.first_tag:view_only()
            end
            -- This will also un-minimize
            -- the client, if needed
            client.focus = c
            c:raise()
         end
   end),
   awful.button({ }, 3, client_menu_toggle_fn()),
   awful.button({ }, 4, function ()
         awful.client.focus.byidx(1)
   end),
   awful.button({ }, 5, function ()
         awful.client.focus.byidx(-1)
end))

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
      local layouts = { l.floating, l.tile, l.tile, l.tile, l.floating,
                        l.floating, l.floating, l.floating, l.floating }
      awful.tag(beautiful.tags_empty, s, layouts)

      -- Create a promptbox for each screen
      s.mypromptbox = awful.widget.prompt()
      -- Create an imagebox widget which will contain an icon indicating which layout we're using.
      -- We need one layoutbox per screen.
      s.mylayoutbox = awful.widget.layoutbox(s)
      s.mylayoutbox:buttons(gears.table.join(
                               awful.button({ }, 1, function () awful.layout.inc( 1) end),
                               awful.button({ }, 3, function () awful.layout.inc(-1) end),
                               awful.button({ }, 4, function () awful.layout.inc( 1) end),
                               awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      -- Create a taglist widget
      s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

      -- Create a tasklist widget
      -- https://www.reddit.com/r/awesomewm/comments/6cuuz8/awesome_fixed_width_tasklist_items/di9lkb5
      s.mytasklist = awful.widget.tasklist(
         s, -- screen
         awful.widget.tasklist.filter.currenttags, -- filter
         tasklist_buttons, -- buttons
         { -- style
            align = "center",
            spacing = 4,
         },
         function (w, buttons, label, data, objects) -- update
            awful.widget.common.list_update(w, buttons, label, data, objects)
            w:set_max_widget_size(beautiful.tasklist_width)
         end,
         wibox.layout.flex.horizontal() -- base widget
      )

      -- Create the wibox
      s.mywibox = awful.wibar({ position = "bottom", screen = s })

      -- Add widgets to the wibox

      local function info_group(widgets, bg_color)
         local outer_padding = 15
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
            s.mylayoutbox,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
            layout = wibox.layout.fixed.horizontal
         },
         s.mytasklist, -- Middle widget
         { -- Right widgets
            wibox.container.margin(wibox.widget.systray(), 0, 15),
            info_group({ lain_bat.widget, lain_vol.widget }, beautiful.base02),
            info_group({ mytextcalendar, mytextclock }, beautiful.base04),
            layout = wibox.layout.fixed.horizontal
         },
         layout = wibox.layout.align.horizontal
      }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = gears.table.join(
   awful.key({ modkey }, "F1", hotkeys_popup.show_help,
      {description="show help", group="awesome"}),
   awful.key({ modkey }, "Left", awful.tag.viewprev,
      {description = "view previous", group = "tag"}),
   awful.key({ modkey }, "Right", awful.tag.viewnext,
      {description = "view next", group = "tag"}),

   awful.key({ modkey }, "a", function () mymainmenu:show() end,
      {description = "show main menu", group = "awesome"}),

   awful.key({ modkey }, "j",
      function ()
         awful.client.focus.bydirection("down")
         if client.focus then client.focus:raise() end
      end,
      {description = "focus down", group = "client"}),
   awful.key({ modkey }, "k",
      function ()
         awful.client.focus.bydirection("up")
         if client.focus then client.focus:raise() end
      end,
      {description = "focus up", group = "client"}),
   awful.key({ modkey }, "h",
      function ()
         awful.client.focus.bydirection("left")
         if client.focus then client.focus:raise() end
      end,
      {description = "focus left", group = "client"}),
   awful.key({ modkey }, "l",
      function ()
         awful.client.focus.bydirection("right")
         if client.focus then client.focus:raise() end
      end,
      {description = "focus right", group = "client"}),


   -- Standard program
   awful.key({ modkey }, "Return", function () awful.spawn(terminal) end,
      {description = "open a terminal", group = "launcher"}),
   awful.key({ modkey, "Control" }, "Return",
      function () awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/files.sh") end,
      {description = "open file browser", group = "launcher"}),
   awful.key({ modkey, "Control" }, "r", awesome.restart,
      {description = "reload awesome", group = "awesome"}),

   -- Layout manipulation
   awful.key({ altkey }, "Tab", function () awful.client.focus.byidx(1) end,
      {description = "focus next", group = "client"}),
   awful.key({ altkey, "Shift" }, "Tab", function () awful.client.focus.byidx(-1) end,
      {description = "focus previous", group = "client"}),
   awful.key({ modkey, "Shift" }, "j",
      function ()
         local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
         local c = client.focus
         if c ~= nil and (current_layout == "floating" or c.floating) then
            c:relative_move(0, 40, 0, 0)
         else
            awful.client.swap.bydirection("down", c, nil)

         end
      end,
      {description = "swap with direction down", group = "client"}),
   awful.key({ modkey, "Shift" }, "k",
      function ()
         local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
         local c = client.focus
         if c ~= nil and (current_layout == "floating" or c.floating) then
            c:relative_move(0, -40, 0, 0)
         else
            awful.client.swap.bydirection("up", c, nil)
         end
      end,
      {description = "swap with direction up", group = "client"}),
   awful.key({ modkey, "Shift" }, "h",
      function ()
         local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
         local c = client.focus
         if c ~= nil and (current_layout == "floating" or c.floating) then
            c:relative_move(-40, 0, 0, 0)
         else
            awful.client.swap.bydirection("left", c, nil)
         end
      end,
      {description = "swap with direction left", group = "client"}),
   awful.key({ modkey, "Shift" }, "l",
      function ()
         local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
         local c = client.focus
         if c ~= nil and (current_layout == "floating" or c.floating) then
            c:relative_move(40, 0, 0, 0)
         else
            awful.client.swap.bydirection("right", c, nil)
         end
      end,
      {description = "swap with direction right", group = "client"}),

   awful.key({ modkey }, "space", function () awful.layout.inc( 1) end,
      {description = "select next layout", group = "layout"}),
   awful.key({ modkey, "Shift" }, "space", function () awful.layout.inc(-1) end,
      {description = "select previous", group = "layout"}),

   awful.key({ modkey, "Control" }, "n",
      function ()
         local c = awful.client.restore()
         -- Focus restored client
         if c then
            client.focus = c
            c:raise()
         end
      end,
      {description = "restore minimized", group = "client"}),

   -- Prompt
   awful.key({ modkey }, "d", function () awful.spawn.with_shell("/home/jason/scripts/rofi.sh") end,
      {description = "run prompt", group = "launcher"}),

   -- Brightness
   awful.key({ }, "XF86MonBrightnessDown",
      function() awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/brightness.sh down") end,
      {description = "decrease brightness", group = "brightness"}),
   awful.key({ }, "XF86MonBrightnessUp",
      function() awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/brightness.sh up") end,
      {description = "increase brightness", group = "brightness"}),

   -- Media Keys
   awful.key({ }, "XF86AudioMute",
      function()
         -- Running the following line doesn't trigger the volume widget update
         -- awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/audio.sh mute")

         -- Running the command async fixes this though
         awful.spawn.easy_async_with_shell(
            os.getenv("HOME") .. "/scripts/audio.sh mute",
            function()
               lain_vol.update()
         end)
      end,
      {description = "mute volume", group = "audio"}),
   awful.key({ }, "XF86AudioLowerVolume",
      function()
         awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/audio.sh down")
         lain_vol.update()
      end,
      {description = "lower volume", group = "audio"}),
   awful.key({ }, "XF86AudioRaiseVolume",
      function()
         awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/audio.sh up")
         lain_vol.update()
      end,
      {description = "raise volume", group = "audio"}),

   awful.key({ }, "XF86AudioPlay",
      function() awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/audio.sh play") end,
      {description = "play music", group = "audio"}),
   awful.key({ }, "XF86AudioStop",
      function() awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/audio.sh stop") end,
      {description = "stop music", group = "audio"}),
   awful.key({ }, "XF86AudioPrev",
      function() awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/audio.sh prev") end,
      {description = "previous song", group = "audio"}),
   awful.key({ }, "XF86AudioNext",
      function() awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/audio.sh next") end,
      {description = "next song", group = "audio"})
)

clientkeys = gears.table.join(
   awful.key({ modkey }, "f",
      function (c)
         c.fullscreen = not c.fullscreen
         c:raise()
      end,
      {description = "toggle fullscreen", group = "client"}),
   awful.key({ modkey }, "c",
      function (c) awful.placement.centered(c, { honor_workarea = true }) end,
      {description = "center client", group = "client"}),
   awful.key({ modkey, "Shift" }, "q", function (c) c:kill() end,
      {description = "quit", group = "client"}),
   awful.key({ modkey, "Control" }, "space", awful.client.floating.toggle ,
      {description = "toggle floating", group = "client"}),
   -- awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
             -- {description = "move to master", group = "client"}),
   awful.key({ modkey }, "o", function (c) c:move_to_screen() end,
      {description = "move to screen", group = "client"}),
   awful.key({ modkey }, "t", function (c) c.ontop = not c.ontop end,
      {description = "toggle keep on top", group = "client"}),
   awful.key({ modkey }, "n",
      function (c)
         -- The client currently has the input focus, so it cannot be
         -- minimized, since minimized clients can't have the focus.
         c.minimized = true
      end ,
      {description = "minimize", group = "client"}),
   awful.key({ modkey }, "w",
      function (c)
         c.maximized = not c.maximized
         c:raise()
      end ,
      {description = "toggle maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   globalkeys = gears.table.join(
      globalkeys,
      -- View tag only.
      awful.key({ modkey }, "#" .. i + 9,
         function ()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
               tag:view_only()
            end
         end,
         {description = "view tag #"..i, group = "tag"}),
      -- Toggle tag display.
      awful.key({ modkey, "Control" }, "#" .. i + 9,
         function ()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
               awful.tag.viewtoggle(tag)
            end
         end,
         {description = "toggle tag #" .. i, group = "tag"}),
      -- Move client to tag.
      awful.key({ modkey, "Shift" }, "#" .. i + 9,
         function ()
            if client.focus then
               local tag = client.focus.screen.tags[i]
               if tag then
                  client.focus:move_to_tag(tag)
               end
            end
         end,
         {description = "move focused client to tag #"..i, group = "tag"}),
      -- Toggle tag on focused client.
      awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
         function ()
            if client.focus then
               local tag = client.focus.screen.tags[i]
               if tag then
                  client.focus:toggle_tag(tag)
               end
            end
         end,
         {description = "toggle focused client on tag #" .. i, group = "tag"})
   )
end

clientbuttons = gears.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
                    border_color = beautiful.border_normal,
                    focus = awful.client.focus.filter,
                    raise = true,
                    keys = clientkeys,
                    buttons = clientbuttons,
                    screen = awful.screen.preferred,
                    placement = awful.placement.no_overlap+awful.placement.no_offscreen,
                    size_hints_honor = false
     }
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

      awful.titlebar(c, { size = beautiful.titlebar_size }) : setup
      {
         wibox.widget.base.empty_widget(),
         wibox.container.margin(awful.titlebar.widget.titlewidget(c), 20),
         wibox.container.margin(wibox.widget {
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.closebutton(c),
            layout = wibox.layout.fixed.horizontal,
         }, nil, 10),
         layout = wibox.layout.align.horizontal
      }
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- Startup
awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/startup.sh")
-- }}}
