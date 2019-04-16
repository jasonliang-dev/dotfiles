-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup").widget

local keys = {}

-- Floating client move and resize step
client_move_step = 50
client_resize_step = 50

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"
altkey = "Mod1"

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

-- {{{ Mouse bindings
root.buttons(gears.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
keys.globalkeys = gears.table.join(
   awful.key({ modkey }, "F1", hotkeys_popup.show_help,
      {description="show help", group="awesome"}),
   awful.key({ modkey }, "Left", awful.tag.viewprev,
      {description = "view previous", group = "tag"}),
   awful.key({ modkey }, "Right", awful.tag.viewnext,
      {description = "view next", group = "tag"}),

   awful.key({ modkey }, "a", function () mymainmenu:show() end,
      {description = "show main menu", group = "awesome"}),

   -- Standard program
   awful.key({ modkey }, "Return", function () awful.spawn(terminal) end,
      {description = "open a terminal", group = "launcher"}),
   awful.key({ modkey, "Control" }, "Return",
      function () awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/files.sh") end,
      {description = "open file browser", group = "launcher"}),
   awful.key({ modkey, "Control" }, "r", awesome.restart,
      {description = "reload awesome", group = "awesome"}),

   -- Focus
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

   -- Layout manipulation
   awful.key({ altkey }, "Tab", function () awful.client.focus.byidx(1) end,
      {description = "focus next", group = "client"}),
   awful.key({ altkey, "Shift" }, "Tab", function () awful.client.focus.byidx(-1) end,
      {description = "focus previous", group = "client"}),

   -- Swap/Move client
   awful.key({ modkey, "Shift" }, "j",
      function ()
         local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
         local c = client.focus
         if c ~= nil and (current_layout == "floating" or c.floating) then
            c:relative_move(0, client_move_step, 0, 0)
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
            c:relative_move(0, -client_move_step, 0, 0)
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
            c:relative_move(-client_move_step, 0, 0, 0)
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
            c:relative_move(client_move_step, 0, 0, 0)
         else
            awful.client.swap.bydirection("right", c, nil)
         end
      end,
      {description = "swap with direction right", group = "client"}),

   -- Resize Client
   awful.key({ modkey, "Control" }, "j",
      function ()
         local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
         local c = client.focus
         if current_layout == "floating" or c.floating == true then
            c:relative_move(0, 0, 0, client_resize_step)
         else
            awful.client.incwfact(0.05)
         end
      end,
      {description = "resize down", group = "layout"}),

   awful.key({ modkey, "Control" }, "k",
      function ()
         local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
         local c = client.focus
         if current_layout == "floating" or c.floating == true then
            c:relative_move(0, 0, 0, -client_resize_step)
         else
            awful.client.incwfact(-0.05)
         end
      end,
      {description = "resize up", group = "layout"}),
   awful.key({ modkey, "Control" }, "h",
      function ()
         local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
         local c = client.focus
         -- Floating: resize client
         if current_layout == "floating" or c.floating == true then
            c:relative_move(0, 0, -client_resize_step, 0)
         else
            awful.tag.incmwfact(-0.05)
         end
      end,
      {description = "resize left", group = "layout"}),
   awful.key({ modkey, "Control" }, "l",
      function ()
         local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
         local c = client.focus
         -- Floating: resize client
         if current_layout == "floating" or c.floating == true then
            c:relative_move(0, 0, client_resize_step, 0)
         else
            awful.tag.incmwfact(0.05)
         end
      end,
      {description = "resize right", group = "layout"}),

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
               awesome.emit_signal("volume_update")
            end
         )
      end,
      {description = "mute volume", group = "audio"}),
   awful.key({ }, "XF86AudioLowerVolume",
      function()
         awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/audio.sh down")
         awesome.emit_signal("volume_update")
      end,
      {description = "lower volume", group = "audio"}),
   awful.key({ }, "XF86AudioRaiseVolume",
      function()
         awful.spawn.with_shell(os.getenv("HOME") .. "/scripts/audio.sh up")
         awesome.emit_signal("volume_update")
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

keys.clientkeys = gears.table.join(
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
   keys.globalkeys = gears.table.join(
      keys.globalkeys,
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

keys.clientbuttons = gears.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Create a wibox for each screen and add it
keys.taglist_buttons = gears.table.join(
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

keys.tasklist_buttons = gears.table.join(
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

-- Set keys
root.keys(keys.globalkeys)
-- }}}


return keys
