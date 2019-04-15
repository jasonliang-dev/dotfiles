local utils = {}

function utils.colorize_text(txt, fg)
   return "<span foreground=\"" .. fg .. "\">" .. txt .. "</span>"
end

return utils
