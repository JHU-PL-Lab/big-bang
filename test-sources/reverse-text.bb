let reverseText(text) =
  if text = "" then
    text
  else
    reverseText(slice(text, 1, -1)) + text(0)
  end

reverseText("BigBang")
