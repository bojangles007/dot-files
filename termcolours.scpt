on run desiredColour
    if length of desiredColour > 0
        set colour to desiredColour
    else
        set colour to { 52007, 48990, 29970 }
    end

   tell application "Terminal"
        tell selected tab of front window
          set background color to colour
     end tell
  end tell
end run
