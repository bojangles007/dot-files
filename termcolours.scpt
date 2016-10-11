on run desiredColour
    if length of desiredColour > 0
        set colour to desiredColour
    else
        set colour to { 55372, 54247, 46907 }
    end

    tell application "Terminal"
        tell selected tab of front window
            set background color to colour
    end tell
  end tell
end run
