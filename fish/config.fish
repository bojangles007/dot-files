set EDITOR emacsclient

# emacsclient
set PATH /Applications/Emacs.app/Contents/MacOS/bin $PATH

eval (python -m virtualfish)

osascript ~/Dotfiles/termcolours.scpt
