function ssh --description 'wrapper around ssh to reset term colours after exit'
        /usr/bin/ssh $argv
        osascript ~/Dotfiles/termcolours.scpt
end
