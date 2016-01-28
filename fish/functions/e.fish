function e --description 'Open file in Emacs'
        for f in $argv:
                emacsclient -n $f
        end
end
