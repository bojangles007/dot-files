function ec --description 'Open files in a new Emacs frame'
        emacsclient -nc $argv[1]
        for f in $argv[2..-1]:
                emacsclient -n $f
        end
end
