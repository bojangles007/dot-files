function ec --description 'Open files in new Emacs frames'
        emacsclient -nc $argv[1]
        for f in $argv[2..(count $argv)]:
                emacsclient -nc $f
        end
end
