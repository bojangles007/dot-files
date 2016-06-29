set EDITOR 'emacsclient -n'
export EDITOR

# Add to path for psql
set PATH (ls -d1  /Applications/Postgres.app/Contents/Versions/*/bin | tail -1) $PATH

# emacsclient
set PATH (ls -d1 /Applications/Emacs.app/Contents/MacOS/bin-x86_64-* | tail -1) $PATH

# Jekyll etc
set PATH $HOME/.gem/ruby/2.0.0/bin $PATH
