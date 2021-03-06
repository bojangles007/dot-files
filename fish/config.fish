set EDITOR emacsclient
export EDITOR

# I applaud the initiative but my high-latency connection regretfully make
# these a massive pain in daily use.
set HOMEBREW_NO_AUTO_UPDATE 1
set HOMEBREW_NO_ANALYTICS 1

# Rbenv
status --is-interactive; and . (rbenv init -|psub)

eval (python -m virtualfish)

# autocompletion for AWS CLI; if the aws_completer binary is installed
test -x (which aws_completer); and complete --command aws --no-files --arguments '(begin; set --local --export COMP_SHELL fish; set --local --export COMP_LINE (commandline); aws_completer | sed \'s/ $//\'; end)'
