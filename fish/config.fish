set EDITOR emacsclient

# Ruby gems
set PATH (echo ~/.gem/ruby/*/bin) $PATH

eval (python -m virtualfish)

# autocompletion for AWS CLI; if the aws_completer binary is installed
test -x (which aws_completer); and complete --command aws --no-files --arguments '(begin; set --local --export COMP_SHELL fish; set --local --export COMP_LINE (commandline); aws_completer | sed \'s/ $//\'; end)'
