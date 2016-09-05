function lpclone --description 'git clone + set laterpay email for repo'
        set repo $argv[1]
        git clone https://github.com/laterpay/$repo ; and cd $repo; and git config --add user.email sbrautaset@laterpay.net
end
