function cert --description 'get information about a certificate'
        openssl x509 -text -noout -in $argv
end
