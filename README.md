# tlser

A demo application to run TLS/DTLS client & server.

## Start server

```
./run.sh server
```

## Start client

```
./run.sh client
```

## More configs via environment variables

### For client

* `TLSER_SERVER_HOST`: default localhost, used to specify server hostname
* `TLSER_CLIENT_CIPHERS`: comma separated cipher suite names. default "ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384"

### For both server and client

* `TLSER_SERVER_PORT`: default 9999, used to specify server port number
* `TLSER_PROTOCOL`: set to `dtls` or `tls` (default)
* `TLSER_CERT`: set to `rsa` (default) or `ecc`
