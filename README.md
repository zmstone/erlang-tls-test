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

* `TLSER_SERVER_HOST`: default is `localhost`, used to specify server hostname.
* `TLSER_CLIENT_CIPHERS`: comma separated cipher suite names. default "ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384"
* `TLSER_MAX_FRAGMENT_LENTH`

### For both server and client

- `TLSER_SERVER_PORT`: default 9999, used to specify server port number
- `TLSER_PROTOCOL`: set to `dtls` or `tls` (default)
- `TLSER_TLS_VERSIONS`: comma separated verstions. e.g. `1.1,1.2,1.3`
- `TLSER_CERT`: `path/to/certificates/dir`, in the dir, there must be:
  - CA: `ca.pem` for both client and server.
  - Client: `client.key`, `client.pem`
  - Server: `server.key`, `server.pem`
