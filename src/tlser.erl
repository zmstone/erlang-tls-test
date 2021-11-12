-module(tlser).

-export([cipher_suites/1,
         versions/0,
         protocol/0,
         server_port/0,
         tls_v13_ciphers/0,
         cert_dir/0
        ]).

cipher_suites(server) ->
    ssl:cipher_suites(all, 'tlsv1.2', openssl);
cipher_suites(client) ->
    case os:getenv("TLSER_CLIENT_CIPHERS") of
        false -> ["ECDHE-ECDSA-AES256-GCM-SHA384", "ECDHE-RSA-AES256-GCM-SHA384"];
        Other -> string:tokens(Other, ",")
    end.

protocol() ->
    case os:getenv("TLSER_PROTOCOL") of
        "dtls" -> dtls;
        _ -> tls
    end.

versions() -> versions(protocol()).

versions(dtls) -> ['dtlsv1.2'];
versions(tls) -> ['tlsv1.2', 'tlsv1.1'].

server_port() ->
    case os:getenv("TLSER_SERVER_PORT") of
        false -> 9999;
        N -> list_to_integer(N)
    end.

tls_v13_ciphers() ->
    ["TLS_AES_256_GCM_SHA384",
     "TLS_AES_128_GCM_SHA256",
     "TLS_CHACHA20_POLY1305_SHA256",
     "TLS_AES_128_CCM_SHA256",
     "TLS_AES_128_CCM_8_SHA256"].

cert_dir() ->
    {ok, Pwd} = file:get_cwd(),
    R = case os:getenv("TLSER_CERT_TYPE") of
            false -> rsa;
            T -> filename:join([Pwd, T])
        end,
    io:format(user, "using certs in: ~s~n", [R]),
    R.
