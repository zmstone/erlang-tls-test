-module(tlser).

-export([cipher_suites/1,
         versions/0,
         protocol/0,
         server_port/0]).

cipher_suites(server) ->
    %% intentially testing tslv1.3 ciper suites in ssl options
    ["TLS_AES_256_GCM_SHA384",
     "TLS_AES_128_GCM_SHA256",
     "TLS_CHACHA20_POLY1305_SHA256",
     "TLS_AES_128_CCM_SHA256",
     "TLS_AES_128_CCM_8_SHA256" |
     ssl:cipher_suites(all, 'tlsv1.2', openssl)];
cipher_suites(client) ->
    ["ECDHE-ECDSA-AES256-GCM-SHA384"].

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
