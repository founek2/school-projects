/**
 * Martin Skalický
 */
#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/rand.h>
#include <openssl/ssl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <iostream>
#include "cstring"

int main(int args_count, char *args[]) {
    const char *hostname = "fit.cvut.cz";
    int port = 443;

    int sd;
    struct sockaddr_in addr;

    sd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    memset(&addr, 0, sizeof(addr));

    int enable = 1;
    if (setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(int)) < 0)
        perror("setsockopt(SO_REUSEADDR) failed");

    //addr.sin_len = sizeof(addr);
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = inet_addr("147.32.232.212");
    if (connect(sd, (struct sockaddr *) &addr, sizeof(addr)) != 0) {
        close(sd);
        perror(hostname);
        abort();
    }

    SSL_library_init();
    SSL_CTX *ctx = SSL_CTX_new(SSLv23_client_method());
    if (ctx == NULL) {
        perror("Chyba SSL_CTX_new\n");
        return 1;
    }

    SSL_CTX_set_options(ctx, SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3 | SSL_OP_NO_TLSv1);
    SSL_CTX_set_ciphersuites(ctx, "TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256");   // disabling TLS_AES_256_GCM_SHA384
    const char * ca_file = "/test/cacert.pem";
    int ress = SSL_CTX_load_verify_locations(ctx, ca_file, NULL);   // load CA certificate
    printf("Loading certificate: %s", ca_file);
    if (ress != 1) {
        perror("Chyba Unable to load certificate\n");
        return 1;
    }

    SSL *ssl = SSL_new(ctx);
    if (ssl == NULL) {
        perror("Chyba SSL_new\n");
        return 1;
    }


    int res = 0;
    res = SSL_set_fd(ssl, sd);
    if (res != 1) {
        perror("Chyba SSL_set_fd\n");
        return 1;
    }

    res = SSL_set_tlsext_host_name(ssl, "fit.cvut.cz");
    if (res != 1) {
        perror("Chyba SSL_set_tlsext_host_name\n");
        return 1;
    }

    res = SSL_connect(ssl);
    if (res != 1) {
        perror("Chyba SSL_connect\n");
        return 1;
    }

    long result = SSL_get_verify_result(ssl);   // verify certificate using trusted CA
    std::cout << result << std::endl;
    if (result != X509_V_OK){
        perror("Chyba SSL_get_verify_result - chyba ověření certifikátu\n");
        return 1;
    }
    printf("Certificate verified using trsuted CA\n");

    const SSL_CIPHER *c = SSL_get_current_cipher(ssl);
    printf("Current used cipher> %s\n", SSL_CIPHER_get_name(c));
    printf("Použito bylo původně: TLS_AES_256_GCM_SHA384\n");
    printf(" * TLS - Protokol umožňující zabezpečnou komunikaci\n");
    printf(" * AES - symetrická šifra\n");
    printf(" * 256 - délka klíče 256b\n");
    printf(" * GCM - provozní řežim blokové šifry zajišťující autentizaci\n");
    printf(" * SHA384 - Typ hashovací funkce pro HMAC\n");

    int i = 0;
    const char *cipher = nullptr;
    printf("\nDostupné šifry dle priority:\n");
    do {
        cipher = SSL_get_cipher_list(ssl, i);
        printf("%d %s\n", i, cipher);
        ++i;
    } while (cipher != nullptr);
    printf("\n");

    X509 *cert = SSL_get_peer_certificate(ssl);
    if (cert == NULL) {
        perror("Chyba SSL_get_peer_certificate\n");
        return 1;
    }

    FILE *cert_file = fopen("certificate.pem", "wb");
    if (cert_file == NULL) {
        perror("Chyba fopen\n");
        return 1;
    }

    // PEM_write_X509(stdout, cert);
    PEM_write_X509(cert_file, cert);
    fclose(cert_file);

    // SSL_set_connect_state(ssl);
    char pszRequest[100] = {0};
    sprintf(pszRequest, "GET /%s HTTP/1.1\r\nHOST: %s\r\nContent-Type: text/plain\r\n\r\n", "kontakty", "fit.cvut.cz");
    int reqSize = strlen(pszRequest);

    printf("REQuest>\n %s", pszRequest);
    res = SSL_write(ssl, pszRequest, reqSize);
    printf("written: %d\n", res);
    if (res != reqSize) {
        perror("Chyba SSL_write\n");
        return 1;
    }

    char buffer[1024];
    FILE *f = fopen("webpage.html", "wb");

    printf("Downloading...\n");
    while (true) {
        res = SSL_read(ssl, buffer, 1024);
        // printf("reading: %dB\n", res);
        if (res < 0) {
            perror("Chyba SSL_read\n");
            return 1;
        }
        if (res == 0)
            break;
        fwrite(buffer, sizeof(char), res, f);
    }
    fclose(f);
    printf("Done.\n");


    SSL_shutdown(ssl);
    close(sd);
    SSL_free(ssl);
    SSL_CTX_free(ctx);

    return 0;
}
