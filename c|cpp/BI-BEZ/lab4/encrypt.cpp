/**
 * Martin Skalický
 */
#include <iostream>
#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/rand.h>
#include <fstream>
#include "custom.h"

int handleErrors() {
    std::cerr << "Nastala chyba" << std::endl;
    return 1;
}

// args> input_bin_file pub_key_file output_encrypted_file
int main(int args_count, char *args[]) {
    if (args_count < 4) {
        std::cerr << "Musíte zadat 3 argumenty: path_to_bin_file path_to_pub_key path_to_output_file" << std::endl;
        return 1;
    }
    const char *in_file = args[1];
    const char *out_file = args[3];
    std::ifstream f_in(in_file, std::ios::in | std::ios::binary);
    std::ofstream f_out(out_file, std::ios::out | std::ios::binary);
    if (!f_in.is_open()) {
        std::cerr << "Nelze otevřít vstupní soubor " << in_file << std::endl;
        return 1;
    }
    if (!f_out.is_open()) {
        std::cerr << "Nelze otevřít výstupní soubor " << out_file << std::endl;
        return 1;
    }

    /* Load pub key */
    EVP_PKEY *pub_key;
    FILE *fp = fopen(args[2], "r");
    pub_key = PEM_read_PUBKEY(fp, NULL, NULL, NULL); //No password protection of the key itself
    if (pub_key == NULL) {
        std::cerr << "Nelze načíst veřejný klíč " << args[2] << std::endl;
        return 1;
    }

    /*
     * Supported are ciphers AES-128-CBC and AES-128-ECB
     */
    const EVP_CIPHER *cipher;
    const std::string cipherName = std::string("AES-128-ECB");

    OpenSSL_add_all_ciphers();
    cipher = EVP_get_cipherbyname(cipherName.c_str());

    EVP_CIPHER_CTX *ctx;
    int ciphertext_len = 0;
    unsigned char iv[EVP_MAX_IV_LENGTH];
    int eklen;
    auto * encrypted_key = new unsigned char[EVP_PKEY_size(pub_key)]; // allocate space for encrypted symmet. key

    if (RAND_load_file("/dev/random", 32) != 32) {
        puts("Cannot seed the random generator!");
        exit(1);
    }

    /* Create and initialise the context */
    if (!(ctx = EVP_CIPHER_CTX_new())) handleErrors();

    /* Initialise the envelope seal operation. This operation generates
     * a key for the provided cipher, and then encrypts that key a number
     * of times (one for each public key provided in the pub_key array). In
     * this example the array size is just one. This operation also
     * generates an IV and places it in iv. */
    if (1 != EVP_SealInit(ctx, cipher, &encrypted_key,
                          &eklen, iv, &pub_key, 1))
        return handleErrors();

    char bytes[4096];
    int len = 0;
    char ct[4096 + EVP_MAX_BLOCK_LENGTH];

    /* write header into out_file */
    struct CUSTOM header(cipherName, eklen);
    f_out << header;

    /* Write IV and ek on right positions */
    f_out.seekp(header.ek_pos);
    f_out.write((const char *) encrypted_key, header.ek_len);
    f_out.seekp(header.iv_pos);
    f_out.write((const char *) iv, header.iv_len);
    f_out.seekp(header.data_pos);

    f_in.read(bytes, 4096);
    while (f_in.gcount() > 0) {
        /* Provide the message to be encrypted, and obtain the encrypted output. */
        if (1 != EVP_SealUpdate(ctx, (unsigned char *) ct, &len, (const unsigned char *) bytes, f_in.gcount()))
            return handleErrors();
        ciphertext_len += len;

        f_out.write(ct, len);
        f_in.read(bytes, 4096);
    }

    /* Finalise the encryption. Further ciphertext bytes may be written at
     * this stage.
     */
    if (1 != EVP_SealFinal(ctx, (unsigned char *) ct, &len)) return handleErrors();
    ciphertext_len += len;
    f_out.write(ct, len);

    f_in.close();
    f_out.close();

    /* Clean up */
    EVP_CIPHER_CTX_free(ctx);
    delete[] encrypted_key;

    return 0;
}
