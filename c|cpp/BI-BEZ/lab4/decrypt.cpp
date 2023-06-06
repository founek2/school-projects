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
        std::cerr << "Musíte zadat 3 argumenty: path_to_ecrypted_file path_to_priv_key path_to_output_file" << std::endl;
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

    /* Load private key */
    EVP_PKEY *priv_key;
    FILE *fp = fopen(args[2], "r");
    if (fp == NULL){
        std::cerr << "Nelze otevřít soubor s priv. key " << args[2];
        return 1;
    }
    priv_key = PEM_read_PrivateKey(fp, NULL, NULL, NULL); //No password protection of the key itself
    if (priv_key == NULL) {
        std::cerr << "Nelze načíst veřejný klíč " << args[2] << std::endl;
        return 1;
    }

    /* Load header from encrypted file */
    CUSTOM header;
    f_in >> header;

    /* Select cipher based on header */
    const EVP_CIPHER *cipher;
    std::string cipherName;
    if (header.cipher_type == 0)
        cipherName = "AES-128-CBC";
    else if (header.cipher_type == 1)
        cipherName = "AES-128-ECB";
    else throw std::runtime_error("Invalid cipher type");

    OpenSSL_add_all_ciphers();
    cipher = EVP_get_cipherbyname(cipherName.c_str());

    EVP_CIPHER_CTX *ctx;
    int open_text_len = 0;
    unsigned char iv[EVP_MAX_IV_LENGTH];
    auto * encrypted_key = new unsigned char[header.ek_len]; // allocate space for encrypted symmet. key

    /* Load ek and IV from right positions */
    f_in.seekg(header.ek_pos);
    f_in.read((char *) encrypted_key, header.ek_len);
    f_in.seekg(header.iv_pos);
    f_in.read((char *) iv, header.iv_len);

    if (RAND_load_file("/dev/random", 32) != 32) {
        puts("Cannot seed the random generator!");
        exit(1);
    }

    /* Create and initialise the context */
    if (!(ctx = EVP_CIPHER_CTX_new())) handleErrors();

    /* Initialise the envelope Open operation. */
    if (1 != EVP_OpenInit(ctx, cipher, encrypted_key,
                          header.ek_len, iv, priv_key))
        return handleErrors();

    char bytes[4096];
    int len = 0;
    char ot[4096 + EVP_MAX_BLOCK_LENGTH];

    f_in.seekg(header.data_pos);
    f_in.read(bytes, 4096);
    while (f_in.gcount() > 0) {
        /* Provide the message to be decrypted, and obtain the decrypted output ot */
        if (1 != EVP_OpenUpdate(ctx, (unsigned char *)ot, &len, (const unsigned char *) bytes, f_in.gcount()))
            return handleErrors();
        open_text_len += len;

        f_out.write(ot, len);
        f_in.read(bytes, 4096);
    }

    /* Finalise the decryption. Further open text bytes may be written at this stage*/
    if (1 != EVP_OpenFinal(ctx, (unsigned char *) ot, &len)) return handleErrors();
    open_text_len += len;
    f_out.write(ot, len);

    f_in.close();
    f_out.close();

    /* Clean up */
    EVP_CIPHER_CTX_free(ctx);
    delete[] encrypted_key;

    return 0;
}
