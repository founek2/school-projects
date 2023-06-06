/**
 * Martin Skalický
 */

#include <stdlib.h>
#include <openssl/evp.h>
#include <string.h>
#include <iostream>
#include <fstream>
#include <algorithm>


std::string toLower(std::string data) {
    std::transform(data.begin(), data.end(), data.begin(),
                   [](unsigned char c){ return std::tolower(c); });
    return data;
}

size_t copy_header(char * bytes,  std::ifstream &fh, std::ofstream &f_out) {
    fh.read(bytes, 15);
    size_t startPos = bytes[10] | (bytes[11] << 8) | (bytes[12] << 16) | (bytes[13] << 24);

    fh.seekg(std::ios::beg);
    fh.read(bytes, startPos);
    f_out.write(bytes, startPos);
    return startPos;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        perror("Musíte uvést šifrovací mód");
        exit(3);
    }

    int res;
    std::string FILE_NAME = "Mad_scientist";
    unsigned char key[EVP_MAX_KEY_LENGTH] = "Muj klic";  // klic pro sifrovani
    unsigned char iv[EVP_MAX_IV_LENGTH] = "inicial. vektor";  // inicializacni vektor
    const std::string cipherName = std::string("DES-") + argv[1];
    const EVP_CIPHER * cipher;

    OpenSSL_add_all_ciphers();
    /* sifry i hashe by se nahraly pomoci OpenSSL_add_all_algorithms() */
    cipher = EVP_get_cipherbyname(cipherName.c_str());
    if(!cipher) {
        printf("Sifra %s neexistuje.\n", cipherName.c_str());
        exit(1);
    }

    EVP_CIPHER_CTX *ctx; // context structure
    ctx = EVP_CIPHER_CTX_new();
    if (ctx == NULL) exit(2);


    /* Sifrovani */
    printf("Running encryption with cipher=%s and key=%s\n", cipherName.c_str(), key);
    res = EVP_EncryptInit_ex(ctx, cipher, NULL, key, iv);  // context init - set cipher, key, init vector
    if(res != 1) exit(3);


    const char *in_file = (FILE_NAME + ".bmp").c_str();
    const char *out_file = (FILE_NAME + "_" + toLower(argv[1]) + ".bmp").c_str();
    std::ifstream fh(in_file, std::ios::in | std::ios::binary |  std::ios::ate);
    std::ofstream f_out(out_file, std::ofstream::out | std::ofstream::binary);

    fh.seekg(0, std::ios::beg);
    char bytes[4096];
    size_t pos = 0;
    char st_out[4096 + EVP_MAX_BLOCK_LENGTH];
    int len = 0;

    if (!fh.is_open()) std::cerr << "Nelze otevřít " << in_file << std::endl;
    if (!f_out.is_open()) std::cerr << "Nelze otevřít " << out_file << std::endl;

    if (fh.is_open() && f_out.is_open()){

        pos = copy_header(bytes, fh, f_out);

        fh.read(bytes, 4096);
        while (fh.gcount() > 0){
            res = EVP_EncryptUpdate(ctx,(unsigned char *) st_out, &len, (const unsigned char *) bytes, fh.gcount());  // encryption of pt
            if(res != 1) exit(4);

            f_out.seekp(pos);
            f_out.write(st_out, len);
            pos += len;
            fh.read(bytes, 4096);
        }
    }


    res = EVP_EncryptFinal_ex(ctx, (unsigned char *) st_out, &len);  // get the remaining ct
    if(res != 1) exit(5);
    f_out.seekp(pos);
    f_out.write(st_out, len);

    fh.close();
    f_out.close();

    /* Clean up */
    EVP_CIPHER_CTX_free(ctx);

    printf("Zašifrovaný obrázek pomocí CBC i ECB byl o 8B delší - způsobeno zarovnáním bloků při šifrování (výchozí chování openSSL)\n");
    printf("Při použití ECB módu je původní obrázek snadno rozpoznatelný, to je způsobeno tím, ");
    printf("že v režimu ECB se šifruje každý blok zvlášť -> stejné bloky jsou zašifrovány stejně a díky tomu jsou plochy na obrázku vidět.\n");
    printf("Zatímco v CBC režimu se aktuální blok ovlivní předchozím zašifrovaným blokem a díky tomu výsledek vypadá mnohem nahodileji než u ECB.\n");

    exit(0);
}
