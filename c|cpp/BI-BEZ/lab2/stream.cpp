/**
 * Martin Skalický
 */

#include <cstdlib>
#include <openssl/evp.h>
#include <cstring>
#include <string>
#include <iostream>

unsigned char toValue(unsigned char letter){
    if (48 <= letter && letter <= 57)   // 0 -> 9
        return letter - 48;
    else if (97 <= letter && letter <= 102){    // a -> f
        return 10 + letter - 97;
    } else{
        throw std::invalid_argument("Invalid character");
    }
}


int main(void) {

    printf("Šifra RC4 nepoužívá IV\n");
    printf("vyzkoušené kombinace st/IV:\n");
    printf("83f5346144a92f17a784b206d7/muj klic\n");
    printf("83f5346144a92f17a784b206d7/\n");
    printf("83f5346144a92f17a784b206d7/kekel\n\n");



    //---------------------------------------------//
    printf("---------------------------------------------\nPLAYING WITH CIPHER\n");

    int res;
    unsigned char ot[1024] = "Moje super tajná zpráva...__?#";  // 8ca98a95a54bc90fb57c7bcdb04ac2facdeb967cbd09b9e73b9167afeecb
    unsigned char ot2[1024] = "abcdefghijklmnopqrstuvwxyz0123";  // a0a48394e05edb17b96430d5bc4ec3491db99f78babc6fe923c579b083a7
    unsigned char st[1024];  // sifrovany text
    unsigned char st2[1024];  // sifrovany text
    unsigned char key[EVP_MAX_KEY_LENGTH] = "Lalal_muj_super_klic_lala";  // klic pro sifrovani
    unsigned char iv[EVP_MAX_IV_LENGTH] = "kekel";  // inicializacni vektor
    const char cipherName[] = "RC4";
    const EVP_CIPHER * cipher;

    OpenSSL_add_all_ciphers();
    /* sifry i hashe by se nahraly pomoci OpenSSL_add_all_algorithms() */
    cipher = EVP_get_cipherbyname(cipherName);
    if(!cipher) {
        printf("Sifra %s neexistuje.\n", cipherName);
        exit(1);
    }

    int otLength = strlen((const char*) ot);
    int ot2Length = strlen((const char*) ot2);
    int stLength = 0;
    int st2Length = 0;
    int tmpLength = 0;

    EVP_CIPHER_CTX *ctx; // context structure
    ctx = EVP_CIPHER_CTX_new();
    if (ctx == NULL) exit(2);

    printf("OT: %s\n", ot);

    /* Sifrovani */
    res = EVP_EncryptInit_ex(ctx, cipher, NULL, key, iv);  // context init - set cipher, key, init vector
    if(res != 1) exit(3);
    res = EVP_EncryptUpdate(ctx,  st, &tmpLength, ot, otLength);  // encryption of pt
    if(res != 1) exit(4);
    stLength += tmpLength;
    res = EVP_EncryptFinal_ex(ctx, st + stLength, &tmpLength);  // get the remaining ct
    if(res != 1) exit(5);
    stLength += tmpLength;

    printf ("Zasifrovano %d znaku.\n", stLength);
    printf("ST: %s\nOT: %s\n", st, ot);
    printf("hex st: ");
    for (int i = 0; i < ot2Length; i++) {
        printf("%02x", st[i]);
    }
    printf("\n");

    /* Sifrovani */
    res = EVP_EncryptInit_ex(ctx, cipher, NULL, key, iv);  // context init - set cipher, key, init vector
    if(res != 1) exit(3);
    res = EVP_EncryptUpdate(ctx,  st2, &tmpLength, ot2, ot2Length);  // encryption of pt
    if(res != 1) exit(4);
    st2Length += tmpLength;
    res = EVP_EncryptFinal_ex(ctx, st2 + st2Length, &tmpLength);  // get the remaining ct
    if(res != 1) exit(5);
    st2Length += tmpLength;

    printf ("Zasifrovano %d znaku.\n", st2Length);
    printf("ST2: %s\nOT2: %s\n", st2, ot2);
    printf("hex st: ");
    for (int i = 0; i < ot2Length; i++) {
        printf("%02x", st2[i]);
    }
    printf("\n");

    /* Desifrovani */
    res = EVP_DecryptInit_ex(ctx, cipher, NULL, key, iv);  // nastaveni kontextu pro desifrovani
    if(res != 1) exit(6);
    res = EVP_DecryptUpdate(ctx, ot, &tmpLength,  st, stLength);  // desifrovani st
    if(res != 1) exit(7);
    otLength += tmpLength;
    res = EVP_DecryptFinal_ex(ctx, ot + otLength, &tmpLength);  // dokonceni (ziskani zbytku z kontextu)
    if(res != 1) exit(8);
    otLength += tmpLength;

    /* Clean up */
    EVP_CIPHER_CTX_free(ctx);
    printf("\ncalculating key permutation from OT2/ST2 (using XOR)\n");
    unsigned char d_key[1024];
    for (int i = 0; i< ot2Length; ++i){
        d_key[i]= ot2[i] ^ st2[i];
    }
    printf("Key permutation: ");
    for (int i = 0; i < ot2Length; i++) {
        printf("%02x", d_key[i]);
    }
    printf("\nDecrypting... ST\n");
    printf("Decrypto message: ");
    unsigned char decrypted[1024];
    for (int i = 0; i< ot2Length; ++i){
        decrypted[i] = st[i] ^ d_key[i];
        printf("%c", decrypted[i]);
    }

    printf("\n");
    // exit(0);

    //---------------------------------------------//
    printf("---------------------------------------------\n");
    // DECRYPTION FROM INPUT
    printf("\n\nProcessing HEX messages from input>\n");
    unsigned char o_t2[1024] = "abcdefghijklmnopqrstuvwxyz0123";
    unsigned char s_t[1024];
    unsigned char s_t2[1024];
    unsigned buffer_len;

    unsigned char buffer[2048];
    std::cin >> buffer;
    buffer_len = strlen((const char *) buffer);
    for (int j = 0; j < buffer_len; j+=2) {
        char letter = (toValue(buffer[j]) << 4) + toValue(buffer[j+1]);
        s_t[j/2] = letter;
    }
    s_t[buffer_len / 2] = '\0';


    std::cin >> buffer;
    for (int j = 0; j < buffer_len; j+=2) {
        char letter = (toValue(buffer[j]) << 4) + toValue(buffer[j+1]);
        s_t2[j/2] = letter;
    }
    s_t2[buffer_len / 2] = '\0';

    // unsigned st_len = strlen((const char*)s_t);
    unsigned st2_len = buffer_len / 2;

    printf("\ncalculating key permutation from OT2/ST2 (using XOR)\n");
    unsigned char d_key2[1024];
    for (int i = 0; i< st2_len; ++i){
        d_key2[i]= o_t2[i] ^ s_t2[i];
    }
    printf("Key permutation: ");
    for (int i = 0; i < st2_len; i++) {
        printf("%02x", d_key2[i]);
    }
    printf("\nDecrypting... ST\n");
    printf("Decrypto message: ");
    unsigned char decrypted2[1024];
    for (int i = 0; i < st2_len; ++i){
        decrypted2[i] = s_t[i] ^ d_key2[i];
        printf("%c", decrypted2[i]);
    }

    printf("\n");
}