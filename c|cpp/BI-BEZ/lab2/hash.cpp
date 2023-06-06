/**
 * Martin Skalický
 */

#include <stdio.h>
#include <string.h>
#include <openssl/evp.h>

int main(int argc, char *argv[]){

    int i, res;
    // char text[] = "Text for hash.";
    char hashFunction[] = "sha1";  // chosen hash function ("sha1", "md5" ...)
    EVP_MD_CTX *ctx;  // context structure
    const EVP_MD *type; // hash function type
    unsigned char hash[EVP_MAX_MD_SIZE]; // char array for hash - 64 bytes (max for sha 512)
    int length;  // resulting hash length

    /* Initialization of OpenSSL hash function list */
    OpenSSL_add_all_digests();
    /* Lookup of the needed hash function */
    type = EVP_get_digestbyname(hashFunction);

    /* If NULL returned, hash does not exist */
    if(!type) {
        printf("Hash %s does not exist.\n", hashFunction);
        exit(1);
    }
    unsigned char firstB = 0xCA;
    unsigned char secB = 0xFE;

    int counter = 0;
    printf("Starting look for hash starting with 0xCAFE\n");
    while((hash[0] != firstB || hash[1] != secB) && counter < INT_MAX ) {
        char text2[11];
        sprintf(text2,"%d", counter);

        ctx = EVP_MD_CTX_create(); // create context for hashing
        if (ctx == NULL) exit(2);

        /* Hash the text */
        res = EVP_DigestInit_ex(ctx, type, NULL); // context setup for our hash type
        if (res != 1) exit(3);
        res = EVP_DigestUpdate(ctx, text2, strlen(text2)); // feed the message in
        if (res != 1) exit(4);
        res = EVP_DigestFinal_ex(ctx, hash, (unsigned int *) &length); // get the hash
        if (res != 1) exit(5);

        EVP_MD_CTX_destroy(ctx); // destroy the context

        /* Print the resulting hash */
        counter++;
    }

    printf("Hash was found for text=%d: ",counter);
    for (i = 0; i < length; i++) {
        printf("%02x", hash[i]);
    }
    printf("\n");
    exit(0);
}