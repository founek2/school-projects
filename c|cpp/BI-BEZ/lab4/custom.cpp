//
// Created by Martin Skalický on 14/04/2020.
//

#include <openssl/evp.h>
#include "custom.h"
#include <fstream>

CUSTOM::CUSTOM(const std::string &cipher_name, int eklen) :
        ek_pos(sizeof(struct CUSTOM)),
        ek_len(eklen),
        iv_len(EVP_MAX_IV_LENGTH),
        iv_pos(ek_pos + ek_len),
        data_pos(iv_pos + iv_len) {
    if (cipher_name == "AES-128-CBC")
        cipher_type = 0;
    else if (cipher_name == "AES-128-ECB")
        cipher_type = 1;
    else
        throw std::runtime_error("Invalid cipher type");
}
std::ofstream& operator << (std::ofstream& of, const CUSTOM& c)
{
    of.write( reinterpret_cast<const char *>(&c.cipher_type), sizeof(c.cipher_type));
    of.write( reinterpret_cast<const char *>(&c.ek_pos), sizeof(c.ek_pos));
    of.write( reinterpret_cast<const char *>(&c.ek_len), sizeof(c.ek_len));
    of.write( reinterpret_cast<const char *>(&c.iv_pos), sizeof(c.iv_pos));
    of.write( reinterpret_cast<const char *>(&c.iv_len), sizeof(c.iv_len));
    of.write( reinterpret_cast<const char *>(&c.data_pos), sizeof(c.data_pos));
    return of;
}

std::ifstream& operator >> (std::ifstream& in_f, CUSTOM& c){
    in_f.read( reinterpret_cast<char *>(&c.cipher_type), sizeof(c.cipher_type));
    in_f.read( reinterpret_cast<char *>(&c.ek_pos), sizeof(c.ek_pos));
    in_f.read( reinterpret_cast<char *>(&c.ek_len), sizeof(c.ek_len));
    in_f.read( reinterpret_cast<char *>(&c.iv_pos), sizeof(c.iv_pos));
    in_f.read( reinterpret_cast<char *>(&c.iv_len), sizeof(c.iv_len));
    in_f.read( reinterpret_cast<char *>(&c.data_pos), sizeof(c.data_pos));
    return in_f;
}

