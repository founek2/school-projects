//
// Created by Martin Skalický on 14/04/2020.
//

#ifndef BI_BEZ_LAB2_CUSTOM_H
#define BI_BEZ_LAB2_CUSTOM_H


#include <string>

struct CUSTOM {
    CUSTOM() = default;
    CUSTOM(const std::string &cipher_name, int eklen);
    unsigned char cipher_type;
    unsigned int ek_pos;
    int ek_len;
    unsigned int iv_len;
    unsigned int iv_pos;
    unsigned int data_pos;

    friend std::ofstream& operator << (std::ofstream& of, const CUSTOM& c);
    friend std::ifstream& operator >> (std::ifstream& in_f, CUSTOM& c);

};


#endif //BI_BEZ_LAB2_CUSTOM_H
