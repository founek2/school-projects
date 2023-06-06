#ifndef __PROGTEST__

#include <cstdio>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <cassert>
#include <cmath>
#include <cctype>
#include <climits>

#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <list>
#include <algorithm>
#include <functional>
#include <memory>

using namespace std;

const uint16_t ENDIAN_LITTLE = 0x4949;
const uint16_t ENDIAN_BIG = 0x4d4d;

#endif /* __PROGTEST__ */

uint16_t convert(char lo, char hi, uint16_t endien) {
    if (endien == ENDIAN_LITTLE)
        return (unsigned char) lo | uint16_t((unsigned char)hi) << 8;
    else
        return (unsigned char)hi | uint16_t((unsigned char)lo) << 8;
}

bool createFile(const char *dstFileName, char *header, char **v2, int width, int height) {
    ofstream OutFile;
    OutFile.open(dstFileName, ios::binary);

    if (!OutFile.is_open())
        return false;

    OutFile.write(header, sizeof(header));
    for (int i = 0; i < height; ++i) {
        OutFile.write(&(v2[i][0]), width);
    }

    if (OutFile.bad())
        return false;

    OutFile.close();

    return true;
}

void flipHoriz(char **v2, int height, int width, int pixelWidth) {

    for (int i = 0; i < height; ++i) {
        int startIndex;
        int endIndex;
        for (startIndex = 0, endIndex = width - pixelWidth;
             startIndex < endIndex; startIndex += pixelWidth, endIndex -= pixelWidth) {
            for (int j = 0; j < pixelWidth; ++j) {
                char tmp = v2[i][startIndex + j];
                v2[i][startIndex + j] = v2[i][endIndex + j];
                v2[i][endIndex + j] = tmp;
            }
        }
    }
}

void flipVert(char **v2, int height) {
    int startIndex = 0;
    int endIndex = 0;
    for (startIndex = 0, endIndex = height - 1; startIndex < endIndex; ++startIndex, --endIndex) {
        char *tmp = v2[startIndex];
        v2[startIndex] = v2[endIndex];
        v2[endIndex] = tmp;
    }
}

void printFile(char **v2, int width, int height) {
    for (int i = 0; i < height; ++i) {
        for (int j = 0; j < width; j++) {
            cout << hex << setw(2) << setfill('0') << (int) v2[i][j];
            cout << " ";
        }
        cout << endl;
    }
}

bool checkEndian(uint16_t endian) {
    return endian == ENDIAN_LITTLE || endian == ENDIAN_BIG;
}

bool flipImage(const char *srcFileName,
               const char *dstFileName,
               bool flipHorizontal,
               bool flipVertical) {
    std::ifstream input(srcFileName, std::ios::binary);

    if (!input.is_open())
        return false;

    int headerLen = 8;
    char *header = new char[headerLen];
    input.seekg(0, input.end);
    int fileSize = input.tellg();
    input.seekg(0, input.beg);

    input.read(header, headerLen);


    uint16_t endian = convert(header[0], header[1], LITTLE_ENDIAN);
    if (!checkEndian(endian))
        return false;

    int width = convert(header[2], header[3], endian);
    int height = convert(header[4], header[5], endian);

    if (width == 0 || height == 0)
        return false;

    uint16_t pixelFormat = convert(header[6], header[7], endian);

    // allowed formats
    char **v2;
    int pixelWidth = 0;
    uint16_t canalNum = pixelFormat & (uint16_t) 0b11;

    uint16_t restZeros = pixelFormat & (uint16_t) 0b1111111111100000;
    if (restZeros != 0) return false;

    if (canalNum == 0)
        pixelWidth = 1;
    else if (canalNum == 0b10)
        pixelWidth = 3;
    else if (canalNum == 0b11)
        pixelWidth = 4;
    else return false;

    uint16_t canalWidth = pixelFormat & (uint16_t) 0b11100;

    bool pixelPerCanal = false;
    // TODO počítat s bitovými kanály....
    if (canalWidth == 0b000) {
        pixelPerCanal = true;
        pixelWidth *= 1; // TODO jeden bit na kanál
        return false;
    }
    else if (canalWidth == 0b01100)// 1B na kanál
        pixelWidth *= 1;
    else if (canalWidth == 0b10000) // 2B na kanál
        pixelWidth *= 2;
    else return false;


    v2 = new char *[height];
    if (pixelPerCanal)
        width = (int) ceil((width*pixelWidth)/8.f);
    else
        width = width * pixelWidth;


    if (fileSize != (height * width) + 8)
        return false;

    for (int k = 0; k < height; ++k) {
        v2[k] = new char[width];

        for (int j = 0; j < width; j += pixelWidth) {
            input.read(&(v2[k][j]), pixelWidth);
        }
    }

    input.close();

    printFile(v2, width, height);
    if (flipHorizontal)
        flipHoriz(v2, height, width, pixelWidth);
    if (flipVertical)
        flipVert(v2, height);
    //cout << endl;
    // printFile(v2, width, height);
    if (!createFile(dstFileName, header, v2, width, height))
        return false;
    // cout << "dokončeno";

    for (int k = 0; k < height; ++k) {

        delete[] v2[k];
    }
    delete[] v2;

    return true;
}

#ifndef __PROGTEST__

bool identicalFiles(const char *p1,
                    const char *p2) {
    std::ifstream f1(p1, std::ios::binary);
    std::ifstream f2(p2, std::ios::binary);

    assert(f1.is_open());
    assert(f2.is_open());

    f1.seekg(0, f1.end);
    int sizeF1 = f1.tellg();
    f1.seekg(0, f1.beg);
    char *fileArray1 = new char[sizeF1];
    f1.read(fileArray1, sizeF1);

    f2.seekg(0, f2.end);
    int sizeF2 = f2.tellg();

    f2.seekg(0, f2.beg);
    char *fileArray2 = new char[sizeF2];
    f2.read(fileArray2, sizeF2);

/*     cout << "output>" << endl;
     for (int i = 0; i < sizeF1; ++i) {
         cout << hex << (int)fileArray1[i];
     }
     cout << endl << "exptected" << endl;
     for (int i = 0; i < sizeF2; ++i) {
         cout << hex << (int)fileArray2[i];
     }

    if (sizeF2 != sizeF1) {
        cout << endl <<"differSize> " <<  sizeF1 << ", " << sizeF2;
        return false;
    }*/

    //seek back to beginning and use std::equal to compare contents

    bool output = std::equal(fileArray1, fileArray1 + sizeF1, fileArray2);
    if (!output)
        cout << "files differ" << endl;
    return output;
}

int main(void) {

    // assert(identicalFiles("ref_00.img", "output_00.img"));

    assert(flipImage("input_00.img", "output_00.img", true, false));

    assert (identicalFiles("output_00.img", "ref_00.img"));


    assert (flipImage("input_01.img", "output_01.img", false, true)
            && identicalFiles("output_01.img", "ref_01.img"));

    assert (flipImage("input_02.img", "output_02.img", true, true)
            && identicalFiles("output_02.img", "ref_02.img"));

    assert (flipImage("input_03.img", "output_03.img", false, false)
            && identicalFiles("output_03.img", "ref_03.img"));

    assert (flipImage("input_04.img", "output_04.img", true, false)
            && identicalFiles("output_04.img", "ref_04.img"));

     assert (flipImage("input_05.img", "output_05.img", true, true)
             && identicalFiles("output_05.img", "ref_05.img"));

     assert (flipImage("input_06.img", "output_06.img", false, true)
             && identicalFiles("output_06.img", "ref_06.img"));

     assert (flipImage("input_07.img", "output_07.img", true, false)
             && identicalFiles("output_07.img", "ref_07.img"));

     assert (flipImage("input_08.img", "output_08.img", true, true)
             && identicalFiles("output_08.img", "ref_08.img"));


     assert (!flipImage("input_09.img", "output_09.img", true, false));

    assert (flipImage("input_10.img", "output_10.img", true, false));

    // extra inputs (optional & bonus tests)
    assert (flipImage("extra_input_00.img", "extra_out_00.img", true, false)
            && identicalFiles("extra_out_00.img", "extra_ref_00.img"));
    assert (flipImage("extra_input_01.img", "extra_out_01.img", false, true)
            && identicalFiles("extra_out_01.img", "extra_ref_01.img"));
    assert (flipImage("extra_input_02.img", "extra_out_02.img", true, false)
            && identicalFiles("extra_out_02.img", "extra_ref_02.img"));
    assert (flipImage("extra_input_03.img", "extra_out_03.img", false, true)
            && identicalFiles("extra_out_03.img", "extra_ref_03.img"));
    assert (flipImage("extra_input_04.img", "extra_out_04.img", true, false)
            && identicalFiles("extra_out_04.img", "extra_ref_04.img"));
    assert (flipImage("extra_input_05.img", "extra_out_05.img", false, true)
            && identicalFiles("extra_out_05.img", "extra_ref_05.img"));
    assert (flipImage("extra_input_06.img", "extra_out_06.img", true, false)
            && identicalFiles("extra_out_06.img", "extra_ref_06.img"));
    assert (flipImage("extra_input_07.img", "extra_out_07.img", false, true)
            && identicalFiles("extra_out_07.img", "extra_ref_07.img"));
    assert (flipImage("extra_input_08.img", "extra_out_08.img", true, false)
            && identicalFiles("extra_out_08.img", "extra_ref_08.img"));
    assert (flipImage("extra_input_09.img", "extra_out_09.img", false, true)
            && identicalFiles("extra_out_09.img", "extra_ref_09.img"));
    assert (flipImage("extra_input_10.img", "extra_out_10.img", true, false)
            && identicalFiles("extra_out_10.img", "extra_ref_10.img"));
    assert (flipImage("extra_input_11.img", "extra_out_11.img", false, true)
            && identicalFiles("extra_out_11.img", "extra_ref_11.img"));
    return 0;
}

#endif /* __PROGTEST__ */
