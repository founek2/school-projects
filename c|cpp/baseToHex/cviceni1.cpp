#include <iostream>

char toValue(char letter){
    if (48 <= letter && letter <= 57)   // 0 -> 9
        return letter - 48;
    else if (97 <= letter && letter <= 102){    // a -> f
        return 10 + letter - 97;
    } else{
        throw std::invalid_argument("Invalid character");
    }
}

int main()
{
    const char hex1[] = {'f', 'c', '5', '7', 'c', '9', 'c', 'f', 'd', 'a', 'c', '7', '2', '0', '2', '6', '9', '4', 'e', '8', 'c', '2', '7', '1', '2', 'd', '1', 'f', '0', 'e', '8', 'e', '2', '1', 'b', '8', 'c', '6', 'f', '6', 'd', '6', '1', '2', 'd', '6', 'c', '9', '6', '8', '4', '7', 'e', '4', '2', 'e', '6', '2', '5', '7', 'b', '3', 'f', '9', 'e', 'a', '9', '8', '4', '6', '6', 'c', 'f', 'c', '8', '7', '6', '1', 'e', 'd', '5', 'e', '0', '3', '0', '8', '3', '1', '1', 'd', 'e', 'b', '1', '5'};
    const char hex2[] = {'a', '8', '3', 'f', 'a', '0', 'b', 'c', 'f', 'a', 'a', 'e', '5', '3', '0', '6', 'f', '5', 'c', '8', 'b', '1', '1', '4', '4', 'e', '6', 'd', '6', 'b', 'f', 'a', '0', '1', 'c', 'c', 'a', '3', '8', 'e', 'a', '2', '3', 'c', 'f', '6', '8', '7', '0', '7', '2', '5', '8', 'b', '4', 'a', '1', 'b', '7', '7', 'c', '0', '9', '1', '8', '5', 'e', 'd', '2', 'a', '0', '8', 'd', 'c', 'f', '5', '0', '4', '8', 'c', '3', 'a', '2', '3', '7', 'c', '5', '9', '7', '4', '9', '8', '3', 'b'};

    char bytes1[sizeof(hex1)];
    char bytes2[sizeof(hex1)];

    for (int i = 0; i< sizeof(bytes1); i+= 2) {
        char letter = (toValue(hex1[i]) << 4) + toValue(hex1[i+1]);
        bytes1[i] = letter;

        letter = (toValue(hex2[i]) << 4) + toValue(hex2[i+1]);
        bytes2[i] = letter;
    }


    for (int i = 0; i< sizeof(bytes1); i++) {
        char letter = bytes1[i] ^ bytes2[i];
        if ((97 <= letter && letter <= 122) || (65 <= letter &&  letter <= 90)) // a –> z,  A -> Z
        std::cout << letter;
    }
    return 0;
}
