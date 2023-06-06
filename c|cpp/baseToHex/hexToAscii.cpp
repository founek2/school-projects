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
    const int LEN = 200;
    char bytes[LEN];
    std::cin >> bytes;

    const int textLen = strlen(bytes);
    if (textLen % 2 == 1){
        printf("invalid Lenght");
        return 1;
    }

    for (int i = 0; i < textLen; i += 2) {
        char letter = (toValue(bytes[i]) << 4) + toValue(bytes[i+1]);
        std::cout << letter;
    }
    std::cout << std::endl;
    return 0;
}
