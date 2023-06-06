#include <iostream>

int main()
{
    const int LEN = 200;
    char letters[LEN];
    std::cin.getline(letters, LEN);

    for (size_t i = 0; i < strlen(letters); i++) {
        std::cout << std::hex << (int)letters[i];
    }
    std::cout << std::endl;
    return 0;
}
