XX      := -c++
CXXFLAGS := -pedantic-errors -Wall -Wextra -Werror

all: build

build: clean hexToAscii asciiToHex

hexToAscii:
	$(CXX) $(CXXFLAGS) hexToAscii.cpp -o hexToAscii

asciiToHex:
	 $(CXX) $(CXXFLAGS) asciiToHex.cpp -o asciiToHex

clean:
	 rm -f {hexToAscii,asciiToHex}
