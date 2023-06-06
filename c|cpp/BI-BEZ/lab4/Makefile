all: clean build

build:
	g++ -Wall encrypt.cpp custom.cpp -lcrypto -o encrypt
	g++ -Wall decrypt.cpp custom.cpp -lcrypto -o decrypt

run: build
	./encrypt Mad_scientist.bmp pubkey.pem output.encrypted
	./decrypt output.encrypted privkey.pem Mad_scientist_decrypted.bmp

clean:
	rm -f encrypt decrypt output.encrypted Mad_scientist_decrypted.bmp

