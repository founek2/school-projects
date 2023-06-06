all: clean run

run: encrypt decrypt

clean:
	rm -f encrypt decrypt

encrypt:
	g++ -Wall encrypt.cpp -lcrypto -o encrypt
	./encrypt ECB
	./encrypt CBC

decrypt:
	g++ -Wall decrypt.cpp -lcrypto -o decrypt
	./decrypt ECB
	./decrypt CBC
