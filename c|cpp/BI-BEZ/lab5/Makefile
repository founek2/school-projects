all: clean build

build:
	g++ -Wall lab5.cpp -lcrypto -lssl -o lab5

run: build
	./lab5

clean:
	rm -f lab5 webpage.html certificate.pem

