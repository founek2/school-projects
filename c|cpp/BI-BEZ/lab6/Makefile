all: clean build

build:
	g++ -Wall main.cpp -lcrypto -lssl -o lab6

run: build
	./lab6

clean:
	rm -f lab6 webpage.html certificate.pem

