run: hash stream

hash:
	g++ -Wall hash.cpp -lcrypto -o hash
	./hash

stream:
	g++ -Wall hash.cpp -lcrypto -o stream
	echo "8ca98a95a54bc90fb57c7bcdb04ac2facdeb967cbd09b9e73b9167afeecb\na0a48394e05edb17b96430d5bc4ec3491db99f78babc6fe923c579b083a7" | ./stream
