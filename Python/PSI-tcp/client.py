import asyncore, socket
from common import *
import re

USER_NAME = "Mnau!"

k = calc_hash(USER_NAME, SERVER_KEY)
print(k)

def calc_hash(text, key):
    values = [ord(c) for c in text]
    hash_name = (sum(values) * 1000) % 65536
    return (hash_name + key) % 65536


class HTTPClient(asyncore.dispatcher):

    def __init__(self, host, path):
        asyncore.dispatcher.__init__(self)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.connect((host, 8181))
        self.send_str(f'{USER_NAME}\a\b')
        self.auth = 0
        self.user_name = USER_NAME
        self.recharging = False

    def handle_connect(self):
        print("connected")

    def handle_close(self):
        self.close()
        print("closed")

    def handle_read(self):
        raw = self.recv(8192)
        data = raw.decode("ascii")
        print("recieved: ", raw)

        if self.auth == 0:
            code = self.get_code()
            self.auth = 1
            return self.send_str(f'{code}dsad\a\b')

        if not self.recharging:
            self.send_str('RECHARGING\a\b')
            self.recharging = True

    def writable(self):
        return len(self.buffer) > 0

    def handle_write(self):
        sent = self.send(self.buffer)
        self.buffer = self.buffer[sent:]

    def get_code(self):
        return calc_hash(self.user_name, CLIENT_KEY)

    def send_str(self, data):
        print("sending", data)
        self.buffer = data.encode("ascii")
        # self.handle_write()

if __name__ == "__main__":
    client = HTTPClient('localhost', '/')
    asyncore.loop()
