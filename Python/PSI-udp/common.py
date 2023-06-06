from ipaddress import ip_address
from pathlib import Path
from typing import NamedTuple
from argparse import ArgumentTypeError
from collections import OrderedDict

CMD_DOWNLOAD_IMAGE = 0x01.to_bytes(1, byteorder="big")
CMD_UPLOAD_FIRMWARE = 0x02.to_bytes(1, byteorder="big")
WINDOW_WIDTH = 2040
SYN_TIMEOUT = 100e-3  # ms

PHOTO_FILE = "foto.png"

# RST = 0b1
FIN = 0b10
SYN = 0b100


def is_ip_addr(val):
    ip_address(val)
    return val


def check_file_exists(path):
    if Path(path).is_file():
        return path
    raise ArgumentTypeError(f'File at path: "{path}" does not exists!')


# 4B 2B 2B 1B 0-255B
class Packet:
    id: int
    seq: int
    ack: int
    meta: int
    data: bytes

    def __init__(self, id, seq, ack, meta, data):
        self.id = id
        self.seq = seq
        self.ack = ack
        self.meta = meta
        self.data = data

    @classmethod
    def from_bytes(cls, bytes_to_parse):
        return cls(int.from_bytes(bytes_to_parse[0:4], byteorder="big"),
                   int.from_bytes(bytes_to_parse[4:6], byteorder="big"),
                   int.from_bytes(bytes_to_parse[6:8], byteorder="big"),
                   bytes_to_parse[8],
                   bytes_to_parse[9:])

    def to_bytes(self):
        return self.id.to_bytes(4, byteorder="big") + self.seq.to_bytes(2, byteorder="big") + self.ack.to_bytes(2, byteorder="big") + self.meta.to_bytes(1, byteorder="big") + self.data

    def __str__(self):
        return f"Packet(id={self.id}, seq={self.seq}, ack={self.ack}, meta={self.meta}, data_len={len(self.data)}')"

    def attributes(self):
        'Return a new OrderedDict which maps field names to their values'
        return self.id, self.seq, self.ack, self.meta, self.data

def is_RST(meta: int):
    return True if meta & RST else False


def is_FIN(meta: int):
    return True if meta & FIN else False


def is_SYN(meta: int):
    return True if meta & SYN else False

def is_cmd_down(data: bytes):
    return True if data == CMD_DOWNLOAD_IMAGE else False

def is_cmd_up(data: bytes):
    return True if data == CMD_UPLOAD_FIRMWARE else False


def is_valid_meta(meta: int):
    cnt = 0
    if is_RST(meta):
        cnt += 1

    if is_FIN(meta):
        cnt += 1

    if is_SYN(meta):
        cnt += 1

    if meta == 0:
        cnt += 1

    return True if cnt == 1 else False


def is_valid_id(id: int):
    return True if id != 0 else False


def is_valid_cmd(data: bytes):
    return True if len(data) == 1 and (data == CMD_DOWNLOAD_IMAGE or data == CMD_UPLOAD_FIRMWARE) else False

def to_bin(data: bytes):
    return bin(int.from_bytes(data, byteorder="big"))
