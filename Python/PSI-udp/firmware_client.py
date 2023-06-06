from common import *
import asyncio
import time
from base import Base
from typing import NamedTuple


class Ack(NamedTuple):
    ack: int = -1
    count: int = 0


class DataStore:
    def __init__(self, path: str):
        with open(path, "rb") as f:
            self.data = f.read()

        self.ptr = 0
        self.data_len = len(self.data)


class UploadFirmwareClient(Base):
    def __init__(self, on_con_lost, firmware_path: str):
        super().__init__(CMD_UPLOAD_FIRMWARE, on_con_lost)

        self.store = DataStore(firmware_path)
        self.same_ack_counter = Ack(0, 0)  # ack_number, count
        self.last_ack = 0
        self.last_move_t: int = 0
        self.end = False

    def datagram_received(self, data: bytes,
                          addr):
        packet = Packet.from_bytes(data)
        id, seq, ack, meta, data = packet.attributes()

        print(f"id={id} seq={seq} ack={ack} meta={bin(meta)} data={bin(int.from_bytes(data, 'big'))}")

        if not self.in_sync and self.id != id:
            return

        # packet validation
        if not is_valid_meta(meta) or not is_valid_id(id) or (is_FIN(meta) and len(data) > 0):
            print("invalid")
            return self.send_reset()

        if is_RST(meta):    # Close connection on RST
            print("DEBUG>")
            print(f"ptr={self.store.ptr} len={len(self.store.data)}")
            print("Received RST -> closing")
            self.close()
            return

        if self.in_sync is True:
            if is_SYN(meta):     # On SYN save ID and init process
                if not is_valid_cmd(data):
                    return self.send_reset()
                if not is_cmd_up(data):
                    return

                print("Received SYN")
                self.synced(id)

                self.send_window()
                asyncio.get_running_loop().create_task(self.timeout_move())     # Start timeout check on window move
            return

        if is_FIN(meta):
            print("Received FIN -> closing")
            self.close()
            return

        num, count = self.same_ack_counter
        if num == ack:                                  # Check for 3x same ack -> send one packet
            self.same_ack_counter = Ack(num, count + 1)

            if self.same_ack_counter.count >= 3:
                self.last_move_t = time.time()
                print("3x same ack -> sending one")
                return self.send_one()
        else:
            self.same_ack_counter = Ack(ack, 1)

        if self.end and self.store.data_len % 65536 == ack:         # When last packed is ACK > send FIN
            self.last_ack = ack
            print("last>", self.last_ack, ack, self.store.data_len % 65536)
            asyncio.get_running_loop().create_task(self.repeat_send(Packet(id, ack, 0, FIN, bytes())))
            return

        if self.last_ack < ack:     # move window
            diff = ack - self.last_ack
            self.last_ack = ack
            self.move_window(diff)
        elif self.last_ack > ack and self.last_ack + WINDOW_WIDTH > ack:    # ack overflow -> compute shift
            last = 0
            while self.last_ack + last <= 65535:
                last += 255

            diff = last + ack - (self.last_ack + last) % 65536
            print(f"ack overflow last={self.last_ack} ack={ack} diff={diff} over={(self.last_ack + last) % 65536}")

            self.last_ack = ack
            self.move_window(diff)

    async def timeout_move(self):
        print("timouting")
        await asyncio.sleep(SYN_TIMEOUT)
        if time.time() - self.last_move_t > SYN_TIMEOUT:
            print("move TIMEOUT")
            self.send_window()

        if self.last_ack != self.store.data_len % 65536:
            asyncio.get_running_loop().create_task(self.timeout_move())

    def send_window(self):
        ptr = self.store.ptr
        data = self.store.data
        seq = ptr % 65536
        for i in range(0, WINDOW_WIDTH, 255):
            chunk = data[ptr + i:ptr + i + 255]
            if len(chunk) == 0:
                self.end = True
                break
            self.send(Packet(self.id, seq, 0, 0, chunk))
            seq = (seq + 255) % 65536

    def move_window(self, num):
        print(f"Moving by {num}")
        self.store.ptr += num
        ptr = self.store.ptr
        data = self.store.data
        seq = (ptr + WINDOW_WIDTH) % 65536

        self.last_move_t = time.time()

        if ptr + WINDOW_WIDTH >= self.store.data_len:
            self.end = True
            return

        i = 0
        while i < num:
            self.send(
                Packet(self.id, (seq + i) % 65536, 0, 0, data[ptr + WINDOW_WIDTH + i:ptr + WINDOW_WIDTH + i + 255])
            )
            i += 255

    def send_one(self):
        ptr = self.store.ptr
        self.send(Packet(self.id, ptr % 65536, 0, 0, self.store.data[ptr:255]))