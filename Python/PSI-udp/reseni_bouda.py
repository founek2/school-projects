import asyncio
import argparse
from common import *
from photo_client import DownloadPhotoClient
from firmware_client import UploadFirmwareClient

parser = argparse.ArgumentParser(description='Download photo or upload firmware using UDP connection')
parser.add_argument('server_ip', type=is_ip_addr,
                    help='IP of server for UDP connection')
parser.add_argument('firmware_path', nargs="?",
                    type=check_file_exists,
                    help='Path to new firmware for upload')


async def main(ip_addr: str, port: int, firmware_path=None):
    loop = asyncio.get_running_loop()

    on_con_lost = loop.create_future()

    transport, protocol = await loop.create_datagram_endpoint(
        lambda: DownloadPhotoClient(on_con_lost) if not firmware_path else UploadFirmwareClient(on_con_lost, firmware_path),
        remote_addr=(ip_addr, port))

    # Wait until the protocol signals that the connection
    # is lost and close the transport.
    try:
        await on_con_lost
    finally:
        transport.close()


if __name__ == "__main__":
    args = parser.parse_args()
    print(args.server_ip)
    asyncio.run(main(args.server_ip, 4000, args.firmware_path))
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
        self.send(Packet(self.id, ptr % 65536, 0, 0, self.store.data[ptr:255]))from common import *
from base import Base


def sort_seq(data: Packet):
    return data.seq


class DataStore():
    def __init__(self):
        self.ptr = 0  # ack
        self.buffer = []
        self.buffer2 = []
        self.first = True
        self.data_len = 0
        self.overflow = 0

        self.data = bytearray()

    def add(self, packet: Packet):
        if (packet.seq < self.ptr and self.ptr - packet.seq < WINDOW_WIDTH * 3) or (
                packet.seq > self.ptr + WINDOW_WIDTH * 3):  # check if seq number is behind ack -> ignore
            return self.ptr

        # when ack is near maximum and got packet with overflowed seq
        if self.overflow == 0 and self.ptr > WINDOW_WIDTH * 3 >= packet.seq:
            self.overflow = 1

        use_buf_2 = self.overflow == 1 and packet.seq < WINDOW_WIDTH * 3

        # Check if packet is already in buffer
        for pac in reversed(self.buffer2 if use_buf_2 else self.buffer):
            if pac.seq > pac.seq:
                break
            if pac.seq == packet.seq:
                return self.ptr

        # detected overflow -> add to buffer2
        if use_buf_2:
            self.buffer2.append(packet)
            self.buffer2.sort(key=sort_seq, reverse=True)
            print("buff2", packet.seq)
            return self.ptr

        print("buff1", packet.seq)
        self.buffer.append(packet)
        self.buffer.sort(key=sort_seq, reverse=True)

        # find all continuing bytes and remove them from buffer
        while self.buffer and self.data_len % 65536 == self.buffer[-1].seq:
            d = self.buffer.pop()
            self.add_data(d.data)

        self.ptr = self.data_len % 65536

        if self.overflow and self.ptr <= 255:   # detected overflow and ack just overflowed
            self.overflow = 0
            self.buffer = self.buffer2      # self.buffer is empty -> switch them
            self.buffer2 = []

            # look again for continuing bytes (in other buffer)
            while self.buffer and self.data_len % 65536 == self.buffer[-1].seq:
                d = self.buffer.pop()
                self.add_data(d.data)

            self.ptr = self.data_len % 65536

        return self.ptr

    def add_data(self, data: bytes):
        self.data_len += len(data)
        self.data.extend(data)

    def save_file(self):
        with open(PHOTO_FILE, 'wb') as f:
            f.write(self.data)


class DownloadPhotoClient(Base):
    def __init__(self, on_con_lost):
        super().__init__(CMD_DOWNLOAD_IMAGE, on_con_lost)
        self.store = DataStore()

    def datagram_received(self, data: bytes,
                          addr):
        packet = Packet.from_bytes(data)
        id, seq, ack, meta, data = packet.attributes()

        print(f"id={id} seq={seq} ack={ack} meta={bin(meta)} data={len(data)}")

        if not self.in_sync and self.id != id:
            return

        # packet validation - invalid seq/meta, FIN but contains DATA -> RST
        if not is_valid_meta(meta) or not is_valid_id(id) or (is_FIN(meta) and len(data) > 0):
            print("invalid")
            return self.send_reset()

        if is_RST(meta):
            print("DEBUG>")
            for item in self.store.buffer:
                print(item)
            print("")
            print("BUFFER 2>")
            for item in self.store.buffer2:
                print(item)
            print("")
            print("Received RST -> closing")
            self.close()
            return

        if self.in_sync is True:    # On SYN save ID
            if is_SYN(meta):
                if not is_valid_cmd(data):
                    return self.send_reset()
                if not is_cmd_down(data):
                    return

                print("Received SYN")
                self.synced(id)
            return

        if is_FIN(meta):
            self.send(Packet(id, 0, seq, FIN, bytes()))
            print("Received FIN -> closing")
            self.store.save_file()
            self.close()
            return

        # add tu buffer and send ACK
        conf_seq = self.store.add(packet)
        self.send(Packet(id, 0, conf_seq, 0, bytes()))import asyncio
from common import *
from typing import NamedTuple


class Seq(NamedTuple):
    seq: int = -1
    count: int = 0


class Base(asyncio.BaseProtocol):
    def __init__(self, cmd, on_con_lost):
        self.on_con_lost = on_con_lost
        self.transport = None
        self.in_sync = True
        self.cmd = cmd
        self.id = -1
        self.same_seq = Seq(-1, 0)  # (seq_num, counter)

    def connection_made(self, transport):
        print("connected")
        self.transport = transport
        self.send_sync()

    def send_sync(self):
        print("sending SYNC")
        self.send(Packet(0, 0, 0, SYN, self.cmd))
        asyncio.get_running_loop().create_task(self.timeout_sync())

    def send(self, packet: Packet):
        print("sending", packet)
        self.transport.sendto(packet.to_bytes())

        if self.in_sync or self.cmd == CMD_UPLOAD_FIRMWARE or is_FIN(packet.meta):
            seq, count = self.same_seq
            if packet.seq == seq:
                self.same_seq = Seq(seq, count + 1)

                if self.same_seq.count >= 20:
                    self.send_reset()

            else:
                self.same_seq = Seq(packet.seq, 1)

    def connection_lost(self, exc):
        print('The server closed the connection')
        self.on_con_lost.set_result(True)

    def error_received(self, exc):
        print('Error received:', exc)

    async def timeout_sync(self):
        await asyncio.sleep(SYN_TIMEOUT)
        if self.in_sync is True:
            self.send_sync()

    def close(self):
        print("closing")
        self.transport.close()

    def synced(self, id):
        self.id = id
        self.in_sync = False

    async def repeat_send(self, data: Packet):
        while True:
            self.send(data)
            await asyncio.sleep(SYN_TIMEOUT)

    def send_reset(self):
        self.send(Packet(0, 0, 0, RST, bytes()))
        print("Sending RST -> closing")
        self.close()
