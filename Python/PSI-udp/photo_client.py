from common import *
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
        self.send(Packet(id, 0, conf_seq, 0, bytes()))