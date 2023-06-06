import asyncio
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
