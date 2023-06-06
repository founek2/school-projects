from photo_client import DataStore
from common import Packet


def test_data_store():
    store = DataStore()
    assert 4 == store.add(Packet(131313, 0, 0, 0, b"ahoj"))
    assert 4 == store.add(Packet(131313, 8 + 255, 0, 0, b"ahoj"))
    assert 8 == store.add(Packet(131313, 4, 0, 0, b"ahoj"))
    assert (8 + 255+ 4) == store.add(Packet(131313, 8, 0, 0, b"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
