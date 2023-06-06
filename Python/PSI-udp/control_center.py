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
