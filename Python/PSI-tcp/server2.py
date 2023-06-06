import asyncio
from common import *
import time
from signal import SIGINT, SIGTERM

import logging

logging.basicConfig(filename='server2.log', level=logging.INFO)


WHOLE_MESSAGES_REGEX = "(?:(?<=\a\b)|(?<=^)).*?\a\b"
PARTIAL_MESSAGE_REGEX = "(?:(?<=\a\b)|(?<=^))((?!\a\b).)+?$"
CONTAINS_RECHARGING_REGEX = "(?:(?<=\a\b)|(?<=^))RECHARGING\a\b"

class EchoServerProtocol(asyncio.Protocol):
    authenticated = False
    auth_command = "CLIENT_USERNAME"
    user_name = ""
    position = Position(None, None)
    direction = None
    recharging = Recharge(False, 0)
    will_recharge = True
    finding_message = False
    buffer = ""
    send_buffer = []
    last_message = 0

    def connection_made(self, transport):
        peername = transport.get_extra_info('peername')
        print('Connection from {}'.format(peername))
        self.transport = transport

    def data_received(self, raw):
        self.last_message = time.time()
        asyncio.get_running_loop().create_task(self.timeout())

        data = self.buffer + raw.decode("ascii")

        print("recv:", raw)

        queue = re.findall(WHOLE_MESSAGES_REGEX, data)
        partial = re.search(PARTIAL_MESSAGE_REGEX, data)
        self.buffer = ""
        self.will_recharge = bool(re.search(CONTAINS_RECHARGING_REGEX, data))
        for msg in queue:
            print("msg", msg.encode('ascii'))
            self.process_message(msg)

        if partial is not None:
            print("partial", partial.group())
            self.should_add_to_buffer(partial.group())

        self.will_recharge = False

    def process_message(self, data):
        if self.recharging.state and not CLIENT["CLIENT_FULL_POWER"](data):
            return self.logic_error()

        if CLIENT["CLIENT_RECHARGING"](data) and not self.recharging.state:
            self.recharging = Recharge(True, time.time())
            return asyncio.get_running_loop().create_task(self.timeout_recharging())

        elif CLIENT["CLIENT_FULL_POWER"](data):
            self.recharging = Recharge(False, 0)
            return self.flush_send_buffer()

        if not self.authenticated:
            return self.process_authentication(data)

        # Authenticated
        if self.finding_message and CLIENT["CLIENT_OK"](data):
            new_pos = get_position(data)
            print("new pos1", new_pos)
            self.position = new_pos

            return self.send_str(COMMANDS["SERVER_PICK_UP"])
        elif CLIENT["CLIENT_OK"](data):
            new_pos = get_position(data)

            self.direction = get_direction(self.position, new_pos, self.direction)
            print("new pos2", new_pos)

            logging.debug(data)
            self.position = new_pos
            return self.make_move()
        elif self.finding_message and CLIENT["CLIENT_MESSAGE"](data):
            if data != "\a\b":
                print("found message", data)
                logging.info("message: " + data)
                self.send_str(COMMANDS["SERVER_LOGOUT"])
                return self.close()
            else:
                cmd, direction = pick_finding_position(self.position, self.direction)
                self.direction = direction
                return self.send_str(cmd)

        return self.syntax_error()

    def make_move(self):
        cmd, new_direction = get_next_move(self.position, self.direction)
        logging.debug("pos " + str(self.position.x) + " " + str(self.position.y) + " dir: " + str(
            self.direction) + " new dir: " + str(new_direction) + " cmd " + cmd)
        self.direction = new_direction
        if cmd == COMMANDS["SERVER_PICK_UP"]:
            self.finding_message = True

        return self.send_str(cmd)

    async def timeout_recharging(self):
        await asyncio.sleep(TIMEOUT_RECHARGING)
        state, timestamp = self.recharging
        if state and time.time() - timestamp > TIMEOUT_RECHARGING:
            return self.close()

    async def timeout(self):
        await asyncio.sleep(TIMEOUT)
        state, _ = self.recharging

        if not state and time.time() - self.last_message > TIMEOUT and not self.transport.is_closing():
            print("timeout closing")
            return self.close()

    def add_to_buffer(self, text):
        self.buffer = text

    def should_add_to_buffer(self, text):
        if VALIDATIONS["CLIENT_RECHARGING"](text):
            return self.add_to_buffer(text)
        if VALIDATIONS["CLIENT_FULL_POWER"](text):
            return self.add_to_buffer(text)

        if not self.authenticated:
            if self.auth_command == "CLIENT_USERNAME":
                if VALIDATIONS_AUTH["CLIENT_USERNAME"](text):
                    return self.add_to_buffer(text)
                else:
                    return self.syntax_error()

            elif self.auth_command == "CLIENT_CONFIRMATION":
                if VALIDATIONS_AUTH["CLIENT_CONFIRMATION"](text):
                    return self.add_to_buffer(text)
                else:
                    return self.syntax_error()

        if self.finding_message:
            if VALIDATIONS["CLIENT_MESSAGE"](text):
                return self.add_to_buffer(text)

        if VALIDATIONS["CLIENT_OK"](text):
            return self.add_to_buffer(text)

        return self.syntax_error()

    def process_authentication(self, data):
        print(self.auth_command)
        if not CLIENT_AUTH[self.auth_command](data):
            return self.syntax_error()

        if self.auth_command == "CLIENT_USERNAME":
            logging.info("username: " + data)
            code = self.auth_username(data)
            self.auth_command = "CLIENT_CONFIRMATION"
            return self.send_str(COMMANDS["SERVER_CONFIRMATION"] + str(code) + "\a\b")
        elif self.auth_command == "CLIENT_CONFIRMATION":
            if self.auth_confirm(data):
                self.send_str(COMMANDS["SERVER_OK"])
                self.authenticated = True
                return self.make_move()
            else:
                return self.error_login()

        self.syntax_error()

    def error_login(self):
        self.send_str(COMMANDS["SERVER_LOGIN_FAILED"])
        self.transport.close()

    def logic_error(self):
        self.send_str(COMMANDS["SERVER_LOGIC_ERROR"])
        self.transport.close()

    def syntax_error(self):
        self.send_str(COMMANDS["SERVER_SYNTAX_ERROR"])
        self.transport.close()

    def send_str(self, text):
        if self.will_recharge:
            self.send_buffer.append(text)
        else:
            to_send = ("".join(self.send_buffer) + text).encode("ascii")
            self.send_buffer.clear()
            logging.info("sending " + to_send.decode("ascii"))
            print("sending", to_send)
            self.transport.write(to_send)

    def flush_send_buffer(self):
        if self.send_buffer:
            self.send_str("")

    def auth_username(self, data):
        msg = get_msg(data)
        self.user_name = msg
        return calc_hash(msg, SERVER_KEY)

    def auth_confirm(self, data):
        msg = get_msg(data)
        code = calc_hash(self.user_name, CLIENT_KEY)
        return code == int(msg)

    def close(self):
        print("closing connection")
        self.transport.close()


async def main():
    # Get a reference to the event loop as we plan to use
    # low-level APIs.
    loop = asyncio.get_running_loop()

    server = await loop.create_server(
        lambda: EchoServerProtocol(),
        IP_ADDR, PORT)

    async with server:
        print(f"Started server on ip={IP_ADDR} port={PORT}")
        await server.serve_forever()


if __name__ == "__main__":
    loop = asyncio.get_event_loop()

    main_task = asyncio.ensure_future(main())
    loop.add_signal_handler(SIGINT, main_task.cancel)
    loop.add_signal_handler(SIGTERM, main_task.cancel)
    try:
        loop.run_until_complete(main_task)
    except asyncio.CancelledError:
        print("Stopped")
    finally:
        loop.close()
