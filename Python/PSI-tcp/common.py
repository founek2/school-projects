import re
import logging
from collections import namedtuple

IP_ADDR = '127.0.0.1'
PORT = 8181

# in seconds
TIMEOUT = 1
TIMEOUT_RECHARGING = 5

SERVER_KEY = 54621
CLIENT_KEY = 45328

Position = namedtuple('Position', 'x y')
Recharge = namedtuple('Recharge', 'state timestamp')

COMMANDS = {
    'SERVER_CONFIRMATION': "",
    'SERVER_MOVE': '102 MOVE\a\b',
    'SERVER_TURN_LEFT': '103 TURN LEFT\a\b',
    'SERVER_TURN_RIGHT': '104 TURN RIGHT\a\b',
    'SERVER_PICK_UP': '105 GET MESSAGE\a\b',
    'SERVER_LOGOUT': '106 LOGOUT\a\b',
    'SERVER_OK': '200 OK\a\b',
    'SERVER_LOGIN_FAILED': '300 LOGIN FAILED\a\b',
    'SERVER_SYNTAX_ERROR': '301 SYNTAX ERROR\a\b',
    'SERVER_LOGIC_ERROR': '302 LOGIC ERROR\a\b',
}

CLIENT_AUTH = {
    'CLIENT_USERNAME': lambda text: len(text) <= 12 and re.match('^((?!\a\b).)*\a\b$', text),
    'CLIENT_CONFIRMATION': lambda text: len(text) <= 7 and re.match('^[0-9]*\a\b$', text) and int(
        get_msg(text)) < 65535,
}
CLIENT = {
    'CLIENT_OK': lambda text: len(text) <= 12 and re.match('^OK -?(0|[1-9][0-9]*) -?(0|[1-9][0-9]*)\a\b$', text),
    'CLIENT_RECHARGING': lambda text: text == "RECHARGING\a\b",
    'CLIENT_FULL_POWER': lambda text: text == 'FULL POWER\a\b',
    'CLIENT_MESSAGE': lambda text: len(text) <= 100 and re.match('^((?!\a\b).)*\a\b$', text)
}


def client_username_v(text: str):
    return len(text) <= 10 or (len(text) == 11 and text.endswith("\a"))


def client_conf_v(text: str):
    return re.match("^[0-9]+\a?$", text) and (
            len(text) <= 5 or (len(text) == 6 and text.endswith("\a"))
    ) and int(re.search("^[0-9]+", text).group()) < 65535


def client_ok_v(text: str):
    return text.startswith("O") and (
            len(text) <= 10 or (len(text) == 11 and text.endswith("\a"))
    )


def client_recharging(text: str):
    return text.startswith("R") and (len(text) <= len("RECHARGING") or text == "RECHARGING\a")


def client_full_power(text: str):
    return text.startswith("F") and (len(text) <= len("FULL POWER") or text == "FULL POWER\a")


def client_message(text: str):
    return len(text) <= 98 or (len(text) == 99 and text.endswith("\a"))


VALIDATIONS_AUTH = {
    "CLIENT_USERNAME": client_username_v,
    "CLIENT_CONFIRMATION": client_conf_v,
}

VALIDATIONS = {
    "CLIENT_OK": client_ok_v,
    "CLIENT_RECHARGING": client_recharging,
    "CLIENT_FULL_POWER": client_full_power,
    "CLIENT_MESSAGE": client_message
}


def get_msg(data: str):
    return data.split("\a\b")[0]


def calc_hash(text, key):
    values = [ord(c) for c in text]
    hash_name = (sum(values) * 1000) % 65536
    return (hash_name + key) % 65536


def get_position(data):
    msg = get_msg(data).split(" ")
    return Position(int(msg[1]), int(msg[2]))


"""
    directions
    x 0 x
    3 T 1
    x 2 x
"""


def get_next_move(position: Position, direction):
    """
    It have turn optimizations - instead of always turning to same side - it decide which turn to use for best outcome
    –> minimal movements
    """
    if direction is None:
        return COMMANDS["SERVER_MOVE"], None

    x, y = position

    if x > -2:
        if direction == 3:
            return COMMANDS["SERVER_MOVE"], direction
        elif direction == 0:  # nahoru
            return COMMANDS["SERVER_TURN_LEFT"], 3
        else:
            return COMMANDS["SERVER_TURN_RIGHT"], direction + 1
    elif x < -2:
        if direction == 1:
            return COMMANDS["SERVER_MOVE"], direction
        elif direction == 0:  # nahoru
            return COMMANDS["SERVER_TURN_RIGHT"], 1
        else:
            return COMMANDS["SERVER_TURN_LEFT"], direction - 1

    logging.debug("Y")
    if y < -2:
        if direction == 0:
            return COMMANDS["SERVER_MOVE"], direction
        elif direction == 3:  # vlevo
            return COMMANDS["SERVER_TURN_RIGHT"], 0
        else:
            return COMMANDS["SERVER_TURN_LEFT"], direction - 1
    elif y > -2:
        if direction == 2:
            return COMMANDS["SERVER_MOVE"], direction
        elif direction == 3:  # vlevo
            return COMMANDS["SERVER_TURN_LEFT"], 2
        else:
            return COMMANDS["SERVER_TURN_RIGHT"], direction + 1

    return COMMANDS["SERVER_PICK_UP"], direction


def get_direction(pos1: Position, pos2: Position, direction):
    x1, y1 = pos1
    x2, y2 = pos2

    if x1 is None:
        return direction

    if x1 == x2 and y1 != y2:
        # 1 > 2 -> 0
        # -2 > -3 -> 2
        return 2 if y1 > y2 else 0
    elif x1 != x2 and y1 == y2:
        # 2 > 3 -> 1
        # -2 > -3 -> 3
        return 3 if x1 > x2 else 1

    # x1 == x2 and y1 == y2
    return direction


def pick_finding_position(position, direction):
    """
    Y - start position (-2, -2)
        x x x x x
        x x x x x
        9 x x x x
        8 7 6 5 4
        Y 0 1 2 3
    """
    x, y = position

    if y == -2 or y == 0 or y == 2:
        if x < 2:
            if direction == 1:
                return COMMANDS["SERVER_MOVE"], direction
            else:
                return COMMANDS["SERVER_TURN_LEFT"], to_left(direction)

        if direction == 0:
            return COMMANDS["SERVER_MOVE"], direction
        else:
            return COMMANDS["SERVER_TURN_LEFT"], to_left(direction)

    if y == -1 or y == 1:
        if x > -2:
            if direction == 3:
                return COMMANDS["SERVER_MOVE"], direction
            else:
                return COMMANDS["SERVER_TURN_LEFT"], to_left(direction)

        if direction == 0:
            return COMMANDS["SERVER_MOVE"], direction
        else:
            return COMMANDS["SERVER_TURN_LEFT"], to_left(direction)

    raise Exception("already searched whole area")


def to_left(direction):
    return 3 if direction == 0 else direction - 1
