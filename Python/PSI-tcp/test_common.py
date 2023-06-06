from common import *


def test_direction():
    assert get_direction((3, 3), (3, 4), None) == 0
    assert get_direction((-2, 3), (-3, 3), None) == 3
    assert get_direction((2, 3), (1, 3), None) == 3
    assert get_direction((0, 3), (1, 3), None) == 1
    assert get_direction((0, 3), (0, 2), None) == 2
    assert get_direction((0, -3), (0, -2), None) == 0
    assert get_direction((3, 4), (3, 4), None) is None


def test_position():
    assert get_position("OK 0 1\a\b") == (0, 1)
    assert get_position("OK -10 15\a\b") == (-10, 15)
    assert get_position("OK -10 -15\a\b") == (-10, -15)


def test_next_move():
    assert get_next_move((10, 0), None) == (COMMANDS["SERVER_MOVE"], None)
    assert get_next_move((-2, -1), 2) != (COMMANDS["SERVER_PICK_UP"], 2)
    assert get_next_move((-2, -2), 2) == (COMMANDS["SERVER_PICK_UP"], 2)

    assert get_next_move((20, -2), 1) == (COMMANDS["SERVER_TURN_LEFT"], 0) \
           or get_next_move((20, -2), 1) == (COMMANDS["SERVER_TURN_RIGHT"], 2)
    assert get_next_move((-20, -2), 3) == (COMMANDS["SERVER_TURN_LEFT"], 2) \
           or get_next_move((-20, -2), 3) == (COMMANDS["SERVER_TURN_RIGHT"], 0)
    assert get_next_move((20, -2), 2) == (COMMANDS["SERVER_TURN_RIGHT"], 3)
    assert get_next_move((20, -2), 0) == (COMMANDS["SERVER_TURN_LEFT"], 3)
    assert get_next_move((-1, -2), 3) == (COMMANDS["SERVER_MOVE"], 3)
    assert get_next_move((-20, -2), 1) == (COMMANDS["SERVER_MOVE"], 1)

    assert get_next_move((-2, -10), 2) == (COMMANDS["SERVER_TURN_LEFT"], 1) \
           or get_next_move((-2, -10), 2) == (COMMANDS["SERVER_TURN_RIGHT"], 3)
    assert get_next_move((-2, 12), 0) == (COMMANDS["SERVER_TURN_LEFT"], 3) \
           or get_next_move((-2, 12), 0) == (COMMANDS["SERVER_TURN_RIGHT"], 1)
    assert get_next_move((-2, -1), 1) == (COMMANDS["SERVER_TURN_RIGHT"], 2)
    assert get_next_move((-2, 32), 3) == (COMMANDS["SERVER_TURN_LEFT"], 2)
    assert get_next_move((-2, -11), 0) == (COMMANDS["SERVER_MOVE"], 0)
    assert get_next_move((-2, 20), 2) == (COMMANDS["SERVER_MOVE"], 2)

    assert get_next_move((-1, 11), 0) == (COMMANDS["SERVER_TURN_LEFT"], 3)


def test_calc_hash():
    assert calc_hash("Mnau!", 54621) == 29869
    assert calc_hash("Mnau!", 54622) != 29869


def test_pick_finding_position():
    assert pick_finding_position((-2, -2), 1) == (COMMANDS["SERVER_MOVE"], 1)
    assert pick_finding_position((1, -2), 1) == (COMMANDS["SERVER_MOVE"], 1)
    assert pick_finding_position((2, -2), 1) == (COMMANDS["SERVER_TURN_LEFT"], 0)

    assert pick_finding_position((2, -1), 3) == (COMMANDS["SERVER_MOVE"], 3)
    assert pick_finding_position((0, -1), 3) == (COMMANDS["SERVER_MOVE"], 3)
    assert pick_finding_position((-1, -1), 3) == (COMMANDS["SERVER_MOVE"], 3)
    assert pick_finding_position((-2, -1), 3) == (COMMANDS["SERVER_TURN_LEFT"], 2)
    assert pick_finding_position((-2, -1), 1) == (COMMANDS["SERVER_TURN_LEFT"], 0)

    assert pick_finding_position((-2, -1), 0) == (COMMANDS["SERVER_MOVE"], 0)
    assert pick_finding_position((-2, 0), 0) == (COMMANDS["SERVER_TURN_LEFT"], 3)


def test_to_left():
    assert to_left(3) == 2
    assert to_left(0) == 3
    assert to_left(1) == 0
    assert to_left(2) == 1
