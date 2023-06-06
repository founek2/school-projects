# Homework 01 - Game of life
# 
# Your task is to implement part of the cell automaton called
# Game of life. The automaton is a 2D simulation where each cell
# on the grid is either dead or alive.
# 
# State of each cell is updated in every iteration based state of neighbouring cells.
# Cell neighbours are cells that are horizontally, vertically, or diagonally adjacent.
#
# Rules for update are as follows:
# 
# 1. Any live cell with fewer than two live neighbours dies, as if by underpopulation.
# 2. Any live cell with two or three live neighbours lives on to the next generation.
# 3. Any live cell with more than three live neighbours dies, as if by overpopulation.
# 4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
#
# 
# Our implementation will use coordinate system will use grid coordinates starting from (0, 0) - upper left corner.
# The first coordinate is row and second is column.
# 
# Do not use wrap around (toroid) when reaching edge of the board.
# 
# For more details about Game of Life, see Wikipedia - https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life


def get_num_repre(alive):
    """
    return numeric life state representation
    alive -> 1, death -> 0
    :param alive: set() of all alive cells
    :return: Integer
    """
    return lambda cell: 1 if cell in alive else 0


def get_neighbor_count(cell, alive):
    """
    return count of alive neighbors
    :param cell: actual cell
    :param alive: set() of all alive cells
    :return: Integer
    """
    get_num = get_num_repre(alive)
    r, c = cell
    # 012
    # 3_4
    # 567
    return get_num((r - 1, c - 1)) + get_num((r - 1, c)) + get_num((r - 1, c + 1)) + get_num((r, c - 1)) + get_num(
        (r, c + 1)) + get_num((r + 1, c - 1)) + get_num((r + 1, c)) + get_num((r + 1, c + 1))


def update(alive, size, iter_n):
    """
    return set of coordinates of alive cells
    after iter_n iterations.
    """
    row, col = size

    act_alive = alive
    while iter_n > 0:
        future_alive = set()
        for r in range(row):
            for c in range(col):
                cell = (r, c)
                neib_count = get_neighbor_count(cell, act_alive)
                if cell in alive:
                    if 2 <= neib_count <= 3:
                        future_alive.add(cell)
                else:
                    if neib_count == 3:
                        future_alive.add(cell)
        act_alive = future_alive
        iter_n -= 1

    return act_alive


def draw(alive, size):
    """
    alive - set of cell coordinates marked as alive, can be empty
    size - size of simulation grid as  tuple - ( 

    output - string showing the board state with alive cells marked with X
    """
    # Example of 3x3 board with 1 alive cell at coordinates (0, 2):
    # +---+ 
    # |  X|
    # |   |
    # |   |
    # +---+

    start = "+"
    row, col = size
    for c in range(col):
        start += "-"
    start += "+"

    body = start + "\n"
    for r in range(row):
        body += "|"
        for c in range(col):
            body += "X" if (r, c) in alive else " "
        body += "|\n"
    body += start

    return body
