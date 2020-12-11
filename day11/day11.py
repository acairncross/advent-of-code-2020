from sys import stdin

def adjacent_seats(grid, r, c):
    for i in (-1, 0, 1):
        for j in (-1, 0, 1):
            if (i != 0 or j != 0) and 0 <= r+i < len(grid) and 0 <= c+j < len(grid[0]):
                spot = grid[r+i][c+j]
                if grid[r+i][c+j] != '.':
                    yield grid[r+i][c+j]

def apply_rules_a(grid, r, c):
    "Apply the rules of Part One to a seat/floor"
    if grid[r][c] == 'L':
        return '#' if all([seat == 'L' for seat in adjacent_seats(grid, r, c)]) else 'L'
    elif grid[r][c] == '#':
        return 'L' if sum([seat == '#' for seat in adjacent_seats(grid, r, c)]) >= 4 else '#'
    else:
        return '.'

def visible_seats(grid, r, c):
    for di in (-1, 0, 1):
        for dj in (-1, 0, 1):
            if (di != 0 or dj != 0):
                i, j = (di, dj)
                while 0 <= r+i < len(grid) and 0 <= c+j < len(grid[0]) and grid[r+i][c+j] == '.':
                    i += di
                    j += dj
                if 0 <= r+i < len(grid) and 0 <= c+j < len(grid[0]):
                    yield grid[r+i][c+j]

def apply_rules_b(grid, r, c):
    "Apply the rules of Part Two to a seat/floor"
    if grid[r][c] == 'L':
        return '#' if all([seat == 'L' for seat in visible_seats(grid, r, c)]) else 'L'
    elif grid[r][c] == '#':
        return 'L' if sum([seat == '#' for seat in visible_seats(grid, r, c)]) >= 5 else '#'
    else:
        return '.'

def apply_round(grid, rule_app):
    return [[rule_app(grid, r, c) for c, _ in enumerate(row)] for r, row in enumerate(grid)]

def same_grid(a, b):
    for i in range(len(a)):
        for j in range(len(a[0])):
            if a[i][j] != b[i][j]:
                return False
    return True

def apply_rounds(grid, rule_app):
    next_grid = apply_round(grid, rule_app)
    while not same_grid(grid, next_grid):
        grid = next_grid
        next_grid = apply_round(grid, rule_app)
    return grid

def count_occupied_seats(grid):
    return sum([spot == '#' for row in grid for spot in row])

def print_grid(grid):
    print('\n'.join([''.join(row) for row in grid]))

grid = [list(line.strip()) for line in stdin]
print(count_occupied_seats(apply_rounds(grid, apply_rules_a)))
print(count_occupied_seats(apply_rounds(grid, apply_rules_b)))
