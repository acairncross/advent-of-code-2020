from sys import stdin
import itertools

def neighbors(coord):
    result = []
    for delta in itertools.product(*[(-1, 0, 1) for _ in range(len(coord))]):
        if all(d == 0 for d in delta):
            continue
        result.append(tuple(c + d for c, d in zip(coord, delta)))
    return set(result)

def count_active(state, xs):
    return sum(x in state for x in xs)

def run_cycle(state):
    next_state = set()

    candidates = {coord for coord in state}
    for coord in state:
        candidates |= neighbors(coord)

    for coord in candidates:
        if coord in state: # active
            if count_active(state, neighbors(coord)) in (2, 3):
                next_state.add(coord)
        else: # inactive
            if count_active(state, neighbors(coord)) == 3:
                next_state.add(coord)

    return next_state

def run_cycle_n(state, n):
    for _ in range(n):
        state = run_cycle(state)
    return state

def make_initial_state(initial_state_grid, D):
    initial_state = set()
    for i, row in enumerate(initial_state_grid):
        for j, cell in enumerate(row):
            if cell == '#':
                initial_state.add(tuple(0 for _ in range(D-2)) + (i, j))
    return initial_state

initial_state_grid = [list(line.strip()) for line in stdin]
print(len(run_cycle_n(make_initial_state(initial_state_grid, 3), 6)))
print(len(run_cycle_n(make_initial_state(initial_state_grid, 4), 6)))
