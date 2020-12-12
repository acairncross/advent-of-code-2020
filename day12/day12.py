from sys import stdin

dirs = {
    'N': (0, 1),
    'S': (0, -1),
    'E': (1, 0),
    'W': (-1, 0),
}

rots = {
    'R': ((0, 1), (-1, 0)),
    'L': ((0, -1), (1, 0)),
}

def parse_instr(instr_str):
    return instr_str[0], int(instr_str[1:])

def run_instrs(instrs, pos, dir, use_waypoint):
    for action, value in instrs:
        if action in dirs.keys():
            if use_waypoint:
                # Move the waypoint
                dir = move(dir, dirs[action], value)
            else:
                # Move the ship
                pos = move(pos, dirs[action], value)
        elif action in rots.keys():
            assert value % 90 == 0
            turns = value // 90
            for _ in range(turns):
                dir = mult(rots[action], dir)
        elif action  == 'F':
            pos = move(pos, dir, value)
    return pos

def move(pos, dir, mag):
    return add(pos, mult(mag, dir))

def add(u, v):
    return tuple(u_i + v_i for u_i, v_i in zip(u, v))

def sub(u, v):
    return tuple(u_i - v_i for u_i, v_i in zip(u, v))
    
def mult(a, v):
    if isinstance(a, int):
        return tuple(a*v_i for v_i in v)
    elif isinstance(a, tuple) and isinstance(a[0], tuple):
        return tuple(dot(row, v) for row in a)

def dot(u, v):
    return sum(u_i * v_i for u_i, v_i in zip(u, v))

def l1_norm(u):
    return sum(abs(u_i) for u_i in u)

instrs = [parse_instr(line.strip()) for line in stdin]
print(l1_norm(run_instrs(instrs, (0, 0), dirs['E'], False)))
print(l1_norm(run_instrs(instrs, (0, 0), (10, 1), True)))
