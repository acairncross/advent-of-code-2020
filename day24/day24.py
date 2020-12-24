from sys import stdin

dir_to_vec = {
    'e': (1, 0),
    'w': (-1, 0),
    'sw': (0, -1),
    'ne': (0, 1),
    'se': (1, -1),
    'nw': (-1, 1),
}

def add(u, v):
    return (u[0] + v[0], u[1] + v[1])

def adjacent_tiles(pos):
    return [add(pos, v) for v in dir_to_vec.values()]

def interp_path(path):
    i = 0
    pos = (0, 0)
    while i < len(path):
        if path[i] in ('s', 'n'):
            pos = add(pos, dir_to_vec[path[i:i+2]])
            i += 2
        elif path[i] in ('e', 'w'):
            pos = add(pos, dir_to_vec[path[i]])
            i += 1
        else:
            assert False
    return pos

def setup_tiles(paths):
    tiles = set()
    for path in paths:
        pos = interp_path(path)
        if pos in tiles:
            tiles.remove(pos)
        else:
            tiles.add(pos)
    return tiles

def apply_rules(tiles):
    next_tiles = set()
    frontier = tiles | {adj_tile for tile in tiles for adj_tile in adjacent_tiles(tile)}
    for tile in frontier:
        num_adj_black = sum(adj_tile in tiles for adj_tile in adjacent_tiles(tile))
        if tile in tiles: # black
            if not (num_adj_black == 0 or num_adj_black > 2):
                next_tiles.add(tile)
        else: # white
            if num_adj_black == 2:
                next_tiles.add(tile)
    return next_tiles

if __name__ == '__main__':
    paths = [line.strip() for line in stdin.readlines()]
    initial_tiles = setup_tiles(paths)
    print(len(initial_tiles))

    tiles = initial_tiles
    for _ in range(100):
        tiles = apply_rules(tiles)
    print(len(tiles))
