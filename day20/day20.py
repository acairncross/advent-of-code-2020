from collections import namedtuple
from functools import reduce
from math import sqrt
from operator import mul
from sys import stdin
from typing import List
import re

OrientedTile = namedtuple('OrientedTile', ['id', 'flip', 'rot'])

SEA_MONSTER = [
    list('                  # '),
    list('#    ##    ##    ###'),
    list(' #  #  #  #  #  #   '),
]

def flatten(xss):
    return [x for xs in xss for x in xs]

def apply_n(f, n, x):
    for _ in range(n):
        x = f(x)
    return x

def rotate2d(grid):
    "Rotate a 2D array 90 degrees clockwise."
    dim = len(grid)
    rotated_grid = [[None for _ in range(dim)] for _ in range(dim)]
    for i in range(dim):
        for j in range(dim):
            rotated_grid[j][dim-i-1] = grid[i][j]
    return rotated_grid

assert rotate2d([
    ['#', '#', '.'],
    ['.', '#', '#'],
    ['.', '.', '.']
]) == [
    ['.', '.', '#'],
    ['.', '#', '#'],
    ['.', '#', '.']
]

def flip2d(grid):
    "Flip a 2D array about its y-axis."
    dim = len(grid)
    flipped_grid = [[None for _ in range(dim)] for _ in range(dim)]
    for i in range(dim):
        for j in range(dim):
            flipped_grid[i][dim-j-1] = grid[i][j]
    return flipped_grid

assert flip2d([
    ['#', '#', '.'],
    ['.', '#', '#'],
    ['.', '.', '.']
]) == [
    ['.', '#', '#'],
    ['#', '#', '.'],
    ['.', '.', '.']
]

def match_border(xs, ys):
    "Does the border xs match the border ys?"
    return all(x == y for x, y in zip(xs, ys))

def all_borders(tile):
    "All the possible borders of a tile (may contain duplicates)."
    left = [row[0] for row in tile]
    right = [row[-1] for row in tile]
    borders = [tile[0], left, tile[-1], right]
    borders += [list(reversed(border)) for border in borders]
    return borders

def find_topleft_candidates(tile_db) -> List[OrientedTile]:
    "Returns the tiles which have two unique connected borders."
    candidates = []
    for candidate_tile_id in tile_db:
        other_tile_ids = [tile_id for tile_id in tile_db if tile_id != candidate_tile_id]
        flip = 0 # Only need to check flip = 0 WLOG
        for rot in range(4):
            top_border = tile_db[candidate_tile_id][flip][rot][0]
            left_border = [row[0] for row in tile_db[candidate_tile_id][flip][rot]]
            unique_top_left_borders = True
            # Go through all other tiles to see if any other tile has these borders
            for other_tile_id in other_tile_ids:
                other_tile = tile_db[other_tile_id][0][0] # Any flip/rot will do
                borders = all_borders(other_tile)
                if any(match_border(border, top_border) for border in borders) or \
                    any(match_border(border, left_border) for border in borders):
                    unique_top_left_borders = False
                    break
            if unique_top_left_borders:
                candidates.append(OrientedTile(candidate_tile_id, 0, rot))
    return candidates

def assemble_tiles(tile_db):
    "Try to assemble the tiles, starting with each of the possible topleft tiles."
    for topleft in find_topleft_candidates(tile_db):
        if grid := assemble_tiles_with_topleft(topleft, tile_db):
            return grid
    return None

def assemble_tiles_with_topleft(topleft: OrientedTile, tile_db) -> List[List[OrientedTile]]:
    """Given a tile to place in the topleft, try to assemble the rest of the tiles.

    Go through the arrangement row-by-row, column-by-column, finding the unique piece that can be
    placed next. If there is no piece that can be placed, or the piece is not unique, give up.
    """
    dim = int(sqrt(len(tile_db)))
    grid = [[None for _ in range(dim)] for _ in range(dim)]
    grid[0][0] = topleft
    available_tile_ids = {tile_id for tile_id in tile_db if tile_id != topleft.id}
    for i in range(dim):
        for j in range(dim):
            if i == j == 0: # topleft, already placed
                continue
            constraints = {}
            if i > 0: # Not the top row
                id, flip, rot = grid[i-1][j]
                constraints['top'] = tile_db[id][flip][rot][-1]
            if j > 0: # Not the left column
                id, flip, rot = grid[i][j-1]
                constraints['left'] = [row[-1] for row in tile_db[id][flip][rot]]
            constrained_tiles = find_constrained_tiles(constraints, available_tile_ids, tile_db)
            if len(constrained_tiles) > 1: # Ambiguous, could be more than one solution!
                return None
            elif len(constrained_tiles) == 0: # No solutions!
                return None
            grid[i][j] = constrained_tiles[0]
            available_tile_ids.remove(constrained_tiles[0].id)
    return grid

def find_constrained_tiles(constraints, available_tile_ids, tile_db) -> List[OrientedTile]:
    "Returns all tiles satisfying the constraints on what the top and left borders should be."
    satisfying_tiles = []
    for tile_id in available_tile_ids:
        for flip in range(2):
            for rot in range(4):
                top_border = tile_db[tile_id][flip][rot][0]
                left_border = [row[0] for row in tile_db[tile_id][flip][rot]]
                sat = True
                if top_constraint := constraints.get('top'):
                    sat = sat and match_border(top_border, top_constraint)
                if left_constraint := constraints.get('left'):
                    sat = sat and match_border(left_border, left_constraint)
                if sat:
                    satisfying_tiles.append(OrientedTile(tile_id, flip, rot))
    return satisfying_tiles

# Part B from here
def count_sea_monsters(grid):
    for flip in range(2):
        for rot in range(4):
            oriented_grid = apply_n(flip2d, flip, apply_n(rotate2d, rot, grid))
            count = count_pictures(SEA_MONSTER, oriented_grid)
            if count > 0:
                return count
    return 0

def unorient_tile(oriented_tile: OrientedTile, tile_db):
    id, flip, rot = oriented_tile
    return tile_db[id][flip][rot]

def strip_border(tile):
    return [row[1:-1] for row in tile[1:-1]]

def join_tiles(grid):
    "Collapse the tiles in a grid of tiles"
    tile_dim = len(grid[0][0])
    joined_grid = []
    for grid_row in grid:
        for tile_row_i in range(tile_dim):
            joined_grid.append(''.join(flatten([tile[tile_row_i] for tile in grid_row])))
    return joined_grid

def count_pictures(pic, grid):
    count = 0
    for y in range(len(grid) - len(pic)):
        for x in range(len(grid[0]) - len(pic[0])):
            if find_picture_at_pos(pic, grid, y, x):
                count += 1
    return count

def find_picture_at_pos(pic, grid, y, x):
    for i in range(len(pic)):
        for j in range(len(pic[0])):
            if pic[i][j] == '#' and grid[y+i][x+j] != '#':
                return False
    return True

def count2d(y, xss):
    return sum(x == y for x in flatten(xss))

tile_db = {}
while True:
    try:
        line = next(stdin).strip()
        tile_id = int(re.match(r'Tile (\d+):', line).groups()[0])
        tile = []
        line = next(stdin).strip()
        while len(line) > 0:
            tile.append(list(line))
            line = next(stdin).strip()
        tile_orientations = {
            0: {rot: apply_n(rotate2d, rot, tile) for rot in range(4)},
            1: {rot: apply_n(rotate2d, rot, flip2d(tile)) for rot in range(4)},
        }
        tile_db[tile_id] = tile_orientations
    except StopIteration:
        break

print(reduce(mul, [candidate.id for candidate in find_topleft_candidates(tile_db)]))

assembled_tiles = [[unorient_tile(tile, tile_db) for tile in row] for row in assemble_tiles(tile_db)]
camera_image = join_tiles([[strip_border(tile) for tile in row] for row in assembled_tiles])
print(count2d('#', camera_image) - count2d('#', SEA_MONSTER) * count_sea_monsters(camera_image))
