from sys import stdin

def sum_score(player):
    total = 0
    for i, card in enumerate(reversed(player)):
        total += (i+1) * card
    return total

def play_game(player1, player2):
    player1, player2 = player1[:], player2[:]
    while len(player1) > 0 and len(player2) > 0:
        x, y = player1.pop(0), player2.pop(0)
        if x > y:
            player1 += [x, y]
        else:
            player2 += [y, x]
    return (0, player1) if len(player1) > 0 else (1, player2)

def play_recursive_combat(player1, player2):
    player1, player2 = player1[:], player2[:]
    visited = set()
    while len(player1) > 0 and len(player2) > 0:
        if (tuple(player1), tuple(player2)) in visited:
            return (0, player1)
        visited.add((tuple(player1), tuple(player2)))
        x, y = player1.pop(0), player2.pop(0)
        if x > len(player1) or y > len(player2):
            if x > y:
                player1 += [x, y]
            else:
                player2 += [y, x]
        else:
            winner_idx, _ = play_recursive_combat(player1[:x], player2[:y])
            if winner_idx == 0:
                player1 += [x, y]
            else:
                player2 += [y, x]
    return (0, player1) if len(player1) > 0 else (1, player2)

player1 = []
stdin.readline()
while line := stdin.readline().strip():
    player1.append(int(line))

player2 = []
stdin.readline()
while line := stdin.readline().strip():
    player2.append(int(line))

print(sum_score(play_game(player1, player2)[1]))
print(sum_score(play_recursive_combat(player1, player2)[1]))
