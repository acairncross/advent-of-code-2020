from sys import stdin
import itertools

def play_game(starting_numbers):
    history = {}
    i = 0
    while True:
        if i < len(starting_numbers):
            number = starting_numbers[i]
        else:
            number = next_number
        if number in history:
            next_number = i - history[number]
        else:
            next_number = 0
        history[number] = i
        yield number
        i += 1

starting_numbers = [int(number) for number in stdin.readline().strip().split(',')]

print(list(itertools.islice(play_game(starting_numbers), 2020))[-1])
print(list(itertools.islice(play_game(starting_numbers), 30000000))[-1])
