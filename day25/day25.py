from sys import stdin

SUBJECT_NUM = 7
P = 20201227

card_public_key = int(stdin.readline().strip())
door_public_key = int(stdin.readline().strip())

def loop_size(public_key):
    value = 1
    for i in range(P):
        value = value * SUBJECT_NUM % P
        if value == public_key:
            return i+1
    assert False

def transform(subject, loop_size):
    value = 1
    for _ in range(loop_size):
        value = value * subject % P
    return value

encryption_key = transform(door_public_key, loop_size(card_public_key))
assert encryption_key == transform(card_public_key, loop_size(door_public_key))
print(encryption_key)
