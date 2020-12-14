from sys import stdin
from collections import defaultdict
import re
from typing import Iterable

def to_bits(x: int) -> str: return bin(x)[2:]

def zero_extend(bs: str) -> str:
    return (36 - len(bs)) * '0' + bs

def apply_mask_a(mask: str, x: int) -> int:
    result = []
    for m, b in zip(mask, zero_extend(to_bits(x))):
        if m == 'X': result.append(b)
        elif m == '1': result.append('1')
        elif m == '0': result.append('0')
    return int(''.join(result), 2)

def run_program_a(prog):
    mask = ''
    mem = defaultdict(int)
    for lhs, rhs in prog:
        if lhs == 'mask':
            mask = rhs
        elif m := re.match(r'mem\[(\d+)\]', lhs):
            addr = int(m.groups()[0])
            mem[addr] = apply_mask_a(mask, int(rhs))
    return mem

def floatify(floating_indices, i, bs) -> Iterable[list[str]]:
    if i == len(floating_indices):
        yield bs[:]
    else:
        for result in floatify(floating_indices, i+1, bs):
            result[floating_indices[i]] = '0'
            yield result
            result[floating_indices[i]] = '1'
            yield result

def apply_mask_b(mask, x):
    result = []
    floating_indices = []
    for i, (m, b) in enumerate(zip(mask, zero_extend(to_bits(x)))):
        if m == '0': result.append(b)
        elif m == '1': result.append('1')
        elif m == 'X':
            result.append('X')
            floating_indices.append(i)
    return [int(''.join(bs)) for bs in floatify(floating_indices, 0, result)]

def run_program_b(prog):
    mask = ''
    mem = defaultdict(int)
    for lhs, rhs in prog:
        if lhs == 'mask':
            mask = rhs
        elif m := re.match(r'mem\[(\d+)\]', lhs):
            for addr in apply_mask_b(mask, int(m.groups()[0])):
                mem[addr] = int(rhs)
    return mem

program = [line.strip().split(' = ') for line in stdin]
print(sum(run_program_a(program).values()))
print(sum(run_program_b(program).values()))
