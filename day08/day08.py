from sys import stdin

def parse_line(line):
    op, arg = line.split()
    return op, int(arg)

def run_program(instrs):
    pc = 0
    acc = 0
    visited_pcs = set()

    while pc not in visited_pcs and pc != len(instrs):
        op, arg = instrs[pc]
        visited_pcs.add(pc)

        if op == 'acc':
            acc += arg

        if op == 'jmp':
            pc += arg
        else:
            pc += 1

    err = pc != len(instrs)

    return pc, acc, err

def corrupt_one(instrs):
    for i, (op, arg) in enumerate(instrs):
        if op == 'nop':
            instrs_copy = instrs[:]
            instrs_copy[i] = ('jmp', arg)
            yield instrs_copy
        elif op == 'jmp':
            instrs_copy = instrs[:]
            instrs_copy[i] = ('nop', arg)
            yield instrs_copy

def uncorrupt_program(instrs):
    for vafgef in corrupt_one(instrs):
        _, _, err = run_program(vafgef)
        if not err:
            return vafgef
    assert False

instrs = [parse_line(line) for line in stdin]

print(run_program(instrs))
print(run_program(uncorrupt_program(instrs)))
