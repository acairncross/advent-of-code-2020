from sys import stdin

def parse_token(tok):
    if tok[0] == '"':
        return tok[1]
    else:
        return int(tok)

def exact_match_rule(s, i, rule, rules):
    for match in match_rule(s, i, rule, rules):
        if match == len(s):
            return True
    return False

def match_rule(s, i, rule, rules):
    for disjunct in rule:
        yield from match_disjunct(s, i, disjunct, rules)

def match_disjunct(s, i, disjunct, rules):
    yield from match_atoms(s, i, disjunct, 0, rules)

def match_atoms(s, i, atoms, atom_i, rules):
    if atom_i < len(atoms):
        for j in match_atom(s, i, atoms[atom_i], rules):
            yield from match_atoms(s, j, atoms, atom_i+1, rules)
    else:
        yield i

def match_atom(s, i, atom, rules):
    if i < len(s):
        if isinstance(atom, int):
            yield from match_rule(s, i, rules[atom], rules)
        elif isinstance(atom, str):
            if s[i] == atom:
                yield i+1
        else:
            assert False

rules = {}
for line in stdin:
    if len(line.strip()) == 0:
        break
    head, body = line.strip().split(':')
    disjuncts = body.split('|')
    rules[int(head)] = [
        [parse_token(tok) for tok in disjunct.split()]
        for disjunct in disjuncts
    ]

messages = []
for line in stdin:
    messages.append(line.strip())

print(sum(exact_match_rule(msg, 0, rules[0], rules) for msg in messages))

rules[8] = [[42], [42, 8]]
rules[11] = [[42, 31], [42, 11, 31]]
print(sum(exact_match_rule(msg, 0, rules[0], rules) for msg in messages))
