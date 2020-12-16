from sys import stdin
import re

def apply_rule(rule, value):
    (lo1, hi1), (lo2, hi2) = rule
    return lo1 <= value <= hi1 or lo2 <= value <= hi2

def ticket_scanning_error_rate(tickets, rules):
    total_error = 0
    invalid_ticket_ids = []
    for i, ticket in enumerate(tickets):
        for value in ticket:
            ok = False
            for rule in rules:
                if apply_rule(rule, value):
                    ok = True
                    break
            if not ok:
                invalid_ticket_ids.append(i)
                total_error += value
    return total_error, invalid_ticket_ids

def find_field_mapping(tickets, rules):
    mapping = {}
    while len(mapping) < len(rules):
        for src_field_num, col in enumerate(zip(*tickets)):
            if src_field_num in mapping:
                continue
            possible_mappings = []
            for dst_field_num, rule in enumerate(rules):
                if dst_field_num in mapping.values():
                    continue
                ok = True
                for value in col:
                    if not apply_rule(rule, value):
                        ok = False
                        break
                if ok:
                    possible_mappings.append(dst_field_num)
            if len(possible_mappings) == 1:
                mapping[src_field_num] = possible_mappings[0]
    # I don't know why but changing the code^ to produce the mapping in the
    # right direction wasn't working, so just invert it at the end
    return {k:v for v,k in mapping.items()}

rules = []
for line in stdin:
    if len(line.strip()) == 0:
        break
    lo1, hi1, lo2, hi2 = re.match(
        r'[a-zA-Z ]+: (\d+)-(\d+) or (\d+)-(\d+)', line.strip()).groups()
    rules.append(((int(lo1), int(hi1)), (int(lo2), int(hi2))))

stdin.readline() # 'your ticket:'
my_ticket = [int(s) for s in stdin.readline().strip().split(',')]
stdin.readline() # newline
stdin.readline() # 'nearby tickets:'

nearby_tickets = []
for line in stdin:
    if len(line.strip()) == 0:
        break
    nearby_tickets.append([int(s) for s in line.strip().split(',')])

error_rate, invalid_ticket_ids = ticket_scanning_error_rate(nearby_tickets, rules)
print(error_rate)

valid_tickets = [ticket for i, ticket in enumerate(nearby_tickets) if i not in invalid_ticket_ids]
mapping = find_field_mapping(valid_tickets, rules)
prod = 1
for i in range(0, 6):
    prod *= my_ticket[mapping[i]]
print(prod)
