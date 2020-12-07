from sys import stdin
import re
from collections import defaultdict

def parse_line(line):
    line = line.replace('.', '')
    line = re.sub(r'(no other)? bags?', '', line)
    head, body = line.split(" contain ")
    parsed_body = []
    for multi_item in body.split(', '):
        if multi_item != '':
            cnt, *item = multi_item.split(' ')
            parsed_body.append((int(cnt), ' '.join(item)))
    return [head, parsed_body]

def make_is_contained_in_dict(rules):
    is_contained_in_dict = defaultdict(set)
    for head, body in rules.items():
        for cnt, item in body:
            is_contained_in_dict[item].add(head)
    return is_contained_in_dict

def all_transitive_containers(x, rules):
    is_contained_in_dict = make_is_contained_in_dict(rules)
    result = set()
    frontier = is_contained_in_dict[x]
    while len(frontier) > 0:
        a = frontier.pop()
        result.add(a)
        for b in is_contained_in_dict[a]:
            if b not in result:
                frontier.add(b)
    return result

def total_transitive_contents(x, rules):
    return sum([cnt + cnt * total_transitive_contents(item, rules) for cnt, item in rules[x]])

rules = dict([parse_line(line.strip()) for line in stdin])
print(len(all_transitive_containers('shiny gold', rules)))
print(total_transitive_contents('shiny gold', rules))
