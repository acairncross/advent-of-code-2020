from sys import stdin

class Node:
    "A doubly linked list Node."

    def __init__(self, datum):
        self.datum = datum
        self.prev = None
        self.next = None

    def __repr__(self):
        return "Node({:d}, {:s}, {:s})".format(
            self.datum,
            "None" if self.prev is None else "Node(...)",
            "None" if self.next is None else "Node(...)"
        )

def shift_node_values(nodes, shift):
    for node in nodes:
        node.datum += shift

def play_game(cups, num_moves):
    nodes = [Node(cup) for cup in cups]
    for i, node in enumerate(nodes):
        node.prev = nodes[(i-1) % len(nodes)]
        node.next = nodes[(i+1) % len(nodes)]
    nodes.sort(key=lambda node: node.datum) # now nodes[i].datum == i+1
    shift_node_values(nodes, -1) # make nodes[i].datum == i
    node = nodes[cups[0]-1] # -1 because of the shift
    for _ in range(num_moves):
        node = play_move(node, nodes)
    shift_node_values(nodes, 1) # undo the shift
    return nodes[0]

def play_move(cur_node, nodes):
    sub_head, sub_tail = excise_nodes(cur_node, cur_node.next.next.next.next)

    j = (cur_node.datum - 1) % len(nodes)
    while nodes[j] is sub_head or nodes[j] is sub_head.next or nodes[j] is sub_tail:
        j = (j - 1) % len(nodes)

    dest_node = nodes[j]
    insert_nodes(dest_node, dest_node.next, sub_head, sub_tail)
    return cur_node.next

def excise_nodes(head, tail):
    "Remove all nodes between head and tail."
    sub_head = head.next
    sub_head.prev = None
    sub_tail = tail.prev
    sub_tail.next = None

    head.next = tail
    tail.prev = head

    return (sub_head, sub_tail)

def insert_nodes(head, tail, sub_head, sub_tail):
    head.next = sub_head
    sub_head.prev = head
    tail.prev = sub_tail
    sub_tail.next = tail

cups = [int(c) for c in stdin.readline().strip()]
node = play_game(cups, 100)
result = []
for _ in range(len(cups)):
    result.append(str(node.datum))
    node = node.next
print(''.join(result))

cups = cups + list(range(max(cups) + 1, 1000001))
node = play_game(cups, 10000000)
print(node.next.datum * node.next.next.datum)
