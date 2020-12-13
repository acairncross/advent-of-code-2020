from sys import stdin

def wait_time(t0, bus_id):
    "Number of minutes to wait for a bus with a particular id."
    # t0 % bus_id = Number of minutes we missed the previous bus by
    # bus_id - t0 % bus_id = Number of minutes to wait until the next bus
    # % bus_id is so that we wait 0 minutes (not bus_id minutes) if we're on time
    return (bus_id - t0 % bus_id) % bus_id

def earliest_bus_id(t0, bus_ids):
    "The earliest bus we can catch after t0."
    return min(bus_ids, key=lambda bus_id: wait_time(t0, bus_id))

def make_chinese_remainder_problem(all_bus_ids):
    return [
        (-i % bus_id, bus_id) # a_i, n_i
        for i, bus_id in enumerate(all_bus_ids)
        if bus_id is not None
    ]

def bezout_coefficients(a, b):
    "See https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm"
    prev_r, r = (a, b)
    prev_s, s = (1, 0)
    prev_t, t = (0, 1)
    
    while r != 0:
        quotient = prev_r // r
        prev_r, r = (r, prev_r - quotient * r)
        prev_s, s = (s, prev_s - quotient * s)
        prev_t, t = (t, prev_t - quotient * t)
    return prev_s, prev_t

def solve_chinese_remainder_problem(problem):
    assert len(problem) > 0
    a1, n1 = problem[0]
    N = n1 # Product of all n_i
    for a2, n2 in problem[1:]:
        N *= n2
        m1, m2 = bezout_coefficients(n1, n2)
        a1 = a1 * m2 * n2 + a2 * m1 * n1
        n1 = n1 * n2
    return a1 % N

if __name__ == '__main__':
    t0 = int(stdin.readline().strip())
    all_bus_ids = [
        int(bus_id) if bus_id != 'x' else None
        for bus_id in stdin.readline().strip().split(',')
    ]
    bus_ids = [bus_id for bus_id in all_bus_ids if bus_id != None]

    min_id = earliest_bus_id(t0, bus_ids)
    print(min_id * wait_time(t0, min_id))

    chinese_remainder_problem = make_chinese_remainder_problem(all_bus_ids)
    print(solve_chinese_remainder_problem(chinese_remainder_problem))
