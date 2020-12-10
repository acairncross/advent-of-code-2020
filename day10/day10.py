from sys import stdin

jolts = sorted([int(line.strip()) for line in stdin])
jolts = [0] + jolts + [jolts[-1] + 3]

def count_diffs(jolts):
    diff1s = 0
    diff3s = 0
    for lo, hi in zip(jolts[:-1], jolts[1:]):
        if hi - lo == 1:
            diff1s += 1
        elif hi - lo == 3:
            diff3s += 1
    return diff1s * diff3s

def count_arrangements(jolts):
    maxjolt = max(jolts)
    arrangements = [0 for _ in range(maxjolt+1)]
    arrangements[-1] = 1
    jolt_idx = len(jolts) - 2
    for j in reversed(range(maxjolt)):
        if jolt_idx > 0 and j == jolts[jolt_idx]:
            for k in range(j+1, min(j+4, maxjolt+1)):
                arrangements[j] += arrangements[k]
            jolt_idx -= 1
    return arrangements[min(jolts)]

print(count_diffs(jolts))
print(count_arrangements(jolts))
