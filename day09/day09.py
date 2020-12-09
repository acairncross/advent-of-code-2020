from sys import stdin

def validate(datum, predecessors):
    for i in range(len(predecessors)):
        for j in range(len(predecessors)):
            if i != j:
                if predecessors[i] + predecessors[j] == datum:
                    return True
    return False

def find_first_invalid_datum(data):
    return [
        datum
        for i, datum in enumerate(encrypted_data[25:])
        if not validate(datum, encrypted_data[i:i+25])
    ][0]

def find_weakness(target, data):
    for i in range(len(data)):
        for j in range(i+1, len(data)):
            subseq = data[i:j+1]
            total = sum(subseq)
            if total == target:
                return min(subseq) + max(subseq)
            elif total > target:
                break

encrypted_data = [int(line.strip()) for line in stdin]
invalid_datum = find_first_invalid_datum(encrypted_data)
print(invalid_datum)
print(find_weakness(invalid_datum, encrypted_data))
