no_steps = 359

pos = 0
lst = [0]

for x in range(1, 50000001):
    pos += no_steps
    pos %= len(lst)
    lst.insert(pos+1, x)
    pos += 1

print(lst[lst.index(0)+1])
