lines = """0: 3
1: 2
2: 4
4: 4
6: 5
8: 8
10: 6
12: 6
14: 8
16: 6
18: 6
20: 8
22: 12
24: 8
26: 8
28: 12
30: 8
32: 12
34: 9
36: 14
38: 12
40: 12
42: 12
44: 14
46: 14
48: 10
50: 14
52: 12
54: 14
56: 12
58: 17
60: 10
64: 14
66: 14
68: 12
70: 12
72: 18
74: 14
78: 14
82: 14
84: 24
86: 14
94: 14"""

def update(firewall):
    for k in firewall:
        firewall[k][1] += firewall[k][2]
        mx, c, d = firewall[k]
        if (c == mx-1 and d == 1) or (c == 0 and d == -1):
            firewall[k][2] *= -1

def config_firewall():
    global firewall
    firewall = {}
    for line in lines.split("\n"):
        k, v = line.split(": ")
        firewall[int(k)] = [int(v), 0, 1]

delay = 0
while True:
    severity = 0
    caught = 0
    config_firewall()
    for x in range(0, delay):
        update(firewall)
    for pos in range(0, max(firewall.keys())+1):
        if firewall.get(pos, [-1, -1, -1])[1] == 0:
            severity += pos * firewall[pos][0]
            caught += 1
        update(firewall)
    if caught == 0:
        break
    delay += 1
