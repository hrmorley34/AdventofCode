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

def config_firewall():
    global firewall
    firewall = {}
    for line in lines.split("\n"):
        k, v = line.split(": ")
        firewall[int(k)] = int(v)

config_firewall()
delay = 0
while True:
    pos = delay
    fail = False
    for p in range(0, max(firewall.keys())+1):
        if p in firewall.keys() and pos % (firewall[p]*2 - 2) == 0:
            fail = True
            break
        pos += 1
    if not fail:
        print(delay)
        break
    delay += 1
