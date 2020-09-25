req = """Step U must be finished before step R can begin.
Step C must be finished before step B can begin.
Step A must be finished before step S can begin.
Step E must be finished before step O can begin.
Step D must be finished before step Z can begin.
Step L must be finished before step X can begin.
Step S must be finished before step F can begin.
Step B must be finished before step J can begin.
Step Z must be finished before step T can begin.
Step X must be finished before step W can begin.
Step K must be finished before step T can begin.
Step M must be finished before step H can begin.
Step I must be finished before step W can begin.
Step T must be finished before step J can begin.
Step N must be finished before step O can begin.
Step F must be finished before step G can begin.
Step W must be finished before step P can begin.
Step G must be finished before step V can begin.
Step Y must be finished before step V can begin.
Step J must be finished before step V can begin.
Step V must be finished before step R can begin.
Step P must be finished before step H can begin.
Step O must be finished before step R can begin.
Step H must be finished before step R can begin.
Step R must be finished before step Q can begin.
Step L must be finished before step O can begin.
Step V must be finished before step H can begin.
Step X must be finished before step K can begin.
Step D must be finished before step N can begin.
Step C must be finished before step P can begin.
Step E must be finished before step I can begin.
Step P must be finished before step O can begin.
Step T must be finished before step F can begin.
Step U must be finished before step K can begin.
Step A must be finished before step O can begin.
Step G must be finished before step O can begin.
Step A must be finished before step W can begin.
Step G must be finished before step Q can begin.
Step U must be finished before step J can begin.
Step V must be finished before step O can begin.
Step J must be finished before step Q can begin.
Step X must be finished before step G can begin.
Step B must be finished before step Y can begin.
Step J must be finished before step R can begin.
Step B must be finished before step F can begin.
Step K must be finished before step F can begin.
Step S must be finished before step Z can begin.
Step T must be finished before step H can begin.
Step W must be finished before step R can begin.
Step I must be finished before step N can begin.
Step Z must be finished before step R can begin.
Step J must be finished before step O can begin.
Step M must be finished before step R can begin.
Step Y must be finished before step J can begin.
Step E must be finished before step J can begin.
Step T must be finished before step G can begin.
Step T must be finished before step V can begin.
Step M must be finished before step O can begin.
Step C must be finished before step J can begin.
Step D must be finished before step O can begin.
Step F must be finished before step P can begin.
Step H must be finished before step Q can begin.
Step F must be finished before step J can begin.
Step Z must be finished before step P can begin.
Step T must be finished before step O can begin.
Step Z must be finished before step M can begin.
Step U must be finished before step H can begin.
Step W must be finished before step J can begin.
Step L must be finished before step Y can begin.
Step A must be finished before step T can begin.
Step M must be finished before step V can begin.
Step O must be finished before step Q can begin.
Step N must be finished before step J can begin.
Step A must be finished before step V can begin.
Step K must be finished before step G can begin.
Step N must be finished before step F can begin.
Step B must be finished before step T can begin.
Step I must be finished before step H can begin.
Step V must be finished before step P can begin.
Step E must be finished before step T can begin.
Step E must be finished before step G can begin.
Step U must be finished before step L can begin.
Step X must be finished before step P can begin.
Step L must be finished before step R can begin.
Step Y must be finished before step O can begin.
Step K must be finished before step O can begin.
Step Z must be finished before step I can begin.
Step P must be finished before step R can begin.
Step A must be finished before step X can begin.
Step O must be finished before step H can begin.
Step C must be finished before step D can begin.
Step D must be finished before step F can begin.
Step X must be finished before step H can begin.
Step D must be finished before step Y can begin.
Step Y must be finished before step P can begin.
Step E must be finished before step V can begin.
Step K must be finished before step H can begin.
Step M must be finished before step G can begin.
Step L must be finished before step I can begin.
Step D must be finished before step K can begin.
Step D must be finished before step M can begin."""

#req = """Step C must be finished before step A can begin.
#Step C must be finished before step F can begin.
#Step A must be finished before step B can begin.
#Step A must be finished before step D can begin.
#Step B must be finished before step E can begin.
#Step D must be finished before step E can begin.
#Step F must be finished before step E can begin."""

req = list(map(lambda r: (r.split(" ")[1], r.split(" ")[7]), req.split("\n")))

def evaluate_req(reqs):
    for r in reqs:
        if availabilitymap[r][0] not in [True, 1]: # not 0.5 for later
            return(False)
    return(True)

uniques = []
for a, b in req:
    if a not in uniques:
        uniques.append(a)
    if b not in uniques:
        uniques.append(b)
uniques.sort()

times = {}
for u in uniques:
    times[u] = ord(u.upper()) - 4 # A = 65 in ASCII -> A=61
workers = {}
for w in range(0, 5):
    workers[w] = [".", 0]

availabilitymap = {}
for u in uniques:
    availabilitymap[u] = [False, []]
for first, before in req:
    availabilitymap[before][1].append(first)

availabilitymap_keys = sorted(list(availabilitymap.keys())) # pre-sorted

order_a = []
while len(order_a) < len(availabilitymap):
    for k in availabilitymap_keys:
        if availabilitymap[k][0]:
            continue
        elif evaluate_req(availabilitymap[k][1]):
            availabilitymap[k][0] = True
            order_a.append(k)
            break
order_as = "".join(order_a)
print(order_as)

for k in availabilitymap:
    availabilitymap[k][0] = False # reset avmap

def update_queue():
    for k in availabilitymap_keys:
        if availabilitymap[k][0]:
            continue
        elif evaluate_req(availabilitymap[k][1]):
            availabilitymap[k][0] = 0.5
            queue.append(k)

order_t = []
queue = []
c_time = 0
while len(order_t) < len(availabilitymap):
    update_queue()
    for wkr, (wrkon, wrkleft) in workers.items():
        if wrkleft:
            wrkleft -= 1
            workers[wkr][1] -= 1
        if wrkon != "." and wrkleft <= 0:
            order_t.append(wrkon)
            availabilitymap[wrkon][0] = 1
            wrkon = "."
            workers[wkr][0] = wrkon
            update_queue()
        if len(queue) and wrkon == ".":
            wrkon = queue.pop(0)
            wrkleft = times[wrkon]
            workers[wkr] = [wrkon, wrkleft]
    c_time += 1
c_time -= 1

order_ts = "".join(order_t)
print(order_ts, "in", c_time, "secs")
