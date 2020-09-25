programs = {}
test_amount = 2000

def create_entry(string):
    number, connections = string.split(" <-> ")
    number = int(number)
    connections = list(map(int, connections.split(", ")))
    programs[number] = [None, connections]

def get_group_mem(group):
    programs[group][0] = group
    for x in range(0, test_amount):
        test_connections(group)
    return(sum(map(lambda p: int(programs[p][0] == group), programs)))

def test_connections(group):
    for p in programs:
        if programs[p][0] is not None:
            continue
        for c in programs[p][1]:
            if programs[c][0] == group:
                programs[p][0] = group
                continue

def get_groups():
    n = 0
    groups = {}
    for n in range(0, len(programs)):
        if programs[n][0] is not None:
            continue
        print(n)
        groups[n] = get_group_mem(n)
        test_amount = len(programs) - sum(groups.values())
    return(groups)
