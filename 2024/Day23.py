from collections import defaultdict

from puzzle_input import puzzle_input

if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    CONNECTIONS = defaultdict(set[str])
    for line in PUZZLE_INPUT:
        a, b = line.split("-")
        CONNECTIONS[a].add(b)
        CONNECTIONS[b].add(a)

    T_TRIPLES = set[frozenset[str]]()
    for a in CONNECTIONS:
        if a[0] != "t":
            continue
        for b in CONNECTIONS[a]:
            # if b == a:
            #     continue
            for c in CONNECTIONS[a] & CONNECTIONS[b]:
                # if c == a or c == b:
                #     continue
                # if not any(s[0] == "t" for s in (a, b, c)):
                #     continue
                T_TRIPLES.add(frozenset({a, b, c}))
    print("Part 1:", len(T_TRIPLES))

    LAN_PARTIES = set[frozenset[str]]()
    explorable = set[frozenset[str]]()
    for a in CONNECTIONS:
        explorable.add(frozenset({a}))
    while explorable:
        party = explorable.pop()
        connspace = set[str].intersection(*(CONNECTIONS[c] for c in party))
        if connspace:
            for m in connspace:
                explorable.add(party | {m})
        else:
            LAN_PARTIES.add(party)
    print("Part 2:", ",".join(sorted(max(LAN_PARTIES, key=len))))
