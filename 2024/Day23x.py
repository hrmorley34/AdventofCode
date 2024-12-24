from itertools import combinations

import networkx as nx
from puzzle_input import puzzle_input

if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    GRAPH = nx.Graph()
    for line in PUZZLE_INPUT:
        a, b = line.split("-")
        GRAPH.add_edge(a, b)

    # set guarantees uniqueness
    CLIQ3 = {
        frozenset(sg3)
        for sg in nx.find_cliques(GRAPH)
        for sg3 in combinations(sg, 3)
        if any(n[0] == "t" for n in sg3)
    }
    print("Part 1:", len(CLIQ3))

    print("Part 2:", ",".join(sorted(max(nx.find_cliques(GRAPH), key=len))))
