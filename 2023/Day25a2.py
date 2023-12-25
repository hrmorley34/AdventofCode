from __future__ import annotations

import networkx as nx
from puzzle_input import puzzle_input


def graph_from_lines(lines: list[str]) -> nx.Graph:
    G = nx.Graph()
    for line in lines:
        left, rights = line.split(": ")
        for right in rights.split():
            G.add_edge(left, right)
    return G


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    wires = graph_from_lines(PUZZLE_INPUT)

    cuts = list(nx.minimum_edge_cut(wires))
    wires.remove_edges_from(cuts)
    a, b = nx.connected_components(wires)
    print("Part 1:", len(a) * len(b))
