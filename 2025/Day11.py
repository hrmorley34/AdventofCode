import networkx as nx
from puzzle_input import puzzle_input


def parse_line(s: str) -> tuple[str, list[str]]:
    left, right = s.split(": ")
    return left, right.split()


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    GRAPH = nx.DiGraph(dict(map(parse_line, PUZZLE_INPUT.splitlines())))
    paths = nx.algorithms.all_simple_paths(GRAPH, "you", "out")
    print("Part 1:", sum(1 for _ in paths))
