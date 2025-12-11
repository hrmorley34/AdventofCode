import networkx as nx
from puzzle_input import puzzle_input


def parse_line(s: str) -> tuple[str, list[str]]:
    left, right = s.split(": ")
    return left, right.split()


def count_paths[T](g: "nx.Graph[T]", start: T, end: T, avoid: list[T] = []):
    subg = nx.subgraph(
        g,
        (
            n
            for n in g
            if nx.has_path(g, start, n) and nx.has_path(g, n, end) and n not in avoid
        ),
    )
    i = sum(1 for _ in nx.algorithms.all_simple_paths(subg, start, end))
    print(start, end, i)
    return i


def count_path_mincut[T](g: "nx.Graph[T]", start: T, end: T):
    subg = nx.subgraph(
        g, (n for n in g if nx.has_path(g, start, n) and nx.has_path(g, n, end))
    )
    min_cut = nx.algorithms.connectivity.minimum_st_node_cut(subg, start, end)
    i = sum(count_paths(subg, start, m) * count_paths(subg, m, end) for m in min_cut)
    print("*", start, end, i)
    return i


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    GRAPH = nx.DiGraph(dict(map(parse_line, PUZZLE_INPUT.splitlines())))
    paths = nx.algorithms.all_simple_paths(GRAPH, "you", "out")
    print("Part 1:", sum(1 for _ in paths))
    paths2_count = 0
    # In my graph, paths all look like svr -> fft -> dac -> out
    if nx.has_path(GRAPH, "fft", "dac"):
        paths2_count += (
            count_path_mincut(GRAPH, "svr", "fft")
            * count_path_mincut(GRAPH, "fft", "dac")
            * count_path_mincut(GRAPH, "dac", "out")
        )
    if nx.has_path(GRAPH, "dac", "fft"):
        paths2_count += (
            count_path_mincut(GRAPH, "svr", "dac")
            * count_path_mincut(GRAPH, "dac", "fft")
            * count_path_mincut(GRAPH, "fft", "out")
        )
    print("Part 2:", paths2_count)
