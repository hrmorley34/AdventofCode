from Day24 import Operation
from puzzle_input import puzzle_input


def show_graph(layer: int):
    import graphviz

    clayer = LAYERS[layer]
    dot = graphviz.Digraph()
    for s in clayer:
        if s[0] in "xy":
            dot.node(s)
            continue
        op = OPS_BY_RESULT[s]

        attr = {}
        if s in CARRIES.values():
            attr["color"] = "red" if s == CARRIES[layer] else "orange"
        dot.node(s, op.op + "\n" + s, **attr)
        for s2 in (op.left, op.right):
            if s2 not in clayer:
                attr = {}
                if s2 in CARRIES.values():
                    attr["color"] = "red" if s2 == CARRIES[layer - 1] else "yellow"
                dot.node(s2, **attr)
            dot.edge(s2, s)
    return dot


def check_layer(layer: int):
    clayer = LAYERS[layer]
    ops = [OPS_BY_RESULT[r] for r in clayer if r[0] not in "xy"]

    if layer == 0:
        (xor,) = {op for op in ops if op.op == "XOR"}
        (and_,) = {op for op in ops if op.op == "AND"}
        assert xor.dest == "z00"
        assert and_.dest == CARRIES[layer]
    else:
        (xor0,) = {
            op
            for op in ops
            if op.op == "XOR" and {op.left[0], op.right[0]} == {"x", "y"}
        }
        (and0,) = {
            op
            for op in ops
            if op.op == "AND" and {op.left[0], op.right[0]} == {"x", "y"}
        }

        (xor1,) = {op for op in ops if op.op == "XOR" and op != xor0}
        (and1,) = {op for op in ops if op.op == "AND" and op != and0}
        (or_,) = {op for op in ops if op.op == "OR"}

        assert {xor1.left, xor1.right} == {xor0.dest, CARRIES[layer - 1]}, xor1
        assert xor1.dest == f"z{layer:02}", xor1.dest
        assert {and1.left, and1.right} == {xor0.dest, CARRIES[layer - 1]}, and1
        assert {or_.left, or_.right} == {and0.dest, and1.dest}, or_
        assert or_.dest == CARRIES.get(layer, f"z{layer+1:02}"), or_.dest


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    data, ops = PUZZLE_INPUT.split("\n\n")
    VARS = {v: b == "1" for v, b in (line.split(": ") for line in data.splitlines())}
    OPS = set(map(Operation.from_line, ops.splitlines()))

    OPS_BY_RESULT = {op.dest: op for op in OPS}
    LAYERS: dict[int, set[str]] = {}
    CARRIES: dict[int, str] = {}
    operations = OPS.copy()
    prev_layers: set[str] = set()
    for layer in range(
        max(int(s[1:]) for op in OPS for s in (op.left, op.right) if s[0] == "x") + 1
    ):
        clayer = LAYERS[layer] = {f"x{layer:02}", f"y{layer:02}"}
        deps: set[str] = set()
        any_new = True
        while any_new:
            any_new = False
            for op in operations.copy():
                if {op.left, op.right} <= clayer | prev_layers:
                    clayer.add(op.dest)
                    operations.remove(op)
                    any_new = True
                    deps.update((op.left, op.right))
        prev_layers.update(clayer)

        if layer != 0:
            (CARRIES[layer - 1],) = LAYERS[layer - 1] & deps
    assert not operations

    for layer in range(max(LAYERS) + 1):
        try:
            check_layer(layer)
        except AssertionError:
            print(layer)
    # Use show_layer in Jupyter to find and fix errors
