import pyparsing
from puzzle_input import puzzle_input


PExpr_eq = pyparsing.infixNotation(
    pyparsing.pyparsing_common.integer,
    [
        (pyparsing.oneOf("+ *"), 2, pyparsing.opAssoc.LEFT),
    ],
)
PExpr_addmult = pyparsing.infixNotation(
    pyparsing.pyparsing_common.integer,
    [
        (pyparsing.Literal("+"), 2, pyparsing.opAssoc.LEFT),
        (pyparsing.Literal("*"), 2, pyparsing.opAssoc.LEFT),
    ],
)


def PEval(parsedexpr: pyparsing.ParseResults):
    i = iter(parsedexpr)
    total = next(i)
    if isinstance(total, pyparsing.ParseResults):
        total = PEval(total)
    else:
        total = int(total)

    for op in i:
        arg = next(i)
        if isinstance(arg, pyparsing.ParseResults):
            arg = PEval(arg)
        else:
            arg = int(arg)
        if op == "*":
            total *= arg
        else:
            total += arg
    return total


PUZZLE_INPUT = puzzle_input().splitlines()
total = 0
total2 = 0
for line in PUZZLE_INPUT:
    e = PExpr_eq.parseString(line)
    o = PEval(e)
    total += o
    e2 = PExpr_addmult.parseString(line)
    o2 = PEval(e2)
    total2 += o2

print(total)
print(total2)
