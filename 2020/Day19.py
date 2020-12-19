import pyparsing
import re
from collections import defaultdict
from puzzle_input import puzzle_input


RE_RULEPART = re.compile(r"(\d+)|\"(\w+)\"")


class NullAdd:
    def __init__(self):
        pass

    def __add__(self, o):
        return o

    def __or__(self, o):
        return o


def line_to_pyparsing(ruleline: str, ruledict: dict):
    num, rule = ruleline.split(":")
    parts = rule.split("|")
    parser = NullAdd()
    for part in parts:
        ms = RE_RULEPART.findall(part)
        parserpart = NullAdd()
        for m in ms:
            if m[0]:
                parserpart += ruledict[int(m[0])]
            else:
                parserpart += pyparsing.Literal(m[1])
        parser |= parserpart
    ruledict[int(num)] << parser


PUZZLE_INPUT = puzzle_input()
RULES, MESSAGES = PUZZLE_INPUT.split("\n\n")


RULEDICT = defaultdict(pyparsing.Forward)
for ruleline in RULES.splitlines():
    line_to_pyparsing(ruleline, RULEDICT)


count = 0
for msg in MESSAGES.splitlines():
    try:
        RULEDICT[0].parseString(msg, parseAll=True)
    except pyparsing.ParseException:
        pass
    else:
        count += 1
print(count)
