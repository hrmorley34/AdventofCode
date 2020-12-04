from puzzle_input import puzzle_input
import re

PUZZLE_INPUT = puzzle_input().split("\n\n")


R_FIELDS = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
O_FIELDS = {"cid"}
validation = {
    "byr": lambda t: re.match(r"^(19[2-9][0-9]|200[0-2])$", t),
    "iyr": lambda t: re.match(r"^(201[0-9]|2020)$", t),
    "eyr": lambda t: re.match(r"^(202[0-9]|2030)$", t),
    "hgt": lambda t: re.match(r"^((1[5-8][0-9]|19[0-3])cm|(59|6[0-9]|7[0-6])in)$", t),
    "hcl": lambda t: re.match(r"^#[0-9a-f]{6}$", t),
    "ecl": lambda t: t in ("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    "pid": lambda t: re.match(r"^[0-9]{9}$", t),
    "cid": lambda t: True,
}

valid_count = 0
validfield_count = 0
for passport in PUZZLE_INPUT:
    fields = set()
    vfields = set()
    for kv in passport.split():
        k, v = kv.split(":")
        fields.add(k)
        if validation[k](v):
            vfields.add(k)

    if fields.issuperset(R_FIELDS):
        valid_count += 1
    if vfields.issuperset(R_FIELDS):
        validfield_count += 1

print(valid_count)
print(validfield_count)
