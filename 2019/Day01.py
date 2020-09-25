from puzzle_input import puzzle_input

# PART 1
PUZZLE_INPUT = puzzle_input()

def fuel_req(mass):
    return (mass // 3) - 2

totals = [fuel_req(int(m)) for m in PUZZLE_INPUT.splitlines()]

print(sum(totals))

# PART 2

subtotals = [fuel_req(int(m)) for m in totals if fuel_req(int(m))>0]
while len(subtotals):
    totals += subtotals
    subtotals = [fuel_req(int(m)) for m in subtotals if fuel_req(int(m))>0]

print(sum(totals))
