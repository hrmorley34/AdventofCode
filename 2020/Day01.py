from puzzle_input import puzzle_input


PUZZLE_INPUT = puzzle_input()
Ns = [int(t) for t in PUZZLE_INPUT.splitlines() if str(t)]

# Part 1
for a in range(len(Ns)):
    for b in range(a + 1, len(Ns)):
        if Ns[a] + Ns[b] == 2020:
            print(str(Ns[a]).rjust(4), str(Ns[b]).rjust(4), str(Ns[a] * Ns[b]).rjust(17))

# Part 2
for a in range(len(Ns)):
    for b in range(a + 1, len(Ns)):
        for c in range(b + 1, len(Ns)):
            if Ns[a] + Ns[b] + Ns[c] == 2020:
                print(
                    str(Ns[a]).rjust(4),
                    str(Ns[b]).rjust(4),
                    str(Ns[c]).rjust(4),
                    str(Ns[a] * Ns[b] * Ns[c]).rjust(12),
                )
