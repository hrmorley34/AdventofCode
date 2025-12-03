from dataclasses import dataclass

from puzzle_input import puzzle_input


@dataclass
class Bank:
    digits: str

    def max_jolt(self, digits: int = 2) -> int:
        # make the most significant digit as large as possible while
        # allowing full length
        prev_index = -1
        dstring = ""
        for i in range(-digits + 1, 1, 1):
            dstring += max(self.digits[prev_index + 1 : None if i >= 0 else i])
            prev_index = self.digits.index(dstring[-1], prev_index + 1)
        return int(dstring)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    banks = list(map(Bank, PUZZLE_INPUT))
    print("Part 1:", sum(b.max_jolt() for b in banks))
    print("Part 2:", sum(b.max_jolt(12) for b in banks))
