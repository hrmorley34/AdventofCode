from __future__ import annotations

from typing import Iterable


# PART 1


class Memory(list[int]):
    def __getitem__(self, y: int) -> int:
        assert y >= 0
        try:
            return list.__getitem__(self, y)
        except:
            return 0

    def __setitem__(self, key: int, value: int):
        assert key >= 0
        try:
            list.__setitem__(self, key, value)
        except:
            while len(self) < key:
                self.append(0)
            self.append(int(value))
            assert self[key] == int(value)

    def get_moded(self, key: int, mode: int, relative_base: int) -> int:
        if mode == 0:
            return self[self[key]]
        elif mode == 1:
            return self[key]
        elif mode == 2:
            return self[relative_base + self[key]]
        else:
            raise ValueError("Bad mode: {0}".format(mode))

    def set_moded(self, key: int, value: int, mode: int, relative_base: int):
        if mode == 0:
            self[self[key]] = value
        elif mode == 1:
            self[key] = value
        elif mode == 2:
            self[relative_base + self[key]] = value
        else:
            raise ValueError("Bad mode: {0}".format(mode))


class IntcodeComputer:
    memory: Memory
    program_counter: int
    queue: tuple[list[int], list[int]]
    relative_base: int

    def __init__(
        self,
        initial_memory: Iterable[int],
        inputqueue: list[int] | None = None,
        outputqueue: list[int] | None = None,
    ):
        self.memory = Memory(initial_memory)
        self.program_counter = 0
        if inputqueue is None:
            inputqueue = list()
        if outputqueue is None:
            outputqueue = list()
        self.queue = (inputqueue, outputqueue)
        self.relative_base = 0

    def run_until_input(self):
        memory = self.memory
        while True:
            operation = memory[self.program_counter]
            modes = [int(x) for x in str(operation)[-3::-1]]
            while len(modes) < 3:
                modes.append(0)
            opcode = int(str(operation)[-2:])
            if opcode == 1:  # +
                memory.set_moded(
                    self.program_counter + 3,
                    memory.get_moded(
                        self.program_counter + 1, modes[0], self.relative_base
                    )
                    + memory.get_moded(
                        self.program_counter + 2, modes[1], self.relative_base
                    ),
                    modes[2],
                    self.relative_base,
                )
                self.program_counter += 4
            elif opcode == 2:  # *
                memory.set_moded(
                    self.program_counter + 3,
                    memory.get_moded(
                        self.program_counter + 1, modes[0], self.relative_base
                    )
                    * memory.get_moded(
                        self.program_counter + 2, modes[1], self.relative_base
                    ),
                    modes[2],
                    self.relative_base,
                )
                self.program_counter += 4
            elif opcode == 3:  # IN
                if len(self.queue[0]):
                    memory.set_moded(
                        self.program_counter + 1,
                        self.queue[0].pop(0),
                        modes[0],
                        self.relative_base,
                    )
                    self.program_counter += 2
                else:
                    return
            elif opcode == 4:  # OUT
                self.queue[1].append(
                    memory.get_moded(
                        self.program_counter + 1, modes[0], self.relative_base
                    )
                )
                self.program_counter += 2
            elif opcode == 5:  # JUMP IF NON-ZERO
                if (
                    memory.get_moded(
                        self.program_counter + 1, modes[0], self.relative_base
                    )
                    != 0
                ):
                    self.program_counter = memory.get_moded(
                        self.program_counter + 2, modes[1], self.relative_base
                    )
                else:
                    self.program_counter += 3
            elif opcode == 6:  # JUMP IF ZERO
                if (
                    memory.get_moded(
                        self.program_counter + 1, modes[0], self.relative_base
                    )
                    == 0
                ):
                    self.program_counter = memory.get_moded(
                        self.program_counter + 2, modes[1], self.relative_base
                    )
                else:
                    self.program_counter += 3
            elif opcode == 7:  # <
                if memory.get_moded(
                    self.program_counter + 1, modes[0], self.relative_base
                ) < memory.get_moded(
                    self.program_counter + 2, modes[1], self.relative_base
                ):
                    v = 1
                else:
                    v = 0
                memory.set_moded(
                    self.program_counter + 3, v, modes[2], self.relative_base
                )
                self.program_counter += 4
            elif opcode == 8:  # ==
                if memory.get_moded(
                    self.program_counter + 1, modes[0], self.relative_base
                ) == memory.get_moded(
                    self.program_counter + 2, modes[1], self.relative_base
                ):
                    v = 1
                else:
                    v = 0
                memory.set_moded(
                    self.program_counter + 3, v, modes[2], self.relative_base
                )
                self.program_counter += 4
            elif opcode == 9:  # ADJUST REL_BASE
                self.relative_base += memory.get_moded(
                    self.program_counter + 1, modes[0], self.relative_base
                )
                self.program_counter += 2
            elif opcode == 99:
                return True
            else:
                raise Exception(f"Unknown opcode at {self.program_counter}: {opcode}")


if __name__ == "__main__":
    PUZZLE_INPUT = input("> ")
    PUZZLE_INPUT_INTS = [int(x) for x in PUZZLE_INPUT.split(",")]

    i = IntcodeComputer(PUZZLE_INPUT_INTS, [1])
    assert i.run_until_input()
    print(i.queue[1])

    # PART 2
    i = IntcodeComputer(PUZZLE_INPUT_INTS, [2])
    assert i.run_until_input()
    print(i.queue[1])
