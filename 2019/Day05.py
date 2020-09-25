# PART 1
PUZZLE_INPUT = input("> ")
memory = [int(x) for x in PUZZLE_INPUT.split(",")]


def mode_mem(pointer, mode):
    if mode == 0:  return memory[int(pointer)] # location specified
    else: return int(pointer) # direct number

pointer = 0
while True:
    operation = memory[pointer]
    modes, opcode = [int(x) for x in str(operation)[-3::-1]], int(str(operation)[-2:])
    while len(modes) < 3: modes.append(0)
    if opcode == 1: # +
        memory[mode_mem(pointer+3, 0)] = \
                                  memory[mode_mem(pointer+1, modes[0])] \
                                + memory[mode_mem(pointer+2, modes[1])]
        pointer += 4
    elif opcode == 2: # *
        memory[mode_mem(pointer+3, 0)] = \
                                  memory[mode_mem(pointer+1, modes[0])] \
                                * memory[mode_mem(pointer+2, modes[1])]
        pointer += 4
    elif opcode == 3: # IN
        memory[mode_mem(pointer+1, 0)] = int(input("- "))
        pointer += 2
    elif opcode == 4: # OUT
        print(memory[mode_mem(pointer+1, modes[0])])
        pointer += 2
    # PART 2 ------
    elif opcode == 5: # jump if non-zero
        if memory[mode_mem(pointer+1, modes[0])] != 0:
            pointer = memory[mode_mem(pointer+2, modes[1])]
        else:
            pointer += 3
    elif opcode == 6: # jump if zero
        if memory[mode_mem(pointer+1, modes[0])] == 0:
            pointer = memory[mode_mem(pointer+2, modes[1])]
        else:
            pointer += 3
    elif opcode == 7: # <
        if memory[mode_mem(pointer+1, modes[0])] \
           < memory[mode_mem(pointer+2, modes[1])]:
            memory[mode_mem(pointer+3, 0)] = 1
        else:
            memory[mode_mem(pointer+3, 0)] = 0
        pointer += 4
    elif opcode == 8: # ==
        if memory[mode_mem(pointer+1, modes[0])] \
           == memory[mode_mem(pointer+2, modes[1])]:
            memory[mode_mem(pointer+3, 0)] = 1
        else:
            memory[mode_mem(pointer+3, 0)] = 0
        pointer += 4
    # --------------
    elif opcode == 99:
        print("Finished")
        break
    else:
        raise Exception(f"Unknown opcode at {pointer}: {opcode}")
