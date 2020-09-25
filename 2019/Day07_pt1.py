# PART 1
PUZZLE_INPUT = input("> ")

def run_thruster(*args):
    memory = [int(x) for x in PUZZLE_INPUT.split(",")]
    
    INPUT_QUEUE = list(args)
    OUTPUT_QUEUE = []

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
            if len(INPUT_QUEUE):
                memory[mode_mem(pointer+1, 0)] = int(INPUT_QUEUE.pop(0))
            else:
                memory[mode_mem(pointer+1, 0)] = int(input("- "))
            pointer += 2
        elif opcode == 4: # OUT
            #print(memory[mode_mem(pointer+1, modes[0])])
            OUTPUT_QUEUE.append(memory[mode_mem(pointer+1, modes[0])])
            pointer += 2
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
        elif opcode == 99:
            #print("Finished")
            break
        else:
            raise Exception(f"Unknown opcode at {pointer}: {opcode}")
    return OUTPUT_QUEUE

top = (0, None)
for t1 in range(0, 5): # 0-4
    n1 = run_thruster(t1, 0)[0]
    for t2 in range(0, 5):
        if t2==t1: continue # no duplicate numbers
        n2 = run_thruster(t2, n1)[0]
        for t3 in range(0, 5):
            if t3 in (t1,t2): continue
            n3 = run_thruster(t3, n2)[0]
            for t4 in range(0, 5):
                if t4 in (t1,t2,t3): continue
                n4 = run_thruster(t4, n3)[0]
                for t5 in range(0, 5):
                    if t5 in (t1,t2,t3,t4): continue
                    n5 = run_thruster(t5, n4)[0]
                    if n5 > top[0]:
                        top = (n5, (t1,t2,t3,t4,t5))
print(top[0])
