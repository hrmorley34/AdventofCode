from Day19 import Device, make_command

script = """\
#ip 3
seti 123 0 1
bani 1 456 1
eqri 1 72 1
addr 1 3 3
seti 0 0 3
seti 0 0 1
bori 1 65536 2
seti 10605201 9 1
bani 2 255 5
addr 1 5 1
bani 1 16777215 1
muli 1 65899 1
bani 1 16777215 1
gtir 256 2 5
addr 5 3 3
addi 3 1 3
seti 27 3 3
seti 0 3 5
addi 5 1 4
muli 4 256 4
gtrr 4 2 4
addr 4 3 3
addi 3 1 3
seti 25 3 3
addi 5 1 5
seti 17 5 3
setr 5 5 2
seti 7 6 3
eqrr 1 0 5
addr 5 3 3
seti 5 8 3"""
ip_command, *program = list(map(make_command, script.splitlines()))

if __name__ == "__main__":
    A_start = 0
    while True:
        print(A_start)
        seen_states = []
        dev = Device(A=A_start)
        dev.opsd[ip_command[0]](*ip_command[1])
        while dev.ip_pos in range(0, len(program)) and len(seen_states)<=10000000:
            if not len(seen_states)%1000000: print("-> "+str(len(seen_states)//1000000)+"M")
            seen_states.append(list(dev))
            com, args = program[dev.ip_pos]
            dev.opsd[com](dev, *args)
            dev.loop()
        if dev.ip_pos not in range(0, len(program)):
            break
        A_start += 1
