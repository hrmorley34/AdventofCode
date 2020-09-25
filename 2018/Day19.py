program = """\
#ip 4
addi 4 16 4
seti 1 5 1
seti 1 2 2
mulr 1 2 3
eqrr 3 5 3
addr 3 4 4
addi 4 1 4
addr 1 0 0
addi 2 1 2
gtrr 2 5 3
addr 4 3 4
seti 2 7 4
addi 1 1 1
gtrr 1 5 3
addr 3 4 4
seti 1 9 4
mulr 4 4 4
addi 5 2 5
mulr 5 5 5
mulr 4 5 5
muli 5 11 5
addi 3 1 3
mulr 3 4 3
addi 3 18 3
addr 5 3 5
addr 4 0 4
seti 0 3 4
setr 4 2 3
mulr 3 4 3
addr 4 3 3
mulr 4 3 3
muli 3 14 3
mulr 3 4 3
addr 5 3 5
seti 0 4 0
seti 0 5 4"""
program = program.splitlines()
def make_command(line):
    command, *args = line.split(" ")
    args = list(map(int, args))
    return([command, args])
program = list(map(make_command, program))
ip_command, *program = program

from Day16 import Device as _old_Device
class Device(_old_Device):
    _reg = [0, 0, 0, 0, 0, 0]
    def __init__(self, A=0, B=0, C=0, D=0, E=0, F=0):
        self._reg = [A, B, C, D, E, F]
        self._ip = None
        self.opsl.append(self._ip_set)
        self.opsd["#ip"] = self._ip_set
    
    def _ip_set(self, P):
        self._ip = P
    def _ip_get(self):
        return(self._ip)
    def _ip_pos_set(self, P):
        self._reg[self._ip] = P
    def _ip_pos_get(self):
        return(self._reg[self._ip])
    ip = property(fget=_ip_get, fset=_ip_set)
    ip_pos = property(fget=_ip_pos_get, fset=_ip_pos_set)

    def loop(self):
        self.ip_pos += 1

if __name__ == "__main__":
    dev = Device(A=1)
    dev.opsd[ip_command[0]](*ip_command[1])
    while dev.ip_pos in range(0, len(program)):
        com, args = program[dev.ip_pos]
        dev.opsd[com](dev, *args)
        dev.loop()
    print(dev)
