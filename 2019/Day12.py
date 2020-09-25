import re, collections
from puzzle_input import puzzle_input

# PART 1
class Moon:
    def __init__(self, pos, vel=(0,0,0)):
        self.pos, self.vel = list(pos), list(vel)
    def __repr__(self):
        return "Moon(pos=<{0[0]:3},{0[1]:3},{0[2]:3}>, vel=<{1[0]:3},{1[1]:3},{1[2]:3}>)".format(self.pos, self.vel)

    def apply_gravity(self, moons):
        for moon in moons:
            for dim in range(0, 3):
                if self.pos[dim] < moon.pos[dim]:
                    self.vel[dim] += 1
                elif self.pos[dim] > moon.pos[dim]:
                    self.vel[dim] -= 1
    def apply_velocity(self):
        self.pos = [p+v for (p,v) in zip(self.pos, self.vel)]

    @property
    def energy(self):
        return sum(map(abs, self.pos)) * sum(map(abs, self.vel))

class MoonSystem:
    def __init__(self, *moons):
        self.moons = []
        for moon in moons:
            if isinstance(moon, Moon): self.moons.append(moon)
            else: self.moons.append(Moon(moon))
        self._step_count = 0
    def __repr__(self):
        return "MoonSystem(\n  "+"\n  ".join(map(repr, self.moons))+")"

    def apply_gravity(self):
        for moon in self.moons:
            moon.apply_gravity(self.moons)
    def apply_velocity(self):
        for moon in self.moons:
            moon.apply_velocity()
    def step(self, n=1):
        for x in range(0, n):
            self.apply_gravity()
            self.apply_velocity()
            self._step_count += 1

    @property
    def energy(self):
        return sum((m.energy for m in self.moons))

##if __name__ == "__main__":
##    PUZZLE_INPUT = puzzle_input()
##    moons = []
##    pattern = re.compile("^<x=( *-?\d+), y=( *-?\d+), z=( *-?\d+)>$", re.MULTILINE)
##    for match in re.finditer(pattern, PUZZLE_INPUT):
##        pos = int(match.group(1)), int(match.group(2)), int(match.group(3))
##        moons.append(Moon(pos))
##
##    moonsystem = MoonSystem(*moons)
##    moonsystem.step(1000)
##    print(moonsystem.energy)

### PART 2 attempt 1 - TOO LONG
##class HistoryRepeatingItselfError(Exception): pass
##
##class MemorableMoonSystem(MoonSystem):
##    def __init__(self, *moons):
##        MoonSystem.__init__(self, *moons)
##        self.history = set()
##        self.add_history()
##    def add_history(self):
##        states = []
##        for moon in self.moons:
##            states.append((tuple(moon.pos), tuple(moon.vel)))
##        states = tuple(states)
##        if states in self.history:
##            raise HistoryRepeatingItselfError
##        self.history.add(tuple(states))
##    def step(self, n=1):
##        for x in range(0, n):
##            self.apply_gravity()
##            self.apply_velocity()
##            self._step_count += 1
##            try: self.add_history()
##            except HistoryRepeatingItselfError: pass
##    def step_until_repeat(self, max_=float("inf")):
##        start_count = self._step_count
##        while self._step_count - start_count < max_:
##            self.apply_gravity()
##            self.apply_velocity()
##            self._step_count += 1
##            try: self.add_history()
##            except HistoryRepeatingItselfError: return self._step_count
##        return False
##
##if __name__ == "__main__":
##    PUZZLE_INPUT = puzzle_input()
##    moons = []
##    pattern = re.compile("^<x=( *-?\d+), y=( *-?\d+), z=( *-?\d+)>$", re.MULTILINE)
##    for match in re.finditer(pattern, PUZZLE_INPUT):
##        pos = int(match.group(1)), int(match.group(2)), int(match.group(3))
##        moons.append(Moon(pos))
##
##    moonsystem = MemorableMoonSystem(*moons)
##    moonsystem.step(1000)
##    print(moonsystem.energy)
##
##    P = 6
##    c = moonsystem.step_until_repeat(10**P - 1000)
##    while not c:
##        print(">", moonsystem._step_count)
##        c = moonsystem.step_until_repeat(10**P)
##    print(c)

# PART 2 attempt 2 - much quicker
def prime_factors(n):
    i = 2
    factors = collections.Counter()
    while i * i <= n:
        if n % i:
            i += 1
        else:
            n //= i
            factors[i] += 1
    if n > 1:
        factors[n] += 1
    return factors
def compute_lcm(*ns):
    if len(ns) == 1 and isinstance(ns[0], (tuple, list)):
        ns = ns[0]
    lcm_factors = collections.Counter()
    ns = sorted(ns, reverse=True)
    while len(ns):
        x, *ns = ns
        f = prime_factors(x)
        for k in set(f.keys()) | set(lcm_factors.keys()):
            if lcm_factors[k] < f[k]:
                lcm_factors[k] = f[k]
    lcm = 1
    for k in lcm_factors.keys():
        lcm *= k**lcm_factors[k]
    return lcm

class MemorableMoonSystem(MoonSystem):
    def __init__(self, *moons):
        MoonSystem.__init__(self, *moons)
        self.history = (set(), set(), set())
        self.repeatpoint = (None, None, None)
        self.add_history()
    def add_history(self):
        states = [], [], []
        for moon in self.moons:
            for x in range(0, 3):
                states[x].append((moon.pos[x], moon.vel[x]))
        states = tuple(states)
        repeated = list(self.repeatpoint)
        for x in range(0, 3):
            if repeated[x] is None and tuple(states[x]) in self.history[x]:
                repeated[x] = self._step_count
            self.history[x].add(tuple(states[x]))
        self.repeatpoint = tuple(repeated)
    def step(self, n=1):
        for x in range(0, n):
            self.apply_gravity()
            self.apply_velocity()
            self._step_count += 1
            self.add_history()
    def step_until_repeat(self, max_=float("inf")):
        start_count = self._step_count
        while self._step_count - start_count < max_:
            self.apply_gravity()
            self.apply_velocity()
            self._step_count += 1
            self.add_history()
            if all(self.repeatpoint):
                return compute_lcm(self.repeatpoint)
        return False

if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    moons = []
    pattern = re.compile("^<x=( *-?\d+), y=( *-?\d+), z=( *-?\d+)>$", re.MULTILINE)
    for match in re.finditer(pattern, PUZZLE_INPUT):
        pos = int(match.group(1)), int(match.group(2)), int(match.group(3))
        moons.append(Moon(pos))

    moonsystem = MemorableMoonSystem(*moons)
    moonsystem.step(1000)
    print(moonsystem.energy)

    P = 6
    c = moonsystem.step_until_repeat(10**P - 1000)
    while not c:
        print(">", moonsystem._step_count)
        c = moonsystem.step_until_repeat(10**P)
    print(c)
