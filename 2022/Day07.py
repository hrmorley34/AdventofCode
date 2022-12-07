from puzzle_input import puzzle_input


class Directory:
    name: str
    contents: dict[str, "Directory | File"]

    def __init__(self, name: str) -> None:
        self.name = name
        self.contents = {}

    def get_size(self) -> int:
        return sum(c.get_size() for c in self.contents.values())

    def part_1(self, threshold: int) -> int:
        output: list[int] = []
        for child in self.contents.values():
            if isinstance(child, Directory):
                output.append(child.part_1(threshold))
        total = self.get_size()
        if total < threshold:
            output.append(total)
        return sum(output)

    def part_2(self, threshold: int) -> tuple[str, int] | None:
        options: list[tuple[str, int]] = []
        for child in self.contents.values():
            if isinstance(child, Directory):
                opt = child.part_2(threshold)
                if opt is not None:
                    options.append(opt)
        options.append((self.name, self.get_size()))
        options = [o for o in options if o[1] >= threshold]
        if not options:
            return None
        return min(options, key=lambda o: o[1])


class File:
    name: str
    size: int

    def __init__(self, name: str, size: int) -> None:
        self.name = name
        self.size = size

    def get_size(self) -> int:
        return self.size


class TermParser:
    wd: list[str]
    last_command: str | None
    root: Directory

    def get_cwd(self) -> Directory:
        d = self.root
        for component in self.wd:
            d = d.contents[component]
            assert isinstance(d, Directory)
        return d

    def __init__(self) -> None:
        self.wd = []
        self.last_command = None
        self.root = Directory("")

    def parse_line(self, line: str) -> None:
        if line[0:2] == "$ ":
            self.parse_command(line[2:])
        else:
            self.parse_output(line)

    def parse_command(self, line: str) -> None:
        args = line.split()
        if args[0] == "cd":
            assert len(args) == 2
            if args[1] == "/":
                self.wd = []
            elif args[1] == "..":
                self.wd.pop(-1)
            else:
                self.wd.append(args[-1])
            self.last_command = None  # has no output
        elif args[0] == "ls":
            self.last_command = "ls"
        else:
            assert False

    def parse_output(self, line: str) -> None:
        assert self.last_command is not None
        if self.last_command == "ls":
            cwd = self.get_cwd()
            t, name = line.split()
            if t == "dir":
                o = Directory(name)
            else:
                o = File(name, int(t))
            cwd.contents[o.name] = o
        else:
            assert False


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    t = TermParser()
    for line in PUZZLE_INPUT:
        t.parse_line(line)
    print(f"Part 1: {t.root.part_1(100_000)}")
    total_size = t.root.get_size()
    free_on_disk = 70000000 - total_size
    threshold = 30000000 - free_on_disk
    rmd = t.root.part_2(threshold)
    assert rmd is not None
    print(f"Part 2: {rmd[1]}")
