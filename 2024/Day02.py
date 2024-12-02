from puzzle_input import puzzle_input


def check_report(report: list[int]) -> bool:
    diffs = [a - b for a, b in zip(report, report[1:])]
    return (all(d < 0 for d in diffs) or all(d > 0 for d in diffs)) and all(
        1 <= abs(d) <= 3 for d in diffs
    )


def check_report_pops(report: list[int]) -> bool:
    if check_report(report):
        return True
    for i in range(len(report)):
        r = report.copy()
        r.pop(i)
        if check_report(r):
            return True
    return False


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    REPORTS = [[int(i) for i in line.split()] for line in PUZZLE_INPUT]

    print("Part 1:", sum(map(check_report, REPORTS)))
    print("Part 2:", sum(map(check_report_pops, REPORTS)))
