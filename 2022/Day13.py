import json
from puzzle_input import puzzle_input
from typing import Any

Packet = list["Packet"] | int


class PacketWrapper:
    packet: Packet

    def __init__(self, packet: Packet) -> None:
        self.packet = packet

    def __lt__(self, o: Any) -> bool:
        if isinstance(o, PacketWrapper):
            c = compare(self.packet, o.packet)
            return c is True
        return NotImplemented


def compare(left: Packet, right: Packet) -> bool | None:
    if not isinstance(left, list) and not isinstance(right, list):
        if left == right:
            return None
        return left < right
    else:
        if not isinstance(left, list):
            left = [left]
        if not isinstance(right, list):
            right = [right]
        index = 0
        while index < len(left) and index < len(right):
            c = compare(left[index], right[index])
            if c is not None:
                return c
            index += 1
        if len(left) == len(right):
            return None
        return len(left) < len(right)


def parse_packet(s: str) -> Packet:
    return json.loads(s)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    PAIRS_DATA = [line.split("\n") for line in PUZZLE_INPUT.split("\n\n")]
    PACKET_PAIRS = [
        (parse_packet(group[0]), parse_packet(group[1])) for group in PAIRS_DATA
    ]

    good_indicies = 0
    for i, (l, r) in enumerate(PACKET_PAIRS, 1):
        if compare(l, r):
            good_indicies += i
    print(f"Part 1: {good_indicies}")

    PACKET_LIST = [parse_packet(line) for line in PUZZLE_INPUT.splitlines() if line]
    SORTABLE_PACKET_LIST = [PacketWrapper(p) for p in PACKET_LIST]
    DIV_2 = PacketWrapper([[2]])
    DIV_6 = PacketWrapper([[6]])
    SORTABLE_PACKET_LIST.extend((DIV_2, DIV_6))
    SORTABLE_PACKET_LIST.sort()

    div_2_loc = div_6_loc = 0
    for i, p in enumerate(SORTABLE_PACKET_LIST, 1):
        if p is DIV_2:
            div_2_loc = i
        elif p is DIV_6:
            div_6_loc = i
    assert div_2_loc and div_6_loc
    print(f"Part 2: {div_2_loc*div_6_loc}")
