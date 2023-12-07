from collections import Counter
from dataclasses import dataclass

from puzzle_input import puzzle_input

CARDS = list("AKQJT98765432")


def card_value(c: str) -> int:
    return -CARDS.index(c)


def get_type(hand: str) -> int:
    "higher number -> better hand"
    c = Counter(hand)
    countset = Counter(c.values())
    if countset == {5: 1}:
        return 10
    elif countset == {4: 1, 1: 1}:
        return 9
    elif countset == {3: 1, 2: 1}:
        return 8
    elif countset == {3: 1, 1: 2}:
        return 7
    elif countset == {2: 2, 1: 1}:
        return 6
    elif countset == {2: 1, 1: 3}:
        return 5
    else:
        assert countset == {1: 5}, countset
        return 0


def hand_lt(left: str, right: str) -> bool:
    "left < right -> -1; left > right -> 1"
    tl, tr = get_type(left), get_type(right)
    if tl != tr:
        return tl < tr

    for cl, cr in zip(left, right):
        if cl != cr:
            return card_value(cl) < card_value(cr)

    assert False
    return False


@dataclass
class Hand:
    hand: str
    bid: int

    @classmethod
    def from_line(cls, line: str) -> "Hand":
        hand, bid = line.split()
        return cls(hand=hand, bid=int(bid))

    def __lt__(self, __value: "Hand") -> bool:
        return hand_lt(self.hand, __value.hand)


CARDS2 = list("AKQT98765432J")


def card2_value(c: str) -> int:
    return -CARDS2.index(c)


def get_type2(hand: str) -> int:
    "higher number -> better hand"
    c = Counter(hand)
    jokers = c.pop("J", 0)
    countset = Counter(c.values())
    if countset == Counter({5 - jokers: 1}) or not countset:
        return 10
    elif countset == Counter({4 - jokers: 1}) + Counter({1: 1}):
        return 9
    elif countset == Counter({3 - jokers: 1}) + Counter({2: 1}):
        return 8
    elif countset == Counter({3 - jokers: 1}) + Counter({1: 2}):
        return 7
    elif countset == Counter({2 - jokers: 1}) + Counter({2: 1, 1: 1}):
        return 6
    elif countset == Counter({2 - jokers: 1}) + Counter({1: 3}):
        return 5
    else:
        assert countset == Counter({1: 5 - jokers}), countset
        return 0


def hand2_lt(left: str, right: str) -> bool:
    "left < right -> -1; left > right -> 1"
    tl, tr = get_type2(left), get_type2(right)
    if tl != tr:
        return tl < tr

    for cl, cr in zip(left, right):
        if cl != cr:
            return card2_value(cl) < card2_value(cr)

    assert False
    return False


@dataclass
class Hand2:
    hand: str
    bid: int

    @classmethod
    def from_line(cls, line: str) -> "Hand2":
        hand, bid = line.split()
        return cls(hand=hand, bid=int(bid))

    def __lt__(self, __value: "Hand2") -> bool:
        return hand2_lt(self.hand, __value.hand)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    hands = list(map(Hand.from_line, PUZZLE_INPUT))
    hands.sort(reverse=False)  # worst to best; first will get index 0->rank 1
    print("Part 1:", sum(i * h.bid for i, h in enumerate(hands, 1)))

    hands2 = list(map(Hand2.from_line, PUZZLE_INPUT))
    hands2.sort(reverse=False)  # worst to best; first will get index 0->rank 1
    print("Part 2:", sum(i * h.bid for i, h in enumerate(hands2, 1)))
