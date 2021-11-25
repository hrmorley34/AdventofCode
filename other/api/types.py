from __future__ import annotations


__all__ = [
    "UnixTimestamp",
    "UserId",
    "Event",
    "AnyEvent",
    "to_event",
    "Day",
    "ALL_DAYS",
    "AnyDay",
    "to_day_int",
    "to_day",
    "Part",
    "ALL_PARTS",
    "AnyPart",
    "to_part_int",
    "to_optional_part_int",
    "to_part",
    "to_optional_part",
    "ApiLeaderboardStarStats",
    "ApiLeaderboardDayStats",
    "ApiLeaderboardDayDict",
    "ApiLeaderboardUser",
    "ApiLeaderboard",
]


from typing import Dict, Literal, NewType, Optional, Sequence, TypedDict, Union, cast


UnixTimestamp = NewType("UnixTimestamp", int)
UserId = NewType("UserId", str)

Event = NewType("Event", str)
AnyEvent = Union[Event, str, int]


def to_event(event: AnyEvent) -> Event:
    return Event(str(event))


Day = Literal[
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15",
    "16",
    "17",
    "18",
    "19",
    "20",
    "21",
    "22",
    "23",
    "24",
    "25",
]
ALL_DAYS = cast(Sequence[Day], tuple(map(str, range(1, 26))))
AnyDay = Union[Day, str, int]


def to_day_int(day: AnyDay) -> int:
    day = int(day)
    if day < 1 or day > 25:
        raise ValueError
    return day


def to_day(day: AnyDay) -> Day:
    return cast(Day, str(to_day_int(day)))


Part = Literal["1", "2"]
ALL_PARTS = cast(Sequence[Part], ("1", "2"))
AnyPart = Union[Part, str, int]


def to_part_int(part: AnyPart) -> int:
    part = int(part)
    if part < 1 or part > 2:
        raise ValueError
    return part


def to_optional_part_int(part: Optional[AnyPart]) -> Optional[int]:
    if part is None:
        return None
    part = int(part)
    if part == 0:
        return None
    elif part < 1 or part > 2:
        raise ValueError
    return part


def to_part(part: AnyPart) -> Part:
    return cast(Part, str(to_part_int(part)))


def to_optional_part(part: Optional[AnyPart]) -> Optional[Part]:
    part = to_optional_part_int(part)
    if part is None:
        return None
    return cast(Part, str(part))


class ApiLeaderboardStarStats(TypedDict):
    get_star_ts: UnixTimestamp


ApiLeaderboardDayStats = Dict[Part, ApiLeaderboardStarStats]
ApiLeaderboardDayDict = Dict[Day, ApiLeaderboardDayStats]


class ApiLeaderboardUser(TypedDict):
    id: UserId
    name: str
    last_star_ts: UnixTimestamp
    stars: int
    local_score: int
    global_score: int
    completion_day_level: ApiLeaderboardDayDict


class ApiLeaderboard(TypedDict):
    event: Event
    owner_id: UserId
    members: Dict[UserId, ApiLeaderboardUser]
