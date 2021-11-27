from __future__ import annotations

import collections.abc
import datetime
import requests
from requests.cookies import RequestsCookieJar
from typing import (
    Any,
    Dict,
    Generator,
    List,
    Literal,
    MutableMapping,
    Optional,
    Tuple,
    Union,
    overload,
)
from weakref import WeakValueDictionary

from .types import (
    ALL_DAYS,
    ALL_PARTS,
    AnyDay,
    AnyPart,
    ApiLeaderboard,
    ApiLeaderboardDayDict,
    ApiLeaderboardUser,
    Day,
    Event,
    Part,
    UserId,
    to_day,
    to_day_int,
    to_optional_part,
    to_part,
)


class LeaderboardYear:
    _YEAROBJECTS: MutableMapping[
        tuple[UserId, Event], LeaderboardYear
    ] = WeakValueDictionary()
    id: UserId
    event: Event
    json: Optional[ApiLeaderboard] = None

    def __new__(cls, id: UserId, event: Event):
        id = UserId(str(id).rjust(6, "0"))
        event = Event(str(event))
        if (id, event) in cls._YEAROBJECTS:
            return cls._YEAROBJECTS[id, event]
        o = super().__new__(cls)
        cls._YEAROBJECTS[id, event] = o
        return o

    def __init__(self, id: UserId, year: Event):
        self.id = UserId(str(id).rjust(6, "0"))
        self.event = Event(str(year))

    def __hash__(self):
        return hash((self.id, self.event))

    def __repr__(self):
        return "LeaderboardYear({}, year={})".format(self.id, self.event)

    def __eq__(self, obj: Any):
        if isinstance(obj, LeaderboardYear):
            return self.id == obj.id and self.event == obj.event
        return NotImplemented

    def fetch(self, cookiejar: RequestsCookieJar):
        j = get_leaderboard_json(self.event, self.id, cookiejar)
        if j is not None:
            self.json = j
        return j

    @property
    def owner_id(self) -> UserId:
        return self.id

    @property
    def members(self) -> Optional[Dict[UserId, LeaderboardYearMember]]:
        if self.json is None:
            return None
        return {
            json["id"]: LeaderboardYearMember(self, json["id"], json)
            for json in self.json["members"].values()
        }


class LeaderboardYearMember:
    _YEARMEMBEROBJECTS: MutableMapping[
        tuple[LeaderboardYear, str], LeaderboardYearMember
    ] = WeakValueDictionary()
    id: UserId
    leaderboardyear: LeaderboardYear
    json: Optional[ApiLeaderboardUser] = None

    def __new__(
        cls, leaderboardyear: LeaderboardYear, id: UserId, *args: Any, **kwargs: Any
    ):
        id = UserId(str(id).rjust(6, "0"))
        if (leaderboardyear, id) in cls._YEARMEMBEROBJECTS:
            return cls._YEARMEMBEROBJECTS[leaderboardyear, id]
        o = super().__new__(cls)
        cls._YEARMEMBEROBJECTS[leaderboardyear, id] = o
        return o

    def __init__(
        self,
        leaderboardyear: LeaderboardYear,
        id: UserId,
        json: Optional[ApiLeaderboardUser] = None,
    ):
        self.id = id
        self.leaderboardyear = leaderboardyear
        self.json = json

    def __hash__(self):
        return hash((self.leaderboardyear, self.id))

    def __repr__(self):
        return "LeaderboardYearMember({}, leaderboard={}, year={})".format(
            self.id, self.leaderboardyear.id, self.leaderboardyear.event
        )

    def __eq__(self, obj: Any):
        if isinstance(obj, LeaderboardYearMember):
            return self.id == obj.id and self.leaderboardyear == obj.leaderboardyear
        return NotImplemented

    def fetch(self) -> Optional[ApiLeaderboardUser]:
        raise NotImplementedError

    @property
    def name(self) -> Optional[str]:
        if self.json is None:
            return None
        return self.json["name"]

    @property
    def owner(self) -> bool:
        return self.id == self.leaderboardyear.id

    @property
    def stars(self) -> Optional[int]:
        if self.json is None:
            return None
        return self.json["stars"]

    @property
    def local_score(self) -> Optional[int]:
        if self.json is None:
            return None
        return self.json["local_score"]

    @property
    def global_score(self) -> Optional[int]:
        if self.json is None:
            return None
        return self.json["global_score"]

    @property
    def days(self) -> Optional[MemberDays]:
        if self.json is None:
            return None
        return MemberDays(
            self.id, self.leaderboardyear.event, self.json["completion_day_level"]
        )


DT_F_N = Union[datetime.datetime, Literal[False], None]


class MemberDays:
    id: UserId
    year: Event
    json: Optional[ApiLeaderboardDayDict] = None

    def __init__(
        self, id: UserId, year: Event, json: Optional[ApiLeaderboardDayDict] = None
    ):
        self.id = id
        self.year = year
        if json is not None:
            self.json = json

    def __repr__(self):
        return "MemberDays({}, year={})".format(self.id, self.year)

    @overload
    def __getitem__(
        self, obj: Tuple[AnyDay] | Tuple[AnyDay, None] | int
    ) -> Dict[Part, DT_F_N]:
        ...

    @overload
    def __getitem__(self, obj: Tuple[AnyDay, AnyPart]) -> DT_F_N:
        ...

    @overload
    def __getitem__(self, obj: str | float) -> Dict[Part, DT_F_N] | DT_F_N:
        ...

    @overload
    def __getitem__(self, obj: slice) -> List[Dict[Part, DT_F_N]]:
        ...

    def __getitem__(self, obj: Any):
        if self.json is None:
            raise NotImplementedError

        if isinstance(obj, slice):
            # slightly imperfect
            if obj.step and obj.step < 0:
                start = min(int(obj.start or 25), 25)
                stop = max(int(obj.stop or 0), 0)
                step = -max(abs(int(obj.step or 1)), 1)
            else:
                start = max(int(obj.start or 1), 1)
                stop = min(int(obj.stop or 26), 26)
                step = max(int(obj.step or 1), 1)
            return [self.get_day(i) for i in range(start, stop, step)]

        day: Day
        part: Part | None = None

        if isinstance(obj, collections.abc.Sequence):
            if len(obj) > 2 or len(obj) < 1:
                raise ValueError
            day = to_day(obj[0])
            if len(obj) >= 2:
                part = to_optional_part(obj[1])
        elif isinstance(obj, int):
            day = to_day(obj)
        elif isinstance(obj, (float, str)):
            os = str(obj).split(".")
            if len(os) > 2 or len(os) < 1:
                raise ValueError
            day = to_day(os[0])
            part = to_part(os[1]) if len(os) >= 2 else None
        else:
            raise ValueError

        if part is None:
            return self.get_day(day)
        else:
            return self.get_day_part(day, part)

    def get_day(self, day: AnyDay) -> Dict[Part, DT_F_N]:
        if self.json is None:
            raise NotImplementedError
        day = to_day(day)

        if not is_day_unlocked(self.year, day):
            default = None
        else:
            default = False

        parts = self.json.get(day)
        if parts is None:
            return {"1": default, "2": default}
        else:
            d: Dict[Part, DT_F_N] = dict()
            for p in ALL_PARTS:
                ts = parts.get(p, {}).get("get_star_ts")
                if ts is None:
                    d[p] = default
                else:
                    d[p] = datetime.datetime.fromtimestamp(ts)
            return d

    def get_day_part(self, day: AnyDay, part: AnyPart) -> DT_F_N:
        if self.json is None:
            raise NotImplementedError
        day = to_day(day)
        part = to_part(part)

        if is_day_unlocked(self.year, day):
            default = False
        else:
            default = None

        ts = self.json.get(day, {}).get(part, {}).get("get_star_ts")
        if ts is None:
            return default
        else:
            return datetime.datetime.fromtimestamp(ts)

    def __iter__(self) -> Generator[Dict[Part, DT_F_N], None, None]:
        for d in ALL_DAYS:
            yield self.get_day(d)

    def to_dict(self) -> Dict[Day, Dict[Part, DT_F_N]]:
        return {d: self.get_day(d) for d in ALL_DAYS}


UNLOCK_TIME = datetime.timedelta(hours=5)


def is_day_unlocked(year: Union[Event, str, int], day: AnyDay) -> bool:
    return (
        datetime.datetime(int(year), 12, to_day_int(day)) + UNLOCK_TIME
        <= datetime.datetime.utcnow()
    )


def get_cookiejar(session_id: str) -> RequestsCookieJar:
    jar = RequestsCookieJar()
    jar.set(name="session", value=session_id, domain=".adventofcode.com", path="/")
    return jar


def get_leaderboard_json(
    year: str, leaderboard: str, cookiejar: RequestsCookieJar
) -> Optional[ApiLeaderboard]:
    url = f"https://adventofcode.com/{year}/leaderboard/private/view/{leaderboard}.json"
    r = requests.get(url, cookies=cookiejar)
    if r.history:
        # redirected -> main leaderboard; session is invalid/missing
        return None
    elif r.status_code != 200:
        return None
    else:
        return r.json()