from __future__ import annotations

import collections.abc
from datetime import datetime, timedelta, timezone
from typing import (
    Any,
    ClassVar,
    Dict,
    Generator,
    List,
    Literal,
    MutableMapping,
    Optional,
    Sequence,
    Tuple,
    Union,
    overload,
)
from weakref import WeakValueDictionary

import requests
from requests.cookies import RequestsCookieJar

from .types import (
    ALL_DAYS,
    ALL_PARTS,
    AnyDay,
    AnyEvent,
    AnyPart,
    AnyUserId,
    ApiLeaderboard,
    ApiLeaderboardDayDict,
    ApiLeaderboardUser,
    Day,
    Event,
    Part,
    UserId,
    emptydict,
    to_day,
    to_day_int,
    to_event,
    to_optional_part,
    to_part,
    to_user_id,
)


class Missing(Exception):
    pass


class LeaderboardYear:
    _YEAROBJECTS: MutableMapping[tuple[UserId, Event], LeaderboardYear] = (
        WeakValueDictionary()
    )
    id: UserId
    event: Event
    json: Optional[ApiLeaderboard] = None

    def __new__(cls, id: AnyUserId, event: AnyEvent):
        id = to_user_id(id)
        event = to_event(event)
        if (id, event) in cls._YEAROBJECTS:
            return cls._YEAROBJECTS[id, event]
        o = super().__new__(cls)
        cls._YEAROBJECTS[id, event] = o
        return o

    def __init__(self, id: AnyUserId, event: AnyEvent):
        self.id = to_user_id(id)
        self.event = to_event(event)

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
    def members(self) -> Dict[UserId, LeaderboardYearMember]:
        if self.json is None:
            raise Missing
        return {
            json["id"]: LeaderboardYearMember(self, json["id"], json)
            for json in self.json["members"].values()
        }

    @property
    def day_count(self) -> int:
        if self.json is None:
            raise Missing
            return get_day_count_guess(self.event)
        return self.json["num_days"]

    @property
    def day1_unlock_time(self) -> datetime:
        if self.json is None:
            raise Missing
            return datetime(int(self.event), 12, 1, 0, tzinfo=UNLOCK_TZ_GUESS)
        return datetime.fromtimestamp(self.json["day1_ts"], timezone.utc)

    def day_unlock_time(self, day: AnyDay) -> datetime:
        return self.day1_unlock_time + timedelta(days=1) * (int(day) - 1)

    def is_day_unlocked(self, day: AnyDay) -> bool:
        return self.day_unlock_time(day) <= datetime.now(timezone.utc)


class LeaderboardYearMember:
    _YEARMEMBEROBJECTS: ClassVar[
        MutableMapping[tuple[LeaderboardYear, UserId], LeaderboardYearMember]
    ] = WeakValueDictionary()
    id: UserId
    leaderboardyear: LeaderboardYear
    json: Optional[ApiLeaderboardUser] = None

    def __new__(
        cls,
        leaderboardyear: LeaderboardYear,
        id: AnyUserId,
        *args: Any,
        **kwargs: Any,
    ):
        id = to_user_id(id)
        if (leaderboardyear, id) in cls._YEARMEMBEROBJECTS:
            return cls._YEARMEMBEROBJECTS[leaderboardyear, id]
        o = super().__new__(cls)
        cls._YEARMEMBEROBJECTS[leaderboardyear, id] = o
        return o

    def __init__(
        self,
        leaderboardyear: LeaderboardYear,
        id: AnyUserId,
        json: Optional[ApiLeaderboardUser] = None,
    ):
        self.id = to_user_id(id)
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
            raise Missing
        return self.json["name"]

    @property
    def full_name(self) -> str:
        return self.name or f"(anonymous user #{self.id})"

    @property
    def owner(self) -> bool:
        return self.id == self.leaderboardyear.id

    @property
    def stars(self) -> int:
        if self.json is None:
            raise Missing
        return self.json["stars"]

    @property
    def local_score(self) -> int:
        if self.json is None:
            raise Missing
        return self.json["local_score"]

    @property
    def global_score(self) -> int:
        if self.json is None:
            raise Missing
        return self.json["global_score"]

    @property
    def days(self) -> MemberDays:
        if self.json is None:
            raise Missing
        return MemberDays(self)


DT_F_N = Union[datetime, Literal[False], None]


def restrict_int(
    value: Any,
    default: int | None = None,
    minimum: int | None = None,
    maximum: int | None = None,
) -> int:
    if value is None and default is not None:
        return default
    result = int(value)
    if minimum is not None and result < minimum:
        return minimum
    if maximum is not None and result > maximum:
        return maximum
    return result


class MemberDays:
    lym: LeaderboardYearMember

    @property
    def ly(self) -> LeaderboardYear:
        return self.lym.leaderboardyear

    @property
    def id(self) -> UserId:
        return self.lym.id

    @property
    def year(self) -> Event:
        return self.ly.event

    @property
    def json(self) -> ApiLeaderboardDayDict | None:
        if self.lym.json is None:
            return None
        return self.lym.json["completion_day_level"]

    def __init__(self, lym: LeaderboardYearMember):
        self.lym = lym

    def __repr__(self):
        return "MemberDays({}, year={})".format(self.id, self.year)

    @overload
    def __getitem__(
        self, obj: Tuple[AnyDay] | Tuple[AnyDay, None] | int
    ) -> Dict[Part, DT_F_N]: ...
    @overload
    def __getitem__(self, obj: Tuple[AnyDay, AnyPart]) -> DT_F_N: ...
    @overload
    def __getitem__(self, obj: str | float) -> Dict[Part, DT_F_N] | DT_F_N: ...
    @overload
    def __getitem__(self, obj: slice) -> List[Dict[Part, DT_F_N]]: ...

    def __getitem__(
        self,
        obj: Tuple[AnyDay] | Tuple[AnyDay, AnyPart | None] | int | str | float | slice,
    ):
        if self.json is None:
            raise Missing

        if isinstance(obj, slice):
            # slightly imperfect
            if obj.step and obj.step < 0:
                start = restrict_int(
                    obj.start, self.ly.day_count, maximum=self.ly.day_count
                )
                stop = restrict_int(obj.stop, 0, minimum=0)
                step = -max(abs(restrict_int(obj.step, 1)), 1)
            else:
                start = restrict_int(obj.start, 1, minimum=1)
                stop = restrict_int(
                    obj.stop, self.ly.day_count + 1, maximum=self.ly.day_count + 1
                )
                step = restrict_int(obj.step, 1, minimum=1)
            return [self.get_day(i) for i in range(start, stop, step)]

        day: Day
        part: Part | None = None

        if isinstance(obj, int):
            day = to_day(obj)
        elif isinstance(obj, (float, str)):
            os = str(obj).split(".")
            if len(os) > 2 or len(os) < 1:
                raise ValueError
            day = to_day(os[0])
            part = to_part(os[1]) if len(os) >= 2 else None
        elif isinstance(obj, collections.abc.Sequence):
            if len(obj) > 2 or len(obj) < 1:
                raise ValueError
            day = to_day(obj[0])
            if len(obj) >= 2:
                part = to_optional_part(obj[1])
        else:
            raise ValueError

        if part is None:
            return self.get_day(day)
        else:
            return self.get_day_part(day, part)

    def get_day(self, day: AnyDay) -> Dict[Part, DT_F_N]:
        if self.json is None:
            raise Missing
        day = to_day(day)

        if not self.ly.is_day_unlocked(day):
            default = None
        else:
            default = False

        parts = self.json.get(day)
        if parts is None:
            return {"1": default, "2": default}
        else:
            d: Dict[Part, DT_F_N] = dict()
            for p in ALL_PARTS:
                ts = parts.get(p, emptydict).get("get_star_ts")
                if ts is None:
                    d[p] = default
                else:
                    d[p] = datetime.fromtimestamp(ts, timezone.utc)
            return d

    def get_day_part(self, day: AnyDay, part: AnyPart) -> DT_F_N:
        if self.json is None:
            raise Missing
        day = to_day(day)
        part = to_part(part)

        if self.ly.is_day_unlocked(day):
            default = False
        else:
            default = None

        ts = self.json.get(day, emptydict).get(part, emptydict).get("get_star_ts")
        if ts is None:
            return default
        else:
            return datetime.fromtimestamp(ts, timezone.utc)

    def __iter__(self) -> Generator[Dict[Part, DT_F_N], None, None]:
        for d in self.iter_days():
            yield self.get_day(d)

    def to_dict(self) -> Dict[Day, Dict[Part, DT_F_N]]:
        return {d: self.get_day(d) for d in self.iter_days()}

    def iter_days(self) -> Sequence[Day]:
        return ALL_DAYS[: self.ly.day_count]


UNLOCK_TZ_GUESS = timezone(timedelta(hours=-5))


def get_unlock_time_guess(year: AnyEvent, day: AnyDay) -> datetime:
    return datetime(int(year), 12, to_day_int(day), 0, tzinfo=UNLOCK_TZ_GUESS)


def is_day_unlocked_guess(year: AnyEvent, day: AnyDay) -> bool:
    return get_unlock_time_guess(year, day) <= datetime.now(UNLOCK_TZ_GUESS)


def get_day_count_guess(year: AnyEvent) -> int:
    return 25 if int(year) < 2025 else 12


def get_cookiejar(session_id: str) -> RequestsCookieJar:
    jar = RequestsCookieJar()
    jar.set(name="session", value=session_id, domain=".adventofcode.com", path="/")
    return jar


def get_leaderboard_json(
    year: AnyEvent, leaderboard: AnyUserId, cookiejar: RequestsCookieJar
) -> Optional[ApiLeaderboard]:
    url = f"https://adventofcode.com/{year}/leaderboard/private/view/{leaderboard}.json"
    r = requests.get(url, cookies=cookiejar)
    if r.history:
        # redirected -> main leaderboard; session is invalid/missing
        raise ValueError("Unexpected redirect; check your token")
    elif r.status_code != 200:
        r.raise_for_status()
    else:
        return r.json()
