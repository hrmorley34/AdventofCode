from __future__ import annotations

from datetime import date, datetime
from enum import Enum
from typing import Generator

from requests.cookies import RequestsCookieJar

from .api import (
    LeaderboardYear,
    LeaderboardYearMember,
    MemberDays,
    get_day_count_guess,
)
from .types import ALL_PARTS, Event, UserId, to_event


class Length(Enum):
    long = 1
    short = 2
    summary = 3


COLOUR_STAR = "\x1b[93m"  # light yellow
COLOUR_HALFSTAR = "\x1b[36m"  # cyan
COLOUR_NOSTAR = "\x1b[90m"  # light black
COLOUR_LOCKEDSTAR = "\x1b[30m"  # black
COLOUR_RESET = "\x1b[39m"  # reset


def starstext(memberdays: MemberDays, colours: bool = False, short: bool = True) -> str:
    if colours:
        cstar = COLOUR_STAR, "*"
        chalfstar = COLOUR_HALFSTAR, "*"
        cnostar = COLOUR_NOSTAR, "*"
        clocked = COLOUR_LOCKEDSTAR, " "
        creset = COLOUR_RESET
    else:
        cstar = "", "*"
        chalfstar = "", "/"
        cnostar = clocked = "", " "
        creset = ""

    outstr = ""
    lastcolour = None

    for day in memberdays.iter_days():
        if not memberdays.ly.is_day_unlocked(day):
            if lastcolour != clocked:
                outstr += clocked[0]
                lastcolour = clocked
            outstr += clocked[1] * (1 if short else 2)
            continue

        p1, p2 = (memberdays.get_day_part(day, p) for p in ALL_PARTS)
        if p1 and p2:
            if lastcolour != cstar:
                outstr += cstar[0]
                lastcolour = cstar
            outstr += cstar[1] * (1 if short else 2)
        elif p1:
            if lastcolour != chalfstar:
                outstr += chalfstar[0]
                lastcolour = chalfstar
            if short:
                outstr += chalfstar[1]
            else:
                outstr += cstar[1] + cnostar[0] + cnostar[1]
                lastcolour = cnostar
        else:
            if lastcolour != cnostar:
                outstr += cnostar[0]
                lastcolour = cnostar
            outstr += cnostar[1] * (1 if short else 2)

    outstr += creset
    return outstr


def leaderboard_line_pprint(
    member: LeaderboardYearMember, colour: bool, length: Length
) -> str:
    assert member.days is not None

    if length in (Length.short, Length.long):
        data = starstext(member.days, colour, length == Length.short)
        return "[{}]".format(data)
    else:  # length == "summary"
        if colour:
            cstar = COLOUR_STAR
            creset = COLOUR_RESET
        else:
            cstar = creset = ""
        return "[{}{: 3}*{}]".format(cstar, member.stars, creset)


def iter_years(until: date | None = None) -> Generator[Event, None, None]:
    if until is None:
        until = datetime.today()
    for iyear in range(2015, until.year):
        yield to_event(iyear)
    if until.month >= 11:
        yield to_event(until.year)


def pretty_print_years(
    user: UserId,
    year: Event,
    cookiejar: RequestsCookieJar,
    leaderboard: UserId | None = None,
    colour: bool = False,
    length: Length = Length.short,
    show_missing: bool = True,
):
    if leaderboard is None:
        leaderboard = user
    ly = LeaderboardYear(user, year)
    if ly.fetch(cookiejar) is None:
        if show_missing:
            if length == Length.short:
                width = get_day_count_guess(year)
                print("{:04} [{}]".format(year, "missing".center(width)))
            elif length == Length.long:
                width = get_day_count_guess(year) * 2
                print("{:04} [{}]".format(year, "missing".center(width)))
            else:
                print("{:04} [{}]".format(year, "gone".center(4)))
        return False
    print(
        "{:04} {}".format(
            year, leaderboard_line_pprint(ly.members[user], colour, length)
        )
    )


def pretty_print_leaderboard(
    leaderboard: UserId,
    year: Event,
    cookiejar: RequestsCookieJar,
    colour: bool = False,
    length: Length = Length.short,
    show_missing: bool = True,
    count: int | None = None,
):
    ly = LeaderboardYear(leaderboard, year)
    if ly.fetch(cookiejar) is None:
        return False
    assert ly.members is not None
    # namewidth = max(len(m.name) for m in ly.members.values())
    # namewidth = min(namewidth, 20)

    scorewidth = max(len(str(m.local_score)) for m in ly.members.values())

    for m in sorted(
        ly.members.values(),
        key=lambda m: (-m.local_score, -m.stars, m.name or "\xff", m.id),
    ):
        if not show_missing and m.stars <= 0:
            continue
        print(
            "{} {} {}".format(
                leaderboard_line_pprint(m, colour, length),
                str(m.local_score).rjust(scorewidth),
                m.full_name,
            )
        )
        if count is not None:
            count -= 1
            if count <= 0:
                break
