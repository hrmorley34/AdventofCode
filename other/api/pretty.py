from __future__ import annotations

import datetime
from requests.cookies import RequestsCookieJar
from typing import Optional

from .api import LeaderboardYear, MemberDays, is_day_unlocked
from .types import ALL_DAYS, ALL_PARTS, Event, UserId


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
        clocked = COLOUR_LOCKEDSTAR, "*"
        creset = COLOUR_RESET
    else:
        cstar = "", "*"
        chalfstar = "", "/"
        cnostar = clocked = "", " "
        creset = ""

    outstr = ""
    lastcolour = None

    for day in ALL_DAYS:
        if not is_day_unlocked(memberdays.year, day):
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


def pretty_print_years(
    user: UserId,
    cookiejar: RequestsCookieJar,
    colour: bool = False,
    length: str = "short",
    year: Optional[Event] = None,
    show_missing: Optional[bool] = None,
):
    if show_missing is None:
        show_missing = year is not None
    if year is None:
        for iyear in range(2015, datetime.datetime.now().year + 1):
            pretty_print_years(
                user=user,
                cookiejar=cookiejar,
                colour=colour,
                length=length,
                year=Event(str(iyear)),
                show_missing=show_missing,
            )
        return

    ly = LeaderboardYear(user, year)
    if ly.fetch(cookiejar) is None:
        if show_missing:
            if length == "short":
                print("{:04} [{}]".format(year, "missing".center(25)))
            elif length == "long":
                print("{:04} [{}]".format(year, "missing".center(50)))
            else:
                print("{:04} [{}]".format(year, "gone".center(4)))
        return False
    if length in ("short", "long"):
        data = starstext(ly.members[user].days, colour, length == "short")
        print("{:04} [{}]".format(year, data))
    else:  # length == "summary"
        if colour:
            cstar = COLOUR_STAR
            creset = COLOUR_RESET
        else:
            cstar = creset = ""
        print("{:04} [{}{: 3}*{}]".format(year, cstar, ly.members[user].stars, creset))
