from __future__ import annotations

import argparse
import colorama
from dataclasses import dataclass
from datetime import datetime
from dotenv import load_dotenv
import os
import sys
from typing import Any, Callable

from api import get_cookiejar
from api.api import LeaderboardYear, LeaderboardYearMember
from api.pretty import Length, iter_years, pretty_print_leaderboard, pretty_print_years
from api.types import ALL_DAYS, Day, Event, Part, UserId, to_event, to_user_id


load_dotenv()


SESSION: str = os.environ["ADVENTOFCODE_SESSION"]
COOKIEJAR = get_cookiejar(SESSION)


def parser_add_colour(
    parser: argparse.ArgumentParser, dest: str, set_default: bool = False
):
    colour = parser.add_mutually_exclusive_group()
    if set_default:
        colour.set_defaults(**{dest: None})
    colour.add_argument(
        "-c",
        "--colour",
        "--color",
        dest=dest,
        action="store_true",
        default=argparse.SUPPRESS,
    )
    colour.add_argument(
        "--no-colour",
        "--no-color",
        dest=dest,
        action="store_false",
        default=argparse.SUPPRESS,
    )
    return colour


def make_parser():
    parser = argparse.ArgumentParser()

    parser_add_colour(parser, dest="rawcolour", set_default=True)

    parser.set_defaults(parser=parser, subparser=main_default)
    subparsers = parser.add_subparsers()

    parser_pprint = subparsers.add_parser("pprint", aliases=["print", "show"])
    parser_pprint.set_defaults(subparser=main_pprint)
    parser_pprint.add_argument("leaderboard", action="store", type=to_user_id)
    parser_add_colour(parser_pprint, dest="rawcolour")
    parser_pprint_long = parser_pprint.add_mutually_exclusive_group()
    parser_pprint_long.set_defaults(length=Length.short)
    parser_pprint_long.add_argument(
        "-l", "--long", dest="length", action="store_const", const=Length.long
    )
    parser_pprint_long.add_argument(
        "--short", dest="length", action="store_const", const=Length.short
    )
    parser_pprint_long.add_argument(
        "-s", "--summary", dest="length", action="store_const", const=Length.summary
    )
    parser_pprint.add_argument(
        "-y", "--year", action="store", type=to_event, default=None
    )
    parser_pprint.add_argument(
        "-u", "--user", action="store", type=to_user_id, nargs="?", default=None
    )

    parser_timeline = subparsers.add_parser("timeline", aliases=["times", "tl"])
    parser_timeline.set_defaults(subparser=main_timeline)
    parser_timeline.add_argument("leaderboard", action="store", type=to_user_id)
    parser_add_colour(parser_timeline, dest="rawcolour")
    parser_timeline.add_argument(
        "-y", "--year", action="store", type=to_event, default=None
    )
    parser_timeline.add_argument(
        "-u", "--user", action="store", type=to_user_id, nargs="?", default=None
    )

    return parser


class Namespace(argparse.Namespace):
    parser: argparse.ArgumentParser
    subparser: Callable[[Namespace], Any]

    rawcolour: bool | None
    colour: bool


def parse_colour(colour: bool | None) -> bool:
    if colour is None:
        strip = None
    else:
        strip = not colour
    colorama.init(convert=None, strip=strip)
    if colour is not None:
        return colour
    if isinstance(sys.stdout, colorama.ansitowin32.StreamWrapper):
        converter: colorama.ansitowin32.AnsiToWin32 = (
            sys.stdout._StreamWrapper__convertor
        )
        return converter.convert
    return not strip


class EmptyNamespace(Namespace):
    pass


def main_default(args: EmptyNamespace):
    args.parser.print_help()


class PPrintNamespace(Namespace):
    leaderboard: UserId
    # colour: bool
    length: Length
    year: Event | None
    user: UserId | None


def main_pprint(args: PPrintNamespace):
    if args.user is None:
        args.user = args.leaderboard
    if args.year is None:
        for year in iter_years():
            pretty_print_years(
                args.user,
                year=year,
                cookiejar=COOKIEJAR,
                leaderboard=args.leaderboard,
                colour=args.colour,
                length=args.length,
            )
    else:
        pretty_print_leaderboard(
            args.leaderboard,
            year=args.year,
            cookiejar=COOKIEJAR,
            colour=args.colour,
            length=args.length,
        )


class TimelineNamespace(Namespace):
    leaderboard: UserId
    # colour: bool
    year: Event | None
    user: UserId | None


@dataclass(eq=True, frozen=True)
class TimelineEvent:
    time: datetime
    member: LeaderboardYearMember
    day: Day
    part: Part


def main_timeline_events(leaderboard: UserId, year: Event) -> set[TimelineEvent]:
    lb = LeaderboardYear(leaderboard, year)
    lb.fetch(COOKIEJAR)
    events: set[TimelineEvent] = set()
    for member in lb.members.values():
        for dayi in ALL_DAYS:
            day = member.days.get_day(dayi)
            for part, dt in day.items():
                if isinstance(dt, datetime):
                    events.add(
                        TimelineEvent(time=dt, member=member, day=dayi, part=part)
                    )
    return events


TIMELINE_STRFTIME = "%Y%m%d%H%M%S"
TIMELINE_STRFTIME_COLOUR = (
    colorama.Fore.LIGHTYELLOW_EX
    + "%Y"
    + colorama.Fore.LIGHTGREEN_EX
    + "%m"
    + colorama.Fore.LIGHTYELLOW_EX
    + "%d"
    + colorama.Fore.LIGHTMAGENTA_EX
    + "%H"
    + colorama.Fore.LIGHTBLUE_EX
    + "%M"
    + colorama.Fore.LIGHTMAGENTA_EX
    + "%S"
    + colorama.Fore.RESET
)


def main_timeline_row(
    e: TimelineEvent, colour: bool = False, include_year: bool = False
) -> str:
    s = e.time.strftime(TIMELINE_STRFTIME_COLOUR if colour else TIMELINE_STRFTIME)
    s += " "
    if colour:
        if include_year:
            s += (
                colorama.Fore.LIGHTYELLOW_EX
                + e.member.leaderboardyear.event.rjust(4)
                + colorama.Fore.LIGHTBLACK_EX
                + "."
            )
        s += (
            colorama.Fore.LIGHTGREEN_EX
            + e.day.rjust(2, " ")
            + colorama.Fore.LIGHTBLACK_EX
            + "."
            + (colorama.Fore.LIGHTYELLOW_EX if e.part == "2" else colorama.Fore.CYAN)
            + e.part
            + colorama.Fore.RESET
        )
    else:
        if include_year:
            s += e.member.leaderboardyear.event.rjust(4) + "."
        s += e.day.rjust(2, " ") + "." + e.part
    s += " "
    s += e.member.full_name
    return s


def main_timeline(args: TimelineNamespace):
    events: set[TimelineEvent]
    if args.year is None:
        events = set()
        for year in iter_years():
            events.update(main_timeline_events(args.leaderboard, year))
    else:
        events = main_timeline_events(args.leaderboard, args.year)

    if args.user is not None:
        events = {e for e in events if e.member.id == args.user}

    for e in sorted(events, key=lambda e: e.time):
        print(main_timeline_row(e, colour=args.colour, include_year=args.year is None))


if __name__ == "__main__":
    parser = make_parser()
    args = parser.parse_args(namespace=Namespace())
    args.colour = parse_colour(args.rawcolour)
    args.subparser(args)
