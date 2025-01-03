from __future__ import annotations

import argparse
import os
import sys
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Callable

import colorama
from dotenv import load_dotenv

from api import get_cookiejar
from api.api import LeaderboardYear, LeaderboardYearMember
from api.pretty import Length, iter_years, pretty_print_leaderboard, pretty_print_years
from api.types import ALL_DAYS, Day, Event, Part, UserId, to_event, to_user_id

load_dotenv()


SESSION: str = os.environ.get("ADVENTOFCODE_SESSION", "")
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
        help="force colour, even on non-pty output",
    )
    colour.add_argument(
        "--no-colour",
        "--no-color",
        dest=dest,
        action="store_false",
        default=argparse.SUPPRESS,
        help="force colour off",
    )
    return colour


def make_parser():
    parser = argparse.ArgumentParser()

    parser_add_colour(parser, dest="rawcolour", set_default=True)

    parser.set_defaults(parser=parser, subparser=main_default)
    subparsers = parser.add_subparsers()

    parser_pprint = subparsers.add_parser(
        "pprint",
        aliases=["print", "show"],
        help="show the leaderboard",
    )
    parser_pprint.set_defaults(subparser=main_pprint)
    parser_pprint.add_argument("leaderboard", action="store", type=to_user_id)
    parser_add_colour(parser_pprint, dest="rawcolour")
    parser_pprint_long = parser_pprint.add_mutually_exclusive_group()
    parser_pprint_long.set_defaults(length=Length.short)
    parser_pprint_long.add_argument(
        "-l",
        "--long",
        dest="length",
        action="store_const",
        const=Length.long,
        help="show 50 stars",
    )
    parser_pprint_long.add_argument(
        "--short",
        dest="length",
        action="store_const",
        const=Length.short,
        help="show 25 stars or slashes (default)",
    )
    parser_pprint_long.add_argument(
        "-s",
        "--summary",
        dest="length",
        action="store_const",
        const=Length.summary,
        help="show the star count only",
    )
    parser_pprint.add_argument(
        "-y",
        "--year",
        action="store",
        type=to_event,
        default=None,
        help="display a single year (default: show all years)",
    )
    parser_pprint.add_argument(
        "-u",
        "--user",
        action="store",
        type=to_user_id,
        nargs="?",
        default=None,
        help=(
            "display a single user (default: show all in a year "
            "or the leaderboard owner in one year)"
        ),
    )
    parser_pprint.add_argument(
        "-n",
        "--top",
        action="store",
        type=int,
        default=None,
        help="limit the number of users shown",
    )

    parser_timeline = subparsers.add_parser(
        "timeline",
        aliases=["times", "tl"],
        help="show a timeline of solves",
    )
    parser_timeline.set_defaults(subparser=main_timeline)
    parser_timeline.add_argument("leaderboard", action="store", type=to_user_id)
    parser_add_colour(parser_timeline, dest="rawcolour")
    parser_timeline.add_argument(
        "-y",
        "--year",
        action="store",
        type=to_event,
        default=None,
        help="display a single year (default: show all years)",
    )
    parser_timeline.add_argument(
        "-u",
        "--user",
        action="store",
        type=to_user_id,
        nargs="?",
        default=None,
        help="display a single user (default: show all users)",
    )
    parser_timeline.add_argument(
        "-n",
        "--recent",
        action="store",
        type=int,
        default=None,
        help="limit the number of events shown",
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
    top: int | None


def main_pprint(args: PPrintNamespace):
    if args.user is None:
        args.user = args.leaderboard
    if args.year is None:
        if args.top is None:
            it = iter_years()
        else:
            it = list(iter_years())[-args.top :]

        for year in it:
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
            count=args.top,
        )


class TimelineNamespace(Namespace):
    leaderboard: UserId
    # colour: bool
    year: Event | None
    user: UserId | None
    recent: int | None


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
    colour_name = colour and (e.day, e.part) == ("25", "2")
    if colour_name:
        s += colorama.Fore.LIGHTYELLOW_EX
    s += e.member.full_name
    if colour_name:
        s += colorama.Fore.RESET
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

    if args.recent is None:
        sl = slice(None)
    else:
        sl = slice(-args.recent, None)

    for e in sorted(events, key=lambda e: e.time)[sl]:
        print(main_timeline_row(e, colour=args.colour, include_year=args.year is None))


if __name__ == "__main__":
    parser = make_parser()
    args = parser.parse_args(namespace=Namespace())
    if not SESSION and args.subparser != main_default:
        print(
            "No session token given - remember to set ADVENTOFCODE_SESSION.",
            file=sys.stderr,
        )
        exit(1)
    args.colour = parse_colour(args.rawcolour)
    args.subparser(args)
