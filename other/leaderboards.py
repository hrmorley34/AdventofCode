from __future__ import annotations

import argparse
from typing import Any, Callable
import colorama
from dotenv import load_dotenv
import os

from api import get_cookiejar
from api.pretty import Length, iter_years, pretty_print_leaderboard, pretty_print_years
from api.types import Event, UserId, to_event, to_user_id


load_dotenv()


SESSION: str = os.environ["ADVENTOFCODE_SESSION"]
COOKIEJAR = get_cookiejar(SESSION)


def make_parser():
    parser = argparse.ArgumentParser()
    parser.set_defaults(parser=parser, subparser=main_default)
    subparsers = parser.add_subparsers()

    parser_pprint = subparsers.add_parser("pprint", aliases=["print", "show"])
    parser_pprint.set_defaults(subparser=main_pprint)
    parser_pprint.add_argument("leaderboard", action="store", type=to_user_id)
    parser_pprint_colour = parser_pprint.add_mutually_exclusive_group()
    parser_pprint_colour.add_argument(
        "-c", "--colour", action="store_true", default=False
    )
    parser_pprint_colour.add_argument(
        "--no-colour", dest="colour", action="store_false"
    )
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

    return parser


class Namespace(argparse.Namespace):
    parser: argparse.ArgumentParser
    subparser: Callable[[Namespace], Any]


class EmptyNamespace(Namespace):
    pass


def main_default(args: EmptyNamespace):
    args.parser.print_help()


class PPrintNamespace(Namespace):
    leaderboard: UserId
    colour: bool
    length: Length
    year: Event
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


if __name__ == "__main__":
    colorama.init()

    parser = make_parser()
    args = parser.parse_args(namespace=Namespace())
    args.subparser(args)
