import argparse
import colorama

from api import Event, UserId, get_cookiejar, pretty_print_years


# TODO: not constants
EVENT = Event("2020")
LEADERBOARD = UserId("")
SESSION: str = ""
COOKIEJAR = get_cookiejar(SESSION)


if __name__ == "__main__":
    colorama.init()

    parser = argparse.ArgumentParser()
    parser.set_defaults(subparser=None)
    subparsers = parser.add_subparsers()

    parser_pprint = subparsers.add_parser("pprint", aliases=["print", "show"])
    parser_pprint.set_defaults(subparser="pprint")
    parser_pprint_colour = parser_pprint.add_mutually_exclusive_group()
    parser_pprint_colour.add_argument(
        "-c", "--colour", action="store_true", default=False
    )
    parser_pprint_colour.add_argument(
        "--no-colour", dest="colour", action="store_false"
    )
    parser_pprint_long = parser_pprint.add_mutually_exclusive_group()
    parser_pprint_long.add_argument(
        "-l",
        "--long",
        dest="length",
        action="store_const",
        const="long",
        default="short",
    )
    parser_pprint_long.add_argument(
        "--short", dest="length", action="store_const", const="short"
    )
    parser_pprint_long.add_argument(
        "-s", "--summary", dest="length", action="store_const", const="summary"
    )
    parser_pprint.add_argument("-y", "--year", action="store", type=int, default=None)

    args = parser.parse_args()

    if args.subparser == "pprint":
        pretty_print_years(LEADERBOARD, cookiejar=COOKIEJAR, colour=args.colour, length=args.length, year=args.year)
    else:
        parser.print_help()
