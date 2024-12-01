import os
import sys
from datetime import datetime, timedelta, timezone

import requests
from dotenv import load_dotenv

from api import get_cookiejar

load_dotenv()


SESSION: str = os.environ.get("ADVENTOFCODE_SESSION", "")
COOKIEJAR = get_cookiejar(SESSION)


def get_input(year: int, day: int) -> bytes:
    url = f"https://adventofcode.com/{year}/day/{day}/input"
    r = requests.get(url, cookies=COOKIEJAR)

    # 400 for invalid token
    r.raise_for_status()
    return r.content


def print_help():
    print(
        f"""\
Usage:
    {sys.argv[0]} [[YEAR] DAY] | -h | --help""",
        file=sys.stderr,
    )


ERR = len(sys.argv) not in (1, 2, 3)
HELP = "-h" in sys.argv or "--help" in sys.argv
if ERR or HELP:
    print_help()
    exit(ERR)

TODAY = datetime.now(timezone(timedelta(hours=-5))).date()
if len(sys.argv) >= 3:
    YEAR = int(sys.argv[1])
else:
    YEAR = TODAY.year
    if TODAY.month < 12:
        YEAR -= 1  # assume we want the previous christmas
if len(sys.argv) >= 2:
    DAY = int(sys.argv[-1])
elif TODAY.month == 12 and TODAY.day <= 25:
    DAY = TODAY.day
else:
    print("DAY is only optional in December, when it can be inferred", file=sys.stderr)
    print_help()
    exit(1)


with os.fdopen(sys.stdout.fileno(), "wb", closefd=False) as f:
    f.write(get_input(YEAR, DAY))
    f.flush()
