# come up with color scheme colors

import sys

import colour


BASE_BLUE = "#008dda"

COLOR_TRANS = {
    "base": 0,
    "orange": 180,
    "red": 120,
    "magenta": 90,
    "green": -90,
    "violet": 30,
    "cyan": -30,
    "yellow": -150,
}


def translate(base, trans):
    c = colour.Color(base)
    delta = trans / 360.0
    c.hue = (c.hue + delta) % 1.0
    return c


def main(args):
    blue = BASE_BLUE
    if len(args) == 1:
        blue = args[0]
    for name, trans in COLOR_TRANS.items():
        color = translate(blue, trans)
        print "%s \"%s\"" % (name, color)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
