# come up with color scheme colors

import sys

import colour


BASE_COLOR = "#334aab"

TRANS = {
    "base": 0,
    "compliment": 180,
    "triad_neg": -120,
    "triad_pos": 120,
    "tetrad_pos": 90,
    "tetrad_neg": -90,
    "analogous_neg": -22.5,
    "analogous_pos": 22.5,
    "split_compl_neg": -157.5,
    "split_compl_pos": 157.5,
}

COLORS = [
    ("base", "base"),
    ("orange", "compliment"),
    ("red", "triad_pos"),
    ("magenta", "tetrad_pos"),
    ("green", "tetrad_neg"),
    ("violet", "analogous_pos"),
    ("cyan", "analogous_neg"),
    ("yellow", "split_compl_neg"),
]


def translate(base, trans):
    c = colour.Color(base)
    delta = TRANS[trans] / 360.0
    c.hue = (c.hue + delta) % 1.0
    return c


def main(args):
    base = BASE_COLOR
    if len(args) == 1:
        base = args[0]
    for name, trans in COLORS:
        color = translate(base, trans)
        print "%s \"%s\"" % (name, color)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
