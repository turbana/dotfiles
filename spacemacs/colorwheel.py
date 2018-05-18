# come up with color scheme colors

import sys

import colour


BASE_BLUE = "#7091ce"
# BASE_BLUE = "#829ed7"

DARKEN_AMOUNT = 0.30

MAIN_COLORS = {
    "blue": 0,
    "orange": 180,
    "red": 120,
    "magenta": 90,
    "green": -90,
    "violet": 30,
    "cyan": -30,
    "yellow": -150,
}

DARK_LUM = 0.10
LIGHT_LUM = 0.90
GRAY_STEPS = (0, 1, 6, 7, 9, 10, 15, 16)


def translate(base, trans):
    c = colour.Color(base)
    delta = trans / 360.0
    c.hue = (c.hue + delta) % 1.0
    return c


def darken(color, amount):
    c = colour.Color(color)
    c.luminance -= amount
    return c


def load_color(name):
    return translate(BASE_BLUE, MAIN_COLORS[name])


def load_grays(dark_lum, light_lum):
    step_size = (light_lum - dark_lum) / 16.0
    for step in GRAY_STEPS:
        lum = dark_lum + (step * step_size)
        c = colour.Color()
        c.hsl = (0.0, 0.0, lum)
        yield c


def main(args):
    print ";; base grays"
    grays = list(load_grays(DARK_LUM, LIGHT_LUM))
    loop = zip(range(-4, 0) + range(1, 5),
               zip(grays, reversed(grays)))
    for n, (c1, c2) in loop:
        print "(base%+d   (if dark \"%s\" \"%s\"))" % (n, c1.hex_l, c2.hex_l)
    print "\n;; main colors"
    for name in MAIN_COLORS:
        color = load_color(name)
        dcolor = darken(color, DARKEN_AMOUNT)
        print "(%-8s (if dark \"%s\" \"%s\"))" % (
            name, color.hex_l, dcolor.hex_l)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
