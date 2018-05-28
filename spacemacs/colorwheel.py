# come up with color scheme colors

import sys

import colour


# main blue color that all other colors are in reference to
BASE_BLUE = "#7091ce"
# luminance of darkest gray
DARK_LUM = 0.10
# luminance of lightest gray
LIGHT_LUM = 0.90
# scale of grays in multiples of steps
# step is defined as (LIGHT_LUM - DARK_LUM) / 16
GRAY_STEPS = (0, 1, 6, 7, 9, 10, 15, 16)
# luminance change when finding colors for light color scheme
DARKEN_AMOUNT = 0.30

# hue transformations defined in degrees from BASE_BLUE
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

# luminance of cyan for dark-mode diff colors
DIFF_DARK = 0.2
# luminance of cyan for light-mode diff colors
DIFF_LIGHT = 0.7
# luminance step size
DIFF_STEP = 0.05
# luminance steps
DIFF_STEPS = [1, 2, -2, -1]


def translate(base, trans):
    c = colour.Color(base)
    delta = trans / 360.0
    c.hue = (c.hue + delta) % 1.0
    return c


def invert(color):
    c = colour.Color(color)
    c.luminance = 1.0 - color.luminance
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


def load_diffs(color, dark_lum, light_lum, step):
    dark = colour.Color(color)
    dark.luminance = dark_lum
    light = colour.Color(color)
    light.luminance = light_lum
    for s in DIFF_STEPS:
        c1 = colour.Color(dark)
        c1.luminance += s * step
        c2 = colour.Color(light)
        c2.luminance -= s * step  # light color is inverted
        yield (c1, c2)


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
        dcolor = invert(color)
        print "(%-8s (if dark \"%s\" \"%s\"))" % (
            name, color.hex_l, dcolor.hex_l)
    print "\n;; diff colors"
    cyan = load_color("cyan")
    for i, (c1, c2) in enumerate(load_diffs(cyan, DIFF_DARK, DIFF_LIGHT,
                                            DIFF_STEP)):
        print "(diff-%d   (if dark \"%s\" \"%s\"))" % (i+1, c1, c2)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
