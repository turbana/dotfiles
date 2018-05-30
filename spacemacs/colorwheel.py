# come up with color scheme colors

import os.path
import StringIO
import subprocess
import sys

import colour

# directory where color files are exported to
COLOR_DIR = "~/.etc/colors"
EMACS_COLOR_THEME = "~/.etc/spacemacs/private-layers/themian/local/themian-theme/themian-theme.el"
EMACS_COLOR_THEME_MARK = ";;;; THEMIAN-COLORS"

# main blue color that all other colors are in reference to
# BASE_BLUE = "#748ed7"
BASE_BLUE = "#7091ce"
# luminance of darkest gray
DARK_LUM = 0.10
# luminance of lightest gray
LIGHT_LUM = 0.90
# scale of grays in multiples of steps
# step is defined as (LIGHT_LUM - DARK_LUM) / 16
GRAY_STEPS = (0, 1, 5, 7, 9, 11, 15, 16)
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


def load_colors():
    gray_names = ["base%+d" % i for i in range(-4, 0) + range(1, 5)]
    gray_dark = list(load_grays(DARK_LUM, LIGHT_LUM))
    gray_light = list(reversed(gray_dark))
    main_names = list(MAIN_COLORS.keys())
    main_dark = map(load_color, main_names)
    main_light = map(invert, main_dark)
    diff_names = ["diff-%d" % (i + 1) for i in range(4)]
    cyan = load_color("cyan")
    diffs = list(load_diffs(cyan, DIFF_DARK, DIFF_LIGHT, DIFF_STEP))
    diff_dark = [x[0] for x in diffs]
    diff_light = [x[1] for x in diffs]
    colors = {
        # "grays": (gray_names, grays, list(reversed(grays))),
        # "main": (main_names, main_dark, main_light),
        # "diff": (diff_names, diff_dark, diff_light),
        "names": gray_names + main_names + diff_names,
        "dark": gray_dark + main_dark + diff_dark,
        "light": gray_light + main_light + diff_light,
    }
    return colors


def emit_simple(colors, filename):
    with open(filename, "w") as out:
        for name, color in colors:
            out.write("%s %s\n" % (name, color.hex_l))


def emit_emacs(colors, filename):
    buffer_head = StringIO.StringIO()
    buffer_colors = StringIO.StringIO()
    buffer_tail = StringIO.StringIO()
    # load color theme buffers
    with open(filename) as file:
        # read head
        for line in file:
            buffer_head.write(line)
            if EMACS_COLOR_THEME_MARK in line:
                break
        # dump old colors
        for line in file:
            if EMACS_COLOR_THEME_MARK in line:
                buffer_tail.write(line)
                break
        # read tail
        for line in file:
            buffer_tail.write(line)
    # load color buffer
    loop = zip(colors["names"], colors["dark"], colors["light"])
    for name, dark, light in loop:
        buffer_colors.write("          (%-8s (if dark \"%s\" \"%s\"))\n" % (
            name, dark.hex_l, light.hex_l))
    # write color theme
    buffer_head.seek(0)
    buffer_colors.seek(0)
    buffer_tail.seek(0)
    with open(filename, "w") as file:
        map(file.write, buffer_head)
        map(file.write, buffer_colors)
        map(file.write, buffer_tail)


def reload_emacs():
    theme_file = os.path.expanduser(EMACS_COLOR_THEME)
    command = """emacsclient -e '(progn (load-file "%s")
        (load-theme '\\''themian-dark t))'"""
    subprocess.call(command.strip() % theme_file, shell=True)


def main(args):
    color_dir = os.path.expanduser(COLOR_DIR)
    colors = load_colors()
    dark_colors = zip(colors["names"], colors["dark"])
    light_colors = zip(colors["names"], colors["light"])
    emit_simple(dark_colors, os.path.join(color_dir, "dark"))
    emit_simple(light_colors, os.path.join(color_dir, "light"))
    emit_emacs(colors, os.path.expanduser(EMACS_COLOR_THEME))
    reload_emacs()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
