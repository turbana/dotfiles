# come up with color scheme colors

import argparse
import gtk
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
BASE_BLUE = "#7596c8"

# hue transformations defined in degrees from BASE_BLUE
MAIN_COLORS = {
    "blue": 0,
    "orange": 190,
    "red": 130,
    "magenta": 100,
    "green": -100,
    "violet": 40,
    "cyan": -40,
    "yellow": -160,
}

# gray values in HSL
GRAYS = (
    # dark theme
    (0.6, 0.05, 0.07),
    (0.6, 0.05, 0.10),
    (0.6, 0.05, 0.15),
    (0.6, 0.05, 0.25),
    (0.6, 0.05, 0.40),
    (0.6, 0.05, 0.60),
    (0.6, 0.05, 0.75),
    (0.6, 0.05, 0.85),
    (0.6, 0.05, 0.90),
    # light theme
    (0.125, 0.90, 0.93),
    (0.125, 0.70, 0.90),
    (0.125, 0.50, 0.80),
    (0.125, 0.40, 0.65),
    (0.125, 0.40, 0.55),
    (0.125, 0.30, 0.40),
    (0.125, 0.30, 0.25),
    (0.125, 0.20, 0.15),
    (0.125, 0.10, 0.10),
)

# luminance of cyan for dark-mode diff colors
DIFF_DARK = 0.2
# luminance of cyan for light-mode diff colors
DIFF_LIGHT = 0.7
# luminance step size
DIFF_STEP = 0.05
# luminance steps
DIFF_STEPS = [0, 3, -3, -0]


def translate(base, trans):
    c = colour.Color(base)
    delta = trans / 360.0
    c.hue = (c.hue + delta) % 1.0
    return c


def invert(color):
    c = colour.Color(color)
    c.luminance = 1.0 - color.luminance
    return c


def load_color(name, base_color):
    return translate(base_color, MAIN_COLORS[name])


def load_grays():
    for hsl in GRAYS:
        c = colour.Color()
        c.hsl = hsl
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


def load_colors(base_color):
    def _load_color(c):
        return load_color(c, base_color)
    gray_names = ["base%+d" % i for i in range(-5, 0) + range(1, 5)]
    grays = list(load_grays())
    half = len(grays) / 2
    gray_dark = grays[:half]
    gray_light = grays[half:]
    main_names = list(MAIN_COLORS.keys())
    main_bg_names = [name+"-bg" for name in main_names]
    main_dark = map(_load_color, main_names)
    main_light = map(invert, main_dark)
    diff_names = ["diff-%d" % (i + 1) for i in range(4)]
    cyan = _load_color("cyan")
    diffs = list(load_diffs(cyan, DIFF_DARK, DIFF_LIGHT, DIFF_STEP))
    diff_dark = [x[0] for x in diffs]
    diff_light = [x[1] for x in diffs]
    colors = {
        "names": gray_names + main_names + main_bg_names + diff_names,
        "dark": gray_dark + main_dark + main_light + diff_dark,
        "light": gray_light + main_light + main_dark + diff_light,
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


def reload_theme():
    subprocess.call("set-color-theme", shell=True)


def current_blue():
    simple = os.path.join(os.path.expanduser(COLOR_DIR), "dark")
    with open(simple) as file:
        for line in file:
            if line.startswith("blue"):
                return line.split()[1]


def color_picker(initial_color):
    csd = gtk.ColorSelectionDialog("Gnome Color Chooser")
    cs = csd.colorsel
    cs.set_current_color(gtk.gdk.color_parse(initial_color))
    if csd.run() != gtk.RESPONSE_OK:
        return None
    color = str(cs.get_current_color())
    if len(color) > 7:
        color = color[0:3] + color[5:7] + color[9:11]
    return color


def parse_args():
    p = argparse.ArgumentParser()
    p.add_argument("--picker", help="use gtk color picker",
                   action="store_true")
    return p.parse_args()


def main(raw_args):
    args = parse_args()
    color_dir = os.path.expanduser(COLOR_DIR)
    if args.picker:
        base_color = color_picker(current_blue())
        if not base_color:
            return 0
    else:
        base_color = BASE_BLUE
    colors = load_colors(base_color)
    dark_colors = zip(colors["names"], colors["dark"])
    light_colors = zip(colors["names"], colors["light"])
    emit_simple(dark_colors, os.path.join(color_dir, "dark"))
    emit_simple(light_colors, os.path.join(color_dir, "light"))
    emit_emacs(colors, os.path.expanduser(EMACS_COLOR_THEME))
    reload_theme()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
