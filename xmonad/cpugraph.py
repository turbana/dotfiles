import calendar
import collections
import datetime
import dbus
import os
import os.path
import subprocess
import sys
import time

COLOR_FILE = os.path.expanduser("~/.etc/colors/current")


def color(c, _cache={}):
    if not _cache:
        with open(COLOR_FILE) as file:
            for line in file:
                name, value = line.split()
                _cache[name] = value
    return _cache[c]


def format(fg=None, bg=None):
    return (("^fg(%s)" % color(fg) if fg else "") +
            ("^bg(%s)" % color(bg) if bg else ""))


DELAY          = 2.0
SHOW_CPU_TOTAL = False

XBM_DIR      = os.path.expanduser("~/.xmonad/dzen2")
INBOX        = os.path.expanduser("~/mail/gmail-professional/inbox/new")
EVENTS       = os.path.expanduser("~/.cal")
IMAP_LOG     = os.path.expanduser("~/.offlineimap/stdout.log")

TIME_CHECK_LOG    = os.path.expanduser("~/.hours-worked")
TIME_CHECK_TARGET = 7.5
TIME_CHECK_START  = datetime.time(hour=16, minute=0)
TIME_CHECK_STALE  = 12

BAR_WIDTH        = 500
BAR_HEIGHT       = 24
GRAPH_HEIGHT     = 16
GRAPH_WIDTH      = 8
SPACE_DEFAULT    = 6
SPACE_LARGE      = 3 * SPACE_DEFAULT
POPUP_ROWS       = 16
POPUP_COLUMNS    = 62
CALENDAR_COLUMNS = 31
EVENTS_COLUMNS   = 31
ROOM_COLUMNS     = 19

COLOR_NORMAL        = color("base+3")
COLOR_HIGHLIGHT     = color("orange")
COLOR_GRAPH         = color("green")
COLOR_GRAPH_BG      = color("base-2")
COLOR_CLOCK         = color("green")
COLOR_URGENT        = color("orange")
#COLOR_EVENT         = color("blue")
COLOR_FAILURE       = color("red")
COLOR_BAR_HIGHLIGHT = color("base-2")

FORMAT_DAY_NAMES             = format("base+4", "base-2")
FORMAT_WEEKEND_OTHER_MONTH   = format("orange", "base-4")
FORMAT_WEEKEND_CURRENT_MONTH = format("orange", "base-3")
FORMAT_WEEKDAY_OTHER_MONTH   = format("base+2", "base-3")
FORMAT_WEEKDAY_CURRENT_MONTH = format("base+4", "base-2")
FORMAT_TODAY                 = format("base+4", "base-1")
FORMAT_MONTH                 = format("base+2", "base-3")
FORMAT_NORMAL                = format("base+3", "base-3")
#FORMAT_EVENT                 = format("green")
FORMAT_EVENT                 = format("yellow")
FORMAT_LINE_START            = "^pa(_LEFT)^p(250)"

EVENT_TODAY  = format("base+4", "base-2")
EVENT_WEEK   = format("base+4", "base-3")
EVENT_FUTURE = format("base+3", "base-4")
EVENT_TIME   = format("green")
EVENT_DATE   = format("orange")

WEATHER_STATION = "KPWT"
WEATHER_UPDATE  = 15 * 60

_WEATHER_TICKS = int(WEATHER_UPDATE / float(DELAY))
current_temp = [0]
have_event_today = [False]
file_changes = collections.defaultdict(int)
current_day = [None]


def main(args):
    try:
        weather_ticks = _WEATHER_TICKS
        prev_stats = list(cpu_stats())
        time.sleep(DELAY / 10.0)

        while True:
            # draw slave window
            if file_changed(EVENTS) or day_changed():
                today = datetime.date.today()
                sys.stdout.write("^cs()\n")
                events = parse_events(EVENTS)
                cal_str = build_calendar(today, events)
                event_str = build_events(today, events)
                sys.stdout.write(combine(event_str, cal_str, POPUP_COLUMNS))

            sys.stdout.write("^tw()^p(1)")

            # show alert icons
            # icon_check(need_time_tracking, COLOR_URGENT,  "clock.xbm")
            icon_check(mail_failure,       COLOR_FAILURE, "mail.xbm")
            icon_check(have_mail,          COLOR_URGENT,  "mail.xbm")
            icon_check(have_im,            COLOR_URGENT,  "info_02.xbm")

            # show cpu usages
            pad(SPACE_LARGE)
            color(COLOR_HIGHLIGHT)
            xbm("cpu.xbm")
            color(COLOR_NORMAL)
            pad(SPACE_DEFAULT)
            stats = list(cpu_stats())
            for pair in zip(stats, prev_stats):
                show_graph(usage(*pair))

            # show memory usages
            pad(SPACE_LARGE)
            color(COLOR_HIGHLIGHT)
            xbm("mem.xbm")
            color(COLOR_NORMAL)
            pad(SPACE_DEFAULT)
            ram, swap = memory_usages()
            show_graph(ram)
            show_graph(swap)

            # show weather
            weather_ticks += 1
            if weather_ticks >= _WEATHER_TICKS:
                weather_ticks = 0
                # XXX
                # update_weather()
            pad(SPACE_LARGE)
            color(COLOR_HIGHLIGHT)
            xbm("temp.xbm")
            color(COLOR_NORMAL)
            pad(SPACE_DEFAULT)
            sys.stdout.write("%d" % current_temp[0])

            # show clock
            pad(SPACE_LARGE)
            color(COLOR_HIGHLIGHT)
            if have_event():
                sys.stdout.write("^co(13x13)^p(-10)")
                color(COLOR_URGENT)
            sys.stdout.write("^ib(1)")
            xbm("clock.xbm")
            sys.stdout.write("^ib(0)")
            color(COLOR_NORMAL)
            pad(SPACE_DEFAULT)
            show_clock()

            pad(SPACE_DEFAULT)
            draw_bar_highlight()
            sys.stdout.write("\n")
            sys.stdout.flush()
            prev_stats = stats
            time.sleep(DELAY)
    except KeyboardInterrupt as e:
        pass


def draw_bar_highlight():
    color(COLOR_BAR_HIGHLIGHT)
    # sys.stdout.write("^ib(1)^ro(%dx1-%d-%d)^ib(0)" % (
    sys.stdout.write("^ib(1)^r(%dx1-%d-%d)^ib(0)" % (
        BAR_WIDTH, BAR_WIDTH, (BAR_HEIGHT/2)-1
    ))


def icon_check(check, color_, icon):
    if check():
        color(color_)
        xbm(icon)
        color(COLOR_NORMAL)
        pad(SPACE_DEFAULT)


def need_time_tracking():
    try:
        now = datetime.datetime.now()
        if TIME_CHECK_START < now.time():
            mtime = os.path.getmtime(TIME_CHECK_LOG)
            modified = datetime.datetime.fromtimestamp(mtime)
            stale = (TIME_CHECK_STALE * 60 * 60) < (now - modified).total_seconds()
            hours = open(TIME_CHECK_LOG).read().strip()
            hours = float(hours) if hours else 0.0
            return stale or hours < TIME_CHECK_TARGET
    except IOError:
        return True
    return False


def build_events(today, events):
    locals = ["\n", "", 0]
    def show(s):
        locals[1] += s
        locals[2] += len(s)
    def show_fmt(s):
        locals[1] += s
    def start_line():
        locals[1] = ""
        locals[2] = 0
    def end_line():
        missing = EVENTS_COLUMNS - locals[2]
        padding = " " * missing if missing > 0 else ""
        locals[0] += locals[1] + padding + "\n"
        start_line()
    for date, time, room, desc in events:
        room = " " + room if room else ""
        if date == today:
            format = EVENT_TODAY
            date = ""
        else:
            format = EVENT_FUTURE
            date = date.strftime(" (%m/%d)")
        start_line()
        show_fmt(format + EVENT_TIME)
        show(time)
        show_fmt(format + EVENT_DATE)
        show(date)
        show_fmt(format)
        if len(room) > ROOM_COLUMNS - 1:
            room = room[:ROOM_COLUMNS-2] + ">"
        show(room)
        end_line()
        show_fmt(format)
        if len(desc) > EVENTS_COLUMNS - 1:
            desc = desc[:EVENTS_COLUMNS-2] + ">"
        show(" %s" % desc)
        end_line()
    return locals[0]


def build_calendar(today, events):
    buffer = [""]
    def show(s):
        buffer[0] += s
    weekdays = calendar.day_name
    show(FORMAT_NORMAL + "   " + FORMAT_DAY_NAMES)
    for weekday in [weekdays[-1]] + weekdays[:-1]:
        show(" %2s " % weekday[:2])
    lines = 0
    print_month = 0
    month_i = 0
    have_event_today[0] = False
    for year, month, day, dayweek in iterdays(today.year, today.month):
        if dayweek == calendar.SUNDAY:
            show("\n")
            lines += 1
            if month != print_month:
                print_month = month
                month_i = 0
            if month_i < len(calendar.month_abbr[month]):
                show(FORMAT_MONTH)
                show(" %s " % calendar.month_abbr[month][month_i])
                month_i += 1
            else:
                show(FORMAT_NORMAL + "   ")
        weekend = dayweek in (calendar.SATURDAY, calendar.SUNDAY)
        inmonth = month == today.month
        inday = day == today.day
        if inmonth and inday:
            show(FORMAT_TODAY)
        elif weekend:
            if inmonth:
                show(FORMAT_WEEKEND_CURRENT_MONTH)
            else:
                show(FORMAT_WEEKEND_OTHER_MONTH)
        else:
            if inmonth:
                show(FORMAT_WEEKDAY_CURRENT_MONTH)
            else:
                show(FORMAT_WEEKDAY_OTHER_MONTH)
        if day == 0:
            day = " "
        else:
            for event in events:
                if event[0] == datetime.date(year, month, day):
                    show(FORMAT_EVENT)
                    if inmonth and inday:
                        have_event_today[0] = True
                    break
        show(" %2s " % day)
    show("\n")
    lines += 1
    while lines < POPUP_ROWS:
        show("\n")
        lines += 1
    return buffer[0]


def cpu_stats():
    with open("/proc/stat") as proc:
        for line in proc:
            if line.startswith("cpu"):
                if not SHOW_CPU_TOTAL and line.startswith("cpu "):
                    continue
                columns = [x for x in line.split(" ") if x]
                idle = int(columns[4])
                total = sum(map(int, columns[1:]))
                yield total, idle


def usage(current, previous, _x=[0.0]):
    (total, idle) = current
    (prev_total, prev_idle) = previous
    d_total = float(total - prev_total)
    d_idle  = float(idle  - prev_idle)
    return (d_total - d_idle) / d_total if d_total else 0.0


def memory_usages():
    with open("/proc/meminfo") as proc:
        for line in proc:
            key, val = line.split(":")
            val = int(val.strip(" kB\n"))
            if   key == "MemTotal":  mem_total = val
            elif key == "MemFree":   mem_free = val
            elif key == "Buffers":   mem_buffers = val
            elif key == "Cached":    mem_cached = val
            elif key == "SwapTotal": swap_total = val
            elif key == "SwapFree":  swap_free = val
    mem_usage = (mem_total - mem_free - mem_buffers - mem_cached) / float(mem_total)
    swap_usage = (swap_total - swap_free) / float(swap_total)
    return (mem_usage, swap_usage)


def show_graph(percent):
    w = GRAPH_WIDTH
    h = GRAPH_HEIGHT
    y = int(percent * h)
    offset = (h/2) - (y/2)
    sys.stdout.write("^ib(1)^fg(%s)^r(%dx%d)" % (COLOR_GRAPH_BG, w, h))
    sys.stdout.write("^fg(%s)" % COLOR_GRAPH)
    sys.stdout.write("^r(%dx%d-%d+%d)" % (w-2, y, w-1, offset))
    sys.stdout.write("^ib(0)^fg()")
    pad(SPACE_DEFAULT)


def show_clock():
    clock = datetime.datetime.now()
    color(COLOR_HIGHLIGHT)
    sys.stdout.write(clock.strftime("%a "))
    color(COLOR_NORMAL)
    sys.stdout.write(clock.strftime("%b %d "))
    color(COLOR_CLOCK)
    sys.stdout.write(clock.strftime("%H:%M"))


def update_weather():
    try:
        proc = subprocess.Popen("weather -q %s | grep '^Temp'" % WEATHER_STATION, shell=True, stdout=subprocess.PIPE)
        stdout, stderr = proc.communicate()
        current_temp[0] = float(stdout.split(" ")[1])
    except Exception as e:
        current_temp[0] = -100.0


def mail_failure():
    try:
        proc = subprocess.Popen("tail -n 15 %s | grep -ci '\(error\|Exception\)'" % IMAP_LOG,
                                  shell=True, stdout=subprocess.PIPE)
        #proc = subprocess.Popen("grep -c 'Exception in thread' %s" % IMAP_LOG, shell=True, stdout=subprocess.PIPE)
        stdout, stderr = proc.communicate()
        return int(stdout) > 0
    except Exception as e:
        return True


def have_mail():
    if not os.path.exists(INBOX):
        return False
    return len(os.listdir(INBOX)) > 0


def have_im():
    try:
        # for checking pidgin IMs
        bus = dbus.SessionBus()
        obj = bus.get_object("im.pidgin.purple.PurpleService", "/im/pidgin/purple/PurpleObject")
        purple = dbus.Interface(obj, "im.pidgin.purple.PurpleInterface")
        im = purple.PurplePrefsGetBool("/plugins/dbus/docklet/blink/im")
        chat = purple.PurplePrefsGetBool("/plugins/dbus/docklet/blink/chat")
        if im and chat:
            convs = purple.PurpleGetConversations()
        elif im:
            convs = purple.PurpleGetIms()
        elif chat:
            convs = purple.PurpleGetChats()
        else:
            convs = None
        for conv in convs:
            count = purple.PurpleConversationGetData(conv, "unseen-count")
            if count and count > 0:
                return True
    except dbus.exceptions.DBusException:
        pass
    return False


def have_event():
    return have_event_today[0]


def pad(pixels):
    sys.stdout.write("^p(%d)" % pixels)


def color(c):
    sys.stdout.write("^fg(%s)" % c)


def xbm(name):
    path = os.path.join(XBM_DIR, name)
    sys.stdout.write("^i(%s)" % path)


def iterdays(year, month):
    cal = calendar.Calendar(calendar.SUNDAY)
    months = [
        month_delta(year, month, -1),
        (year, month),
        month_delta(year, month, 1),
    ]
    dates = []
    for year, month in months:
        for week in cal.monthdays2calendar(year, month):
            for day, weekday in week:
                dates.append((year, month, day, weekday))
    DAY = 2
    count = 0
    for date in dates:
        # emit all non-empty days along with up to 6 leading and trailing empty days
        # 66 = 28 (days in feb) + 31 (days in march) + 7 (extra week)
        if date[DAY] or count < 7 or 66 < count:
            count += 1
            yield date


def month_delta(year, month, count):
    if count < -12 or count > 12:
        raise ValueError("count must be in range [-12, 12]")
    month += count
    if month < 1:
        month += 12
        year -=1
    elif month > 12:
        month -= 12
        year += 1
    return year, month


def parse_events(events_filename):
    events = []
    for line in open(events_filename):
        line = line.strip()
        if not line or line[0] == "#": continue
        tmp = [s.strip() for s in line.split("-")]
        desc = " ".join(tmp[1:])
        info = tmp[0].split(" ")
        if len(info) >= 3:
            date, time = info[0:2]
            room = " ".join(info[2:])
        elif len(info) == 2:
            date, time = info
            room = ""
        elif len(info) == 1:
            date = info[0]
            room = ""
            time = " " * 5
        else:
            print(len(info), info)
            print("error", line)
            # error parsing
            continue
        date = parse_date(date)
        time = time.zfill(5)
        #print "desc=", desc, "date=", date, "room=", room, "time=", time
        events.append((date, time, room, desc))
    today = datetime.date.today()
    events.sort(key=lambda t: (t[0], t[1]))
    return [e for e in events if e[0] >= today]


def parse_date(date_str):
    for fmt in ("%m/%d/%y", "%m/%d/%Y", "%m/%d"):
        try:
            date = datetime.datetime.strptime(date_str, fmt)
            if date.year == 1900:
                date = date.replace(year=datetime.date.today().year)
            return date.date()
        except ValueError:
            pass


def combine(left, right, cols):
    left_lines = left.split("\n")
    right_lines = right.split("\n")
    s = ""
    for _ in range(POPUP_ROWS):
        left = left_lines.pop(0) if left_lines else ""
        right = right_lines.pop(0) if right_lines else ""
        s += left + right + "\n"
    return s


def file_changed(filename):
    if not os.path.exists(filename): return False
    mtime = os.path.getmtime(filename)
    changed = file_changes[filename] != mtime
    file_changes[filename] = mtime
    return changed


def day_changed():
    today = datetime.date.today()
    if today != current_day[0]:
        current_day[0] = today
        return True
    return False


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
