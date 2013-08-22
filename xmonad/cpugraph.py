import sys
import time
import os
import os.path
import datetime
import subprocess
import dbus

DELAY        = 2.0
SHOW_TOTAL   = False
XBM_DIR      = "~/.xmonad/dzen2"
INBOX        = "~/.mail/eastern/INBOX/new"

GRAPH_HEIGHT  = 16
GRAPH_WIDTH   = 8
SPACE_DEFAULT = 6

COLOR_NORMAL    = "#cccccc"
COLOR_HIGHLIGHT = "#999933"
COLOR_GRAPH     = "#006600"
COLOR_CLOCK     = "#33ff33"
COLOR_URGENT    = "#ffff33"

WEATHER_STATION = "KGEG"
WEATHER_UPDATE  = 60 * 60

_WEATHER_TICKS = int(WEATHER_UPDATE / float(DELAY))
current_temp = [0]

# for checking pidgin IMs
bus = dbus.SessionBus()
obj = bus.get_object("im.pidgin.purple.PurpleService", "/im/pidgin/purple/PurpleObject")
purple = dbus.Interface(obj, "im.pidgin.purple.PurpleInterface")

def main(args):
	try:
		weather_ticks = _WEATHER_TICKS
		prev_stats = list(cpu_stats())
		time.sleep(DELAY / 10.0)
		while True:
			# check for mail
			if have_mail():
				color(COLOR_URGENT)
				xbm("mail.xbm")
				color(COLOR_NORMAL)
				pad(SPACE_DEFAULT)

			# check for IMs
			if have_im():
				color(COLOR_URGENT)
				xbm("info_02.xbm")
				color(COLOR_NORMAL)
				pad(SPACE_DEFAULT)

			# show cpu usages
			pad(SPACE_DEFAULT * 4)
			color(COLOR_HIGHLIGHT)
			xbm("cpu.xbm")
			color(COLOR_NORMAL)
			pad(SPACE_DEFAULT)
			stats = list(cpu_stats())
			usages = map(usage, zip(stats, prev_stats))
			map(show_graph, usages)

			# show memory usages
			pad(SPACE_DEFAULT * 4)
			color(COLOR_HIGHLIGHT)
			xbm("mem.xbm")
			color(COLOR_NORMAL)
			pad(SPACE_DEFAULT)
			map(show_graph, memory_usages())

			# show weather
			weather_ticks += 1
			if weather_ticks >= _WEATHER_TICKS:
				weather_ticks = 0
				update_weather()
			pad(SPACE_DEFAULT * 4)
			color(COLOR_HIGHLIGHT)
			xbm("temp.xbm")
			color(COLOR_NORMAL)
			pad(SPACE_DEFAULT)
			sys.stdout.write("%d" % current_temp[0])

			# show clock
			pad(SPACE_DEFAULT * 4)
			color(COLOR_HIGHLIGHT)
			xbm("clock.xbm")
			color(COLOR_NORMAL)
			pad(SPACE_DEFAULT)
			show_clock()

			pad(SPACE_DEFAULT)
			sys.stdout.write("\n")
			sys.stdout.flush()
			prev_stats = stats
			time.sleep(DELAY)
	except KeyboardInterrupt, e:
		pass

def cpu_stats():
	with open("/proc/stat") as proc:
		for line in proc:
			if line.startswith("cpu"):
				if not SHOW_TOTAL and line.startswith("cpu "):
					continue
				columns = [x for x in line.split(" ") if x]
				idle = int(columns[4])
				total = sum(map(int, columns[1:]))
				yield total, idle

def usage(((total, idle), (prev_total, prev_idle)), _x=[0.0]):
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
			elif key == "SwapTotal": swap_total = val
			elif key == "SwapFree":  swap_free = val
	mem_usage = (mem_total - mem_free) / float(mem_total)
	swap_usage = (swap_total - swap_free) / float(swap_total)
	return (mem_usage, swap_usage)

def show_graph(load):
	w = GRAPH_WIDTH
	h = GRAPH_HEIGHT
	sys.stdout.write("^ib(1)^fg(%s)^r(%dx%d)" % (COLOR_NORMAL, w, h))
	x = int(load * h)
	y = (h/2) - (x/2)
	sys.stdout.write("^fg(%s)" % COLOR_GRAPH)
	sys.stdout.write("^r(%dx%d-%d+%d)" % (w-2, x, w-1, y))
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
	proc = subprocess.Popen("weather -qi KGEG", shell=True, stdout=subprocess.PIPE)
	stdout, stderr = proc.communicate()
	current_temp[0] = float(stdout.split(" ")[1])

def have_mail():
	return len(os.listdir(os.path.expanduser(INBOX))) > 0

def have_im():
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
	return False

def pad(pixels):
	sys.stdout.write("^p(%d)" % pixels)

def color(c):
	sys.stdout.write("^fg(%s)" % c)

def xbm(name):
	path = os.path.join(os.path.expanduser(XBM_DIR), name)
	sys.stdout.write("^i(%s)" % path)

if __name__ == "__main__":
	sys.exit(main(sys.argv[1:]))
