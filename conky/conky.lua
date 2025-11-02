title_format = '${font Deja Vu Sans Mono:size=10}${color orange}'
value_format = '${font Deja Vu Sans Mono:size=10}${color green}'
clear_format = '${font}${color}'

function conky_title(...)
   local args={...}
   return conky_parse(title_format .. table.concat(args, ' ') .. clear_format)
end

function conky_value(...)
   local args={...}
   return conky_parse(value_format .. table.concat(args, ' ') .. clear_format)
end

function conky_convert(n, factor, offset)
   local value = (conky_parse(n) * factor) + offset
   return math.floor(value)
end

function conky_temp_cpu()
   return conky_parse("${hwmon coretemp temp 1}")
end

function conky_temp_nvme()
   return conky_parse("${hwmon nvme temp 1}")
end

function conky_temp_mobo()
   return conky_parse("${hwmon gigabyte_wmi temp 1}")
end
