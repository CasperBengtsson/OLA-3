# ============================================================
# Opgave 5.2 – Wind at Århus Lufthavn and Anholt Havn
# + where the storm hit hardest, and wind direction for the whole week
# Storm in October 2023 (storm peak 20–21 Oct)
# Data: DMI metObs API (no API key needed since Dec 2025)
# ============================================================

library(jsonlite)   # only used to read the JSON from the API

# ---- 1. Settings ------------------------------

base_url    <- "https://opendataapi.dmi.dk/v2/metObs/collections/observation/items"
station_url <- "https://opendataapi.dmi.dk/v2/metObs/collections/station/items"

stations <- c("Århus Lufthavn" = "06070", "Anholt Havn" = "06079")
colours  <- c("Århus Lufthavn" = "#1f5fbf", "Anholt Havn" = "#d1495b")

start_time <- "2023-10-16T00:00:00Z"
end_time   <- "2023-10-22T23:59:59Z"

# The storm in Danish time – used for the grey shading and for
# splitting the week into before / during / after (section 6)
storm_start <- as.POSIXct("2023-10-20 00:00", tz = "Europe/Copenhagen")
storm_end   <- as.POSIXct("2023-10-22 00:00", tz = "Europe/Copenhagen")

# Titles
plot_title    <- "Stormen rammer både Århus og Anholt hårdt"
plot_subtitle <- "Timemiddel af vindhastighed. Pilene viser hvor vinden blæser hen (hver 6. time)"
y_label       <- "Vindhastighed (m/s)"
x_label       <- "Dato"
source_text   <- "Kilde: DMI Frie Data, metObs API (opendataapi.dmi.dk)"

# "Normal" October wind speed, drawn as a dotted line.
# 6.0 m/s = DMI's national normal for October (reference period 1961–90).
# Set calculate_norm to TRUE to instead calculate it from the two stations'
# own October data 2013–2022 (section 3b) – takes 20 API calls.

national_norm  <- 6.0
norm_speed     <- national_norm

# Storm by definition: mean wind of 24.5 m/s or more (Beaufort 10)
storm_limit <- 24.5
storm_label <- "Stormgrænse: 24,5 m/s"
norm_label     <- "Normal oktober (landsgennemsnit)"
calculate_norm <- TRUE


# ---- 2. Function: get one parameter for one station ---------

# from/to default to the storm week, but can be changed (used in 3b)
get_obs <- function(station_id, parameter, from = start_time, to = end_time) {
  url <- paste0(base_url,
                "?stationId=",   station_id,
                "&parameterId=", parameter,
                "&datetime=",    from, "/", to,
                "&limit=10000")
  result <- fromJSON(url)
  
  if (length(result$features) == 0) {
    stop("No data returned for ", station_id, " / ", parameter)
  }
  
  # The useful columns are inside features$properties
  df <- result$features$properties[, c("observed", "value")]
  names(df)[2] <- parameter
  df
}


# ---- 3. Get speed + direction for a set of stations ----------
# Made into a function so we can reuse it for the hardest-hit
# stations in section 7.

# from/to default to the October 2023 week; section 9 uses November 2022
get_wind_data <- function(station_list, from = start_time, to = end_time) {
  wind <- data.frame()
  
  for (name in names(station_list)) {
    speed     <- get_obs(station_list[name], "wind_speed_past1h", from, to)
    direction <- get_obs(station_list[name], "wind_dir_past1h",   from, to)
    
    one <- merge(speed, direction, by = "observed")   # match on time
    one$station <- name
    wind <- rbind(wind, one)
  }
  
  # Text -> date-time (API is in UTC), then show it in Danish time
  wind$time <- as.POSIXct(wind$observed,
                          format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  attr(wind$time, "tzone") <- "Europe/Copenhagen"
  
  wind[order(wind$station, wind$time), ]
}

all_data <- get_wind_data(stations)
head(all_data)


# ---- 3b. Calculate the October norm from the data ----
# Mean hourly wind speed for October, both stations, 2013–2022.
# Only runs if calculate_norm is TRUE in section 1.

if (calculate_norm) {
  october_means <- c()
  
  for (name in names(stations)) {
    for (year in 2013:2022) {
      oct <- get_obs(stations[name], "wind_speed_past1h",
                     from = paste0(year, "-10-01T00:00:00Z"),
                     to   = paste0(year, "-10-31T23:59:59Z"))
      october_means <- c(october_means, mean(oct$wind_speed_past1h, na.rm = TRUE))
    }
  }
  
  norm_speed <- round(mean(october_means), 1)
  norm_label <- "Normal oktober (gns. 2013–2022)"
  cat("Calculated October norm:", norm_speed, "m/s\n")
}


# ---- 4. Helper: draw wind arrows ----------------------------
# wind_dir = the direction the wind comes FROM (0/360 = north).
# The arrow points the way the wind blows TO (opposite direction).
# Work in inches so the arrows are not stretched by the axis scales.

draw_wind_arrows <- function(time, speed, direction, col, arrow_length = 0.2) {
  x_in <- grconvertX(as.numeric(time), "user", "inches")
  y_in <- grconvertY(speed,            "user", "inches")
  
  radians <- direction * pi / 180
  x_end <- x_in - arrow_length * sin(radians)
  y_end <- y_in - arrow_length * cos(radians)
  
  arrows(x0 = time, y0 = speed,
         x1 = grconvertX(x_end, "inches", "user"),
         y1 = grconvertY(y_end, "inches", "user"),
         length = 0.06, col = col, lwd = 1.6)
}


# ---- 5. The wind graph (as functions, so they can be reused) ----
# draw_wind_graph() only DRAWS the graph (lines, axes, shading, legend).
# plot_wind() wraps it: opens a png file, draws, adds titles, saves.
# Splitting it this way lets section 9 draw the same graph inside a gif.
#
# Options (all have defaults, so the October calls don't need them):
#   storm_line  = TRUE  -> dotted line at the storm limit (24.5 m/s)
#   show_arrows = FALSE -> no direction arrows on the lines
#   shade_from / shade_to / shade_label -> which period is shaded grey
#   now = a time    -> a black vertical "we are here" line (used in the gif)

draw_wind_graph <- function(data, station_list, station_colours,
                            norm, norm_text, storm_line = FALSE,
                            show_arrows = TRUE,
                            shade_from = storm_start, shade_to = storm_end,
                            shade_label = "Storm", now = NULL) {
  
  y_max   <- ceiling(max(data$wind_speed_past1h, na.rm = TRUE)) + 2
  if (storm_line) y_max <- max(y_max, storm_limit + 2)   # make sure the line fits
  y_ticks <- seq(0, y_max, by = 2)
  
  plot(data$time, data$wind_speed_past1h, type = "n",
       ylim = c(0, y_max),
       xaxt = "n", yaxt = "n",
       xlab = x_label, ylab = y_label)
  
  axis(2, at = y_ticks, las = 1)        # numbers 0, 2, 4 … on the y-axis
  
  # Daily ticks on the x-axis: from the first to the last day in the data
  # (trunc(..., "days") cuts a time down to midnight)
  first_day <- as.POSIXct(trunc(min(data$time), "days"))
  last_day  <- as.POSIXct(trunc(max(data$time), "days"))
  days <- seq(first_day, last_day, by = "day")
  axis.POSIXct(1, at = days, format = "%d/%m")
  abline(v = days, h = y_ticks, col = "grey90")
  
  # Shade the storm period
  rect(shade_from, -1, shade_to, 100, col = "#00000012", border = NA)
  text(shade_from, max(data$wind_speed_past1h, na.rm = TRUE) + 1.5,
       shade_label, pos = 4, cex = 0.9, col = "grey30")
  
  # Dotted line for the normal October wind speed
  abline(h = norm, lty = 3, lwd = 2, col = "grey20")
  
  # Optional: dotted line for the storm limit
  if (storm_line) abline(h = storm_limit, lty = 3, lwd = 2, col = "#bd0026")
  
  # One line + arrows per station
  for (name in names(station_list)) {
    s <- data[data$station == name, ]
    lines(s$time, s$wind_speed_past1h, col = station_colours[name], lwd = 2)
    
    if (show_arrows) {
      every_6h <- s[as.numeric(format(s$time, "%H")) %% 6 == 0, ]
      draw_wind_arrows(every_6h$time, every_6h$wind_speed_past1h,
                       every_6h$wind_dir_past1h, col = station_colours[name])
    }
  }
  
  # Optional: "we are here" line for the gif
  if (!is.null(now)) abline(v = now, lwd = 2, col = "black")
  
  # Legend: one solid line per station + dotted norm (+ dotted storm limit)
  legend_text   <- c(names(station_list), paste0(norm_text, ": ", norm, " m/s"))
  legend_colour <- c(station_colours, "grey20")
  legend_type   <- c(rep(1, length(station_list)), 3)
  
  if (storm_line) {
    legend_text   <- c(legend_text, storm_label)
    legend_colour <- c(legend_colour, "#bd0026")
    legend_type   <- c(legend_type, 3)
  }
  
  legend("topleft", legend = legend_text, col = legend_colour,
         lty = legend_type, lwd = 2, bty = "n")
}

# "..." passes any extra options (norm, norm_text, storm_line, show_arrows,
# shade_from, ...) straight on to draw_wind_graph()
plot_wind <- function(data, station_list, station_colours, main_title,
                      file, subtitle = plot_subtitle, ...) {
  
  png(file, width = 1400, height = 850, res = 130)
  par(mar = c(6, 5, 5, 2))   # room for title + subtitle and source
  
  draw_wind_graph(data, station_list, station_colours, ...)
  
  title(main = main_title, line = 3, cex.main = 1.3)
  mtext(subtitle, side = 3, line = 1.5, cex = 0.9, col = "grey30")
  mtext(source_text, side = 1, line = 4.5, adj = 1, cex = 0.75, col = "grey40")
  
  invisible(dev.off())
}

plot_wind(all_data, stations, colours,
          main_title = plot_title,
          file       = "storm_okt_2023.png",
          norm       = norm_speed,
          norm_text  = norm_label)


# ---- 6. Wind direction: before, during and after the storm ----

direction_names <- c("N", "NØ", "Ø", "SØ", "S", "SV", "V", "NV")

# Degrees -> compass point. Each point covers 45°, e.g. NØ = 22.5°–67.5°
to_compass <- function(degrees) {
  points  <- c(direction_names, "N")          # 360° is north again
  compass <- points[round(degrees / 45) + 1]
  compass[which(degrees == 0)] <- "Stille"    # DMI uses 0 for calm
  factor(compass, levels = c(direction_names, "Stille"))   # fixed order
}

# Label every hour as before / during / after the storm
add_period <- function(data) {
  data$period <- "Før stormen"
  data$period[data$time >= storm_start] <- "Under stormen"
  data$period[data$time >= storm_end]   <- "Efter stormen"
  data$period <- factor(data$period,
                        levels = c("Før stormen", "Under stormen", "Efter stormen"))
  data
}

# Percentage of hours from each direction, one row per period + whole week.
# Each row adds up to 100 % (give or take rounding).
direction_table <- function(data, name) {
  s <- data[data$station == name, ]
  
  per_period <- prop.table(table(s$period, s$compass), margin = 1) * 100
  whole_week <- prop.table(table(s$compass)) * 100
  
  result <- round(rbind(per_period, "Hele ugen" = whole_week))
  
  # Drop the "Stille" column if there were no calm hours
  if (sum(result[, "Stille"]) == 0) result <- result[, direction_names]
  result
}

# Bar chart: one panel per station, bars for before / during / after
plot_directions <- function(data, station_list, station_colours, file) {
  png(file, width = 1400, height = 700, res = 130)
  par(mfrow = c(1, length(station_list)),   # panels side by side
      mar = c(5, 5, 3, 1), oma = c(2, 0, 3, 0))
  
  for (name in names(station_list)) {
    pct <- direction_table(data, name)
    pct <- pct[c("Før stormen", "Under stormen", "Efter stormen"), ]  # not "Hele ugen"
    bar_colours <- c("grey80", station_colours[name], "grey45")
    
    barplot(pct, beside = TRUE, col = bar_colours, border = NA,
            ylim = c(0, 100), las = 1,
            ylab = "Andel af timerne (%)", xlab = "Vinden kom fra",
            main = name)
    legend("topright", legend = rownames(pct), fill = bar_colours,
           border = NA, bty = "n")
  }
  
  mtext("Hvor kom vinden fra før, under og efter stormen?",
        side = 3, outer = TRUE, line = 1, cex = 1.2, font = 2)
  mtext(source_text, side = 1, outer = TRUE, line = 0.5, adj = 1,
        cex = 0.75, col = "grey40")
  
  invisible(dev.off())
}

all_data$compass <- to_compass(all_data$wind_dir_past1h)
all_data <- add_period(all_data)

for (name in names(stations)) {
  cat("\n", name, "– andel af timerne (%) hvor vinden kom fra hver retning\n")
  print(direction_table(all_data, name))
}

plot_directions(all_data, stations, colours, "vindretning_aarhus_anholt.png")

# Strongest hour for each station during the storm
storm <- all_data[all_data$period == "Under stormen", ]
for (name in names(stations)) {
  s <- storm[storm$station == name, ]
  top <- s[which.max(s$wind_speed_past1h), ]
  cat(name, ": max", top$wind_speed_past1h, "m/s at",
      format(top$time, "%d/%m %H:%M"), "from", as.character(top$compass), "\n")
}


# ---- 7. Where did the storm hit hardest? ---------------------
# Fetch wind speed for ALL Danish stations during the storm in one call
# (no stationId, but a bounding box around Denmark), and rank them by
# their highest hourly mean wind speed.

storm_from <- "2023-10-19T22:00:00Z"   # = 20/10 00:00 Danish time
storm_to   <- "2023-10-21T21:59:59Z"   # = 21/10 23:59 Danish time
dk_box     <- "7,54,16,58"             # lon/lat: south-west corner, north-east corner

url <- paste0(base_url,
              "?parameterId=wind_speed_past1h",
              "&datetime=", storm_from, "/", storm_to,
              "&bbox=", dk_box,
              "&limit=10000")
storm_all <- fromJSON(url)$features$properties

# Highest hourly mean per station
max_wind <- aggregate(value ~ stationId, data = storm_all, FUN = max)
names(max_wind)[2] <- "max_wind"

# Station names (the station list has one row per "version" of a station,
# so we ask for the versions valid during the storm and keep one per id)
info_url <- paste0(station_url,
                   "?bbox=", dk_box,
                   "&datetime=2023-10-20T00:00:00Z",
                   "&limit=10000")
station_info <- fromJSON(info_url)$features$properties[, c("stationId", "name")]
station_info <- station_info[!duplicated(station_info$stationId), ]

ranking <- merge(max_wind, station_info, by = "stationId")
ranking <- ranking[order(ranking$max_wind, decreasing = TRUE), ]
head(ranking, 10)

# Bar chart of the 10 hardest-hit stations (highest at the top)
top10 <- head(ranking, 10)

png("hardest_ramt_top10.png", width = 1400, height = 850, res = 130)
par(mar = c(6, 12, 5, 2))   # wide left margin for the station names
barplot(rev(top10$max_wind), names.arg = rev(top10$name),
        horiz = TRUE, las = 1, col = "#7b3294", border = NA,
        xlim = c(0, ceiling(max(top10$max_wind)) + 2),
        xlab = "Højeste timemiddel af vindhastighed (m/s)")
abline(v = national_norm, lty = 3, lwd = 2, col = "grey20")
title(main = "Hvor ramte stormen hårdest? 20.–21. oktober 2023",
      line = 3, cex.main = 1.3)
mtext("De 10 stationer med den højeste timemiddelvind. Prikket linje = normal oktober (6 m/s)",
      side = 3, line = 1.5, cex = 0.9, col = "grey30")
mtext(source_text, side = 1, line = 4.5, adj = 1, cex = 0.75, col = "grey40")
invisible(dev.off())


# ---- 7b. Same graphs for the two hardest-hit stations --------
# By default: the top 2 from the ranking. Or pick them yourself, e.g.
# hard_stations <- c("Kegnæs Fyr" = "06119", "Sønderborg Lufthavn" = "06118")

hard_stations <- setNames(ranking$stationId[1:2], ranking$name[1:2])
hard_colours  <- setNames(c("#7b3294", "#e08214"), names(hard_stations))

hard_data <- get_wind_data(hard_stations)

plot_wind(hard_data, hard_stations, hard_colours,
          main_title = paste("Hårdest ramt:",
                             paste(names(hard_stations), collapse = " og ")),
          file       = "storm_hardest_ramt.png",
          norm       = national_norm,
          norm_text  = "Normal oktober (landsgennemsnit)")

hard_data$compass <- to_compass(hard_data$wind_dir_past1h)
hard_data <- add_period(hard_data)

for (name in names(hard_stations)) {
  cat("\n", name, "– andel af timerne (%) hvor vinden kom fra hver retning\n")
  print(direction_table(hard_data, name))
}

plot_directions(hard_data, hard_stations, hard_colours, "vindretning_hardest_ramt.png")


# ---- 7c. Kegnæs Fyr vs. the average of three hard-hit stations ----
# Skagen Fyr is left out: it is windy in all weather, so it says little
# about where the storm was centred.

kegnaes    <- c("Kegnæs Fyr" = "06119")
group_ids  <- c("Gniben" = "06169", "Røsnæs Fyr" = "06159", "Drogden Fyr" = "06183")
group_name <- "Gns. Gniben, Røsnæs & Drogden"

focus_ids <- c(kegnaes, group_ids)   # also used to highlight these 4 on the gif (8f)

# Average wind DIRECTION can't be an ordinary mean: the mean of 350° and
# 10° would be 180° (south), although both are close to north.
# Instead: turn each direction into a point on a circle (sin, cos),
# average those, and turn the result back into degrees with atan2().
circular_mean <- function(degrees) {
  radians  <- degrees * pi / 180
  mean_dir <- atan2(mean(sin(radians)), mean(cos(radians))) * 180 / pi
  if (mean_dir <= 0) mean_dir <- mean_dir + 360   # 0–360°, north = 360 like DMI
  mean_dir
}

group_data <- get_wind_data(group_ids)

# Average per hour across the three stations
avg_speed <- aggregate(wind_speed_past1h ~ observed, data = group_data, FUN = mean)
not_calm  <- group_data[which(group_data$wind_dir_past1h > 0), ]   # 0 = calm, no direction
avg_dir   <- aggregate(wind_dir_past1h ~ observed, data = not_calm, FUN = circular_mean)

avg_data <- merge(avg_speed, avg_dir, by = "observed")
avg_data$station <- group_name
avg_data$time <- as.POSIXct(avg_data$observed,
                            format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
attr(avg_data$time, "tzone") <- "Europe/Copenhagen"

# Put Kegnæs and the average in one table with the same columns
kegnaes_data <- get_wind_data(kegnaes)

columns      <- c("observed", "wind_speed_past1h", "wind_dir_past1h", "station", "time")
compare_data <- rbind(kegnaes_data[, columns], avg_data[, columns])
compare_data <- compare_data[order(compare_data$station, compare_data$time), ]

# plot_wind() and plot_directions() only use the NAMES of this vector
compare_stations <- setNames(c("06119", "gns"), c("Kegnæs Fyr", group_name))
compare_colours  <- setNames(c("#7b3294", "#1b9e77"), names(compare_stations))

plot_wind(compare_data, compare_stations, compare_colours,
          main_title = "Kegnæs Fyr sammenlignet med tre andre hårdt ramte stationer",
          file       = "storm_kegnaes_vs_gns.png",
          norm       = national_norm,
          norm_text  = "Normal oktober (landsgennemsnit)",
          storm_line = TRUE)

compare_data$compass <- to_compass(compare_data$wind_dir_past1h)
compare_data <- add_period(compare_data)

for (name in names(compare_stations)) {
  cat("\n", name, "– andel af timerne (%) hvor vinden kom fra hver retning\n")
  print(direction_table(compare_data, name))
}

plot_directions(compare_data, compare_stations, compare_colours,
                "vindretning_kegnaes_vs_gns.png")


# ============================================================
# ---- 8. Storm maps: wind speed and direction on a map --------
# ============================================================
# Needs one extra package for the outline of Denmark:
#   install.packages("maps")        (only once)

library(maps)


# ---- 8a. Fetch speed + direction + position for ALL stations ----
# Same idea as section 7 (bbox instead of stationId), but now we also
# keep each station's position, which the API sends along with every
# observation in geometry$coordinates = c(longitude, latitude).

get_all_stations <- function(parameter, from, to) {
  url <- paste0(base_url,
                "?parameterId=", parameter,
                "&datetime=",    from, "/", to,
                "&bbox=",        dk_box,
                "&limit=20000")
  result <- fromJSON(url)
  
  coords <- result$features$geometry$coordinates
  if (is.list(coords)) coords <- do.call(rbind, coords)   # list of pairs -> 2-column matrix
  
  df <- data.frame(stationId = result$features$properties$stationId,
                   observed  = result$features$properties$observed,
                   lon       = coords[, 1],
                   lat       = coords[, 2],
                   value     = result$features$properties$value)
  names(df)[5] <- parameter
  df
}

map_speed <- get_all_stations("wind_speed_past1h", storm_from, storm_to)
map_dir   <- get_all_stations("wind_dir_past1h",   storm_from, storm_to)

# Join speed and direction: same station + same hour
map_data <- merge(map_speed,
                  map_dir[, c("stationId", "observed", "wind_dir_past1h")],
                  by = c("stationId", "observed"))

map_data$time <- as.POSIXct(map_data$observed,
                            format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
attr(map_data$time, "tzone") <- "Europe/Copenhagen"

# Add station names (station_info comes from section 7)
map_data <- merge(map_data, station_info, by = "stationId")

head(map_data)


# ---- 8b. Colours: DMI's wind categories (Beaufort scale) ----
# right = FALSE in cut() means each group includes its lower limit,
# e.g. exactly 24.5 m/s counts as "Storm".

wind_breaks  <- c(0, 8.0, 10.8, 13.9, 17.2, 20.8, 24.5, Inf)
wind_labels  <- c("Under 8 m/s (op til jævn vind)",
                  "8,0–10,7  Frisk vind",
                  "10,8–13,8  Hård vind",
                  "13,9–17,1  Stiv kuling",
                  "17,2–20,7  Hård kuling",
                  "20,8–24,4  Stormende kuling",
                  "24,5+  Storm")
# Lowest group is grey so ordinary wind fades into the background
wind_colours <- c("#d9d9d9", "#fed976", "#feb24c", "#fd8d3c",
                  "#f03b20", "#bd0026", "#67000d")

speed_colour <- function(speed) {
  wind_colours[cut(speed, breaks = wind_breaks, right = FALSE, labels = FALSE)]
}


# ---- 8c. Helpers that draw one map ---------------------------

map_xlim <- c(7.9, 15.3)    # longitude: from Vestjylland to Bornholm
map_ylim <- c(54.5, 57.85)  # latitude:  from Gedser to Skagen

# Sea + neighbouring countries in light grey, Denmark a bit darker
draw_dk_map <- function(mar = c(0.5, 0.5, 2, 0.5)) {
  map("world", xlim = map_xlim, ylim = map_ylim, fill = TRUE,
      col = "grey95", border = "grey75", bg = "#e3eef7", mar = mar)
  map("world", regions = "Denmark", fill = TRUE,
      col = "grey85", border = "grey50", add = TRUE)
}

# Arrows from each station in the direction the wind blows TO.
# Same maths as draw_wind_arrows() in section 4, but now the arrow
# LENGTH also grows with the wind speed.
draw_map_arrows <- function(lon, lat, speed, direction, inch_per_ms = 0.012,
                            col = "grey20", lwd = 1.3) {
  ok <- !is.na(speed) & !is.na(direction) & direction > 0   # skip calm/missing
  lon <- lon[ok]; lat <- lat[ok]; speed <- speed[ok]; direction <- direction[ok]
  
  x_in <- grconvertX(lon, "user", "inches")
  y_in <- grconvertY(lat, "user", "inches")
  
  arrow_length <- speed * inch_per_ms       # e.g. 25 m/s -> 0.3 inch
  radians <- direction * pi / 180
  x_end <- x_in - arrow_length * sin(radians)
  y_end <- y_in - arrow_length * cos(radians)
  
  arrows(lon, lat,
         grconvertX(x_end, "inches", "user"),
         grconvertY(y_end, "inches", "user"),
         length = 0.04, lwd = lwd, col = col)
}

# One complete map: background, arrows, then coloured dots on top.
# focus = a vector of station ids to highlight (or NULL for no highlighting)
draw_storm_map <- function(snapshot, point_size = 1.4, inch_per_ms = 0.012,
                           mar = c(0.5, 0.5, 2, 0.5), focus = NULL) {
  draw_dk_map(mar = mar)
  
  if (is.null(focus)) {
    # All stations drawn the same way
    draw_map_arrows(snapshot$lon, snapshot$lat,
                    snapshot$wind_speed_past1h, snapshot$wind_dir_past1h,
                    inch_per_ms = inch_per_ms)
    points(snapshot$lon, snapshot$lat, pch = 21, cex = point_size,
           bg = speed_colour(snapshot$wind_speed_past1h), col = "grey20")
    
  } else {
    in_focus <- snapshot$stationId %in% focus
    other    <- snapshot[!in_focus, ]
    main     <- snapshot[in_focus, ]
    
    # Other stations: small, faded dots (40 % colour) and light grey arrows
    draw_map_arrows(other$lon, other$lat,
                    other$wind_speed_past1h, other$wind_dir_past1h,
                    inch_per_ms = inch_per_ms, col = "grey65", lwd = 1)
    points(other$lon, other$lat, pch = 21, cex = point_size * 0.7,
           bg = adjustcolor(speed_colour(other$wind_speed_past1h), alpha.f = 0.4),
           col = "grey60")
    
    # Focus stations: drawn last (on top), big dots, thick black ring, names
    draw_map_arrows(main$lon, main$lat,
                    main$wind_speed_past1h, main$wind_dir_past1h,
                    inch_per_ms = inch_per_ms, col = "black", lwd = 2)
    points(main$lon, main$lat, pch = 21, cex = point_size * 1.4,
           bg = speed_colour(main$wind_speed_past1h), col = "black", lwd = 2.5)
    text(main$lon, main$lat, main$name, pos = 4, offset = 0.9,
         cex = 0.8, font = 2)
  }
}

wind_legend <- function(position = "topright", text_size = 0.75) {
  legend(position, legend = wind_labels, pch = 21, pt.bg = wind_colours,
         col = "grey20", pt.cex = 1.5, cex = text_size,
         title = "Timemiddelvind", bg = "white", box.col = "grey70")
}


# ---- 8d. Map 1: the peak of the storm at every station -------
# Sort all rows by wind speed (highest first) and keep the FIRST row
# for each station = that station's strongest hour (and its direction).

peak <- map_data[order(map_data$wind_speed_past1h, decreasing = TRUE), ]
peak <- peak[!duplicated(peak$stationId), ]

n_labels <- 5                 # how many of the top stations get a name label
top_peak <- head(peak, n_labels)

# "Impact areas": record storm surges (water level above normal, metres).
# Not from the DMI API – from Wikipedia's article on Storm Babet.
surge <- data.frame(town  = c("Aabenraa", "Sønderborg", "Gedser"),
                    lon   = c(9.42, 9.79, 11.93),
                    lat   = c(55.04, 54.91, 54.57),
                    water = c(2.16, 2.10, 1.89))

png("stormkort_hoejdepunkt.png", width = 1400, height = 1100, res = 130)

draw_storm_map(peak, point_size = 1.8, inch_per_ms = 0.018,
               mar = c(2.5, 0.5, 4, 0.5))

# Names + values for the hardest-hit stations
text(top_peak$lon, top_peak$lat,
     paste0(top_peak$name, " ", round(top_peak$wind_speed_past1h, 1), " m/s"),
     pos = 4, offset = 0.8, cex = 0.75, font = 2)

# Storm surge towns as blue triangles
points(surge$lon, surge$lat, pch = 24, cex = 1.6, bg = "#2c7fb8", col = "white")
text(surge$lon, surge$lat, paste0(surge$town, " +", surge$water, " m"),
     pos = 1, offset = 0.7, cex = 0.7, font = 2, col = "#2c7fb8")

wind_legend("topright")
legend("bottomleft",
       legend = c("Vinden blæser hen (længere pil = mere vind)",
                  "Stormflod: vandstand over normal"),
       pch = c(NA, 24), lty = c(1, NA), col = c("grey20", "white"),
       pt.bg = c(NA, "#2c7fb8"), pt.cex = 1.4, cex = 0.75,
       bg = "white", box.col = "grey70")

mtext("Stormens højdepunkt: højeste timemiddelvind på hver station, 20.–21. oktober 2023",
      side = 3, line = 2, font = 2, cex = 1.15)
mtext("Pilen viser vindretningen i stationens kraftigste time",
      side = 3, line = 0.7, cex = 0.9, col = "grey30")
mtext(paste(source_text, "· Stormflod: Wikipedia (Storm Babet)"),
      side = 1, line = 1, adj = 1, cex = 0.7, col = "grey40")

invisible(dev.off())


# ---- 8e. Map 2: the storm's progression, every 6 hours -------
# 8 small maps (2 rows x 4) plus a shared legend underneath.
# layout() splits the picture into numbered boxes; each new plot
# fills the next box.

map_times <- seq(storm_start, by = "6 hours", length.out = 8)

png("stormkort_udvikling.png", width = 1800, height = 1150, res = 130)

layout(rbind(1:4, 5:8, rep(9, 4)), heights = c(1, 1, 0.25))
par(oma = c(1.5, 0, 3, 0))   # outer margins for the overall title and source

for (i in seq_along(map_times)) {
  snapshot <- map_data[map_data$time == map_times[i], ]   # all stations at that hour
  draw_storm_map(snapshot, point_size = 1.1, mar = c(0.3, 0.3, 1.6, 0.3))
  mtext(format(map_times[i], "%d/%m kl. %H:%M"), side = 3, line = 0.3,
        font = 2, cex = 0.85)
}

# Box 9: an empty plot that only holds the legend
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = wind_labels, pch = 21, pt.bg = wind_colours,
       col = "grey20", pt.cex = 1.6, cex = 0.85, ncol = 4, bty = "n",
       title = "Timemiddelvind · pilene viser hvor vinden blæser hen")

mtext("Stormens udvikling hver 6. time, 20.–21. oktober 2023",
      side = 3, outer = TRUE, line = 1, font = 2, cex = 1.2)
mtext(source_text, side = 1, outer = TRUE, line = 0.3, adj = 1,
      cex = 0.7, col = "grey40")

invisible(dev.off())


# ---- 8f. (Optional) Animation: one frame per hour ------------
# Set make_gif to TRUE and install the package once:
#   install.packages("gifski")

# No storm-surge triangles here – only wind. The 4 stations from 7c
# (focus_ids) are highlighted; all others are faded into the background.

make_gif <- TRUE

if (make_gif) {
  library(gifski)
  
  all_hours <- sort(unique(map_data$time))
  
  # Everything drawn inside save_gif({ ... }) becomes frames in the gif
  save_gif({
    for (i in seq_along(all_hours)) {
      snapshot <- map_data[map_data$time == all_hours[i], ]
      draw_storm_map(snapshot, point_size = 1.6, inch_per_ms = 0.016,
                     mar = c(0.5, 0.5, 2.5, 0.5), focus = focus_ids)
      mtext(paste("Stormen", format(all_hours[i], "%d/%m kl. %H:%M")),
            side = 3, line = 0.8, font = 2, cex = 1.2)
      wind_legend("topright")
      legend("bottomleft",
             legend = c("Stationer i fokus", "Øvrige stationer"),
             pch = 21, pt.cex = c(2.2, 1.1), pt.lwd = c(2.5, 1),
             col = c("black", "grey60"), pt.bg = "white",
             cex = 0.8, bg = "white", box.col = "grey70")
    }
  }, gif_file = "stormkort_animation.gif",
  width = 1000, height = 800, res = 110, delay = 0.3)
}


# ============================================================
# ---- 9. November 2022: graph + moving compass (gif) ----------
# ============================================================
# Same two stations as the October graph. The period matches the
# graph in the assignment (14–21 November 2022).

nov_from <- "2022-11-14T00:00:00Z"
nov_to   <- "2022-11-21T23:59:59Z"

nov_norm      <- 6.5    # DMI's national normal for November (1961–90)
nov_norm_text <- "Normal november (landsgennemsnit)"

nov_data <- get_wind_data(stations, from = nov_from, to = nov_to)
head(nov_data)


# ---- 9a. Find the windiest period automatically ----
# The hour with the highest wind (either station), and 12 hours either side.

peak_time  <- nov_data$time[which.max(nov_data$wind_speed_past1h)]
nov_shade_from <- peak_time - 12 * 3600    # times are in seconds
nov_shade_to   <- peak_time + 12 * 3600

cat("Highest hourly wind:", max(nov_data$wind_speed_past1h, na.rm = TRUE),
    "m/s at", format(peak_time, "%d/%m %H:%M"), "\n")


# ---- 9b. Still graph (no arrows) ----

plot_wind(nov_data, stations, colours,
          main_title  = "Blæsten i november 2022 ved Århus og Anholt",
          subtitle    = "Timemiddel af vindhastighed. Vindretningen vises i kompas-animationen",
          file        = "storm_nov_2022.png",
          norm        = nov_norm,
          norm_text   = nov_norm_text,
          show_arrows = FALSE,
          shade_from  = nov_shade_from,
          shade_to    = nov_shade_to,
          shade_label = "Kraftigste vind (±12 timer)")


# ---- 9c. The compass ----
# One compass per station. North is up, like a map.
#   - The DOT shows where the wind comes FROM.
#   - Its distance from the centre shows the wind speed
#     (centre = calm, outer ring = the highest speed in the period).
#   - The fading trail shows the previous hours, so you can see the
#     direction turning.

# Outer ring: the highest wind in the period, rounded up to the next 5 m/s
compass_max <- ceiling(max(nov_data$wind_speed_past1h, na.rm = TRUE) / 5) * 5

draw_compass <- function(data, name, colour, now, trail_hours = 12) {
  # The hours to show: from (now - trail_hours) up to now, for this station
  s <- data[data$station == name &
              data$time <= now &
              data$time >  now - trail_hours * 3600, ]
  s <- s[which(s$wind_dir_past1h > 0), ]   # drop calm / missing direction
  
  par(mar = c(2, 1, 2.5, 1))
  plot.new()
  plot.window(xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3), asp = 1)  # asp = 1 keeps it round
  
  # Speed rings every 5 m/s
  circle <- seq(0, 2 * pi, length.out = 200)
  for (speed in seq(5, compass_max, by = 5)) {
    r <- speed / compass_max
    lines(r * cos(circle), r * sin(circle), col = "grey85")
    text(0, r, paste(speed, "m/s"), cex = 0.6, col = "grey55", pos = 3, offset = 0.1)
  }
  lines(cos(circle), sin(circle), col = "grey40", lwd = 1.5)
  
  # The 8 compass points. A direction of d degrees sits at
  # x = sin(d), y = cos(d)  ->  0° = straight up (north), 90° = right (east)
  compass_rad <- seq(0, 315, by = 45) * pi / 180
  segments(0, 0, sin(compass_rad), cos(compass_rad), col = "grey90")
  text(1.15 * sin(compass_rad), 1.15 * cos(compass_rad),
       direction_names, font = 2, cex = 0.9)
  
  if (nrow(s) > 0) {
    radius <- pmin(s$wind_speed_past1h / compass_max, 1)
    angle  <- s$wind_dir_past1h * pi / 180
    x <- radius * sin(angle)
    y <- radius * cos(angle)
    
    # Trail: older hours more transparent
    hours_ago <- as.numeric(difftime(now, s$time, units = "hours"))
    strength  <- 1 - hours_ago / trail_hours          # 1 = now, ~0 = oldest
    lines(x, y, col = adjustcolor(colour, alpha.f = 0.3), lwd = 1.5)
    # adjustcolor() needs one colour per dot when each dot has its own
    # transparency, so we repeat the station colour once per dot with rep()
    trail_colours <- sapply(strength, function(s) {
      adjustcolor(colour, alpha.f = 0.1 + 0.6 * s)
    })
    points(x, y, pch = 16, cex = 1.1, col = trail_colours)
    
    # The current hour: a needle from the centre to a big dot
    newest <- nrow(s)                                 # data is sorted by time
    segments(0, 0, x[newest], y[newest], col = colour, lwd = 3)
    points(x[newest], y[newest], pch = 21, cex = 2.3, bg = colour, col = "white", lwd = 2)
    
    mtext(paste0(round(s$wind_speed_past1h[newest], 1), " m/s fra ",
                 as.character(to_compass(s$wind_dir_past1h[newest]))),
          side = 1, line = 0.5, font = 2, col = colour)
  }
  
  mtext(name, side = 3, line = 0.8, font = 2, cex = 1.1)
}


# ---- 9d. One frame = graph on top + one compass per station below ----

draw_nov_frame <- function(now) {
  layout(matrix(c(1, 1,
                  2, 3), nrow = 2, byrow = TRUE), heights = c(1, 1.3))
  par(oma = c(1.5, 0, 3, 0))
  
  par(mar = c(4, 5, 1, 2))
  draw_wind_graph(nov_data, stations, colours,
                  norm = nov_norm, norm_text = nov_norm_text,
                  show_arrows = FALSE,
                  shade_from = nov_shade_from, shade_to = nov_shade_to,
                  shade_label = "Kraftigste vind", now = now)
  
  for (name in names(stations)) {
    draw_compass(nov_data, name, colours[name], now)
  }
  
  mtext(paste("Hvor kom vinden fra? November 2022 –", format(now, "%d/%m kl. %H:%M")),
        side = 3, outer = TRUE, line = 1, font = 2, cex = 1.2)
  mtext("Prikken viser hvor vinden kommer fra; jo længere ude, jo mere vind. Halen viser de sidste 12 timer",
        side = 1, outer = TRUE, line = 0.3, adj = 0, cex = 0.7, col = "grey30")
  mtext(source_text, side = 1, outer = TRUE, line = 0.3, adj = 1,
        cex = 0.7, col = "grey40")
}

# A still picture of the windiest hour – for the report
png("kompas_nov_2022_peak.png", width = 1200, height = 1000, res = 120)
draw_nov_frame(peak_time)
invisible(dev.off())


# ---- 9e. The gif: one frame every 2 hours ----
# install.packages("gifski")   (only once)

library(gifski)

gif_step    <- 2                                   # hours between frames
all_times   <- sort(unique(nov_data$time))
frame_times <- all_times[seq(1, length(all_times), by = gif_step)]

save_gif({
  for (i in seq_along(frame_times)) {
    draw_nov_frame(frame_times[i])
  }
}, gif_file = "kompas_nov_2022.gif",
width = 1200, height = 1000, res = 120, delay = 0.15)
