# ============================================================
# Opgave 5.2 – Wind at Århus Syd and Anholt Havn
# Storm in October 2023 (storm peak 20–21 Oct)
# Data: DMI metObs API (no API key needed since Dec 2025)
# ============================================================

library(jsonlite)   # only used to read the JSON from the API

# ---- 1. Settings ------------------------------

base_url <- "https://opendataapi.dmi.dk/v2/metObs/collections/observation/items"

stations <- c("Århus Lufthavn" = "06070", "Anholt Havn" = "06079")

start_time <- "2023-10-16T00:00:00Z"
end_time   <- "2023-10-22T23:59:59Z"

# Titles
plot_title    <- "Stormen rammer både Århus og Anholt hårdt"
plot_subtitle <- "Timemiddel af vindhastighed. Pilene viser hvor vinden blæser hen (hver 6. time)"
y_label       <- "Vindhastighed (m/s)"
x_label       <- "Dato (dansk tid)"
source_text <- "Kilde: DMI Frie Data, metObs API (opendataapi.dmi.dk)"

colours <- c("Århus Lufthavn" = "#1f5fbf", "Anholt Havn" = "#d1495b")

# "Normal" October wind speed, drawn as a dotted line.
# 6.0 m/s = DMI's national normal for October (reference period 1961–90).
# Set calculate_norm to TRUE to instead calculate it from the two stations'
# own October data 2013–2022 (section 3b) – takes 20 API calls.

norm_speed     <- 6.0
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


# ---- 3. Get speed + direction for each station ---------------

all_data <- data.frame()

for (name in names(stations)) {
  speed     <- get_obs(stations[name], "wind_speed_past1h")
  direction <- get_obs(stations[name], "wind_dir_past1h")
  
  one <- merge(speed, direction, by = "observed")   # match on time
  one$station <- name
  all_data <- rbind(all_data, one)
}

# Text -> date-time (API is in UTC), then show it in Danish time
all_data$time <- as.POSIXct(all_data$observed,
                            format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
attr(all_data$time, "tzone") <- "Europe/Copenhagen"

all_data <- all_data[order(all_data$station, all_data$time), ]

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


# ---- 5. The plot --------------------------------------------

png("storm_okt_2023.png", width = 1400, height = 850, res = 130)

par(mar = c(6, 5, 5, 2))   # room for title + subtitle

y_max   <- ceiling(max(all_data$wind_speed_past1h, na.rm = TRUE)) + 2
y_ticks <- seq(0, y_max, by = 2)

plot(all_data$time, all_data$wind_speed_past1h, type = "n",
     ylim = c(0, y_max),
     xaxt = "n", yaxt = "n",
     xlab = x_label, ylab = y_label)

axis(2, at = y_ticks, las = 1)        # numbers 0, 2, 4 … on the y-axis

# Daily ticks on the x-axis
days <- seq(as.POSIXct("2023-10-16", tz = "Europe/Copenhagen"),
            by = "day", length.out = 8)
axis.POSIXct(1, at = days, format = "%d/%m")
abline(v = days, h = y_ticks, col = "grey90")

# Shade the storm (20–21 Oct)
storm_start <- as.POSIXct("2023-10-20 00:00", tz = "Europe/Copenhagen")
storm_end   <- as.POSIXct("2023-10-22 00:00", tz = "Europe/Copenhagen")
rect(storm_start, -1, storm_end, 100, col = "#00000012", border = NA)
text(storm_start, max(all_data$wind_speed_past1h, na.rm = TRUE) + 1.5,
     "Storm", pos = 4, cex = 0.9, col = "grey30")

# Dotted line for the normal October wind speed
abline(h = norm_speed, lty = 3, lwd = 2, col = "grey20")

# One line + arrows per station
for (name in names(stations)) {
  s <- all_data[all_data$station == name, ]
  lines(s$time, s$wind_speed_past1h, col = colours[name], lwd = 2)
  
  every_6h <- s[as.numeric(format(s$time, "%H")) %% 6 == 0, ]
  draw_wind_arrows(every_6h$time, every_6h$wind_speed_past1h,
                   every_6h$wind_dir_past1h, col = colours[name])
}

legend("topleft",
       legend = c(names(stations), paste0(norm_label, ": ", norm_speed, " m/s")),
       col    = c(colours, "grey20"),
       lty    = c(1, 1, 3),   # two solid station lines + one dotted norm line
       lwd    = 2, bty = "n")

title(main = plot_title, line = 3, cex.main = 1.3)
mtext(plot_subtitle, side = 3, line = 1.5, cex = 0.9, col = "grey30")
mtext(source_text, side = 1, line = 4.5, adj = 1, cex = 0.75, col = "grey40")

# Note: the arrows are drawn for the current window size.
# If you resize the plot window, run section 5 again.
# To save a fixed-size version, wrap section 5 in:
#   png("storm_okt_2023.png", width = 1200, height = 700, res = 120)
#   ... section 5 ...
#   dev.off()

dev.off()

# ---- 6. Which direction did the wind come from? -------------

to_compass <- function(degrees) {
  points <- c("N", "NØ", "Ø", "SØ", "S", "SV", "V", "NV", "N")
  points[round(degrees / 45) + 1]
}

storm <- all_data[all_data$time >= storm_start & all_data$time < storm_end, ]
storm$compass <- to_compass(storm$wind_dir_past1h)

# Most common wind direction during the storm, per station
table(storm$station, storm$compass)

# Strongest hour for each station
for (name in names(stations)) {
  s <- storm[storm$station == name, ]
  top <- s[which.max(s$wind_speed_past1h), ]
  cat(name, ": max", top$wind_speed_past1h, "m/s at",
      format(top$time, "%d/%m %H:%M"), "from", top$compass, "\n")
}

