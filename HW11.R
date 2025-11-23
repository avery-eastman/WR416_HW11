library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggpubr)

mich <- read_csv("michigan.csv")
mill <- read_csv("mill.csv")

mich$Date <- mdy(mich$Date)
mill$Date <- mdy(mill$Date)

mich <- mich |>
  mutate(discharge_ls = Discharge_cfs * 28.3168,
         site = "Michigan Creek")

mill <- mill |>
  mutate(discharge_ls = Discharge_ls,
         site = "Mill Creek")

hydro_df <- bind_rows(
  mich %>% select(Date, discharge_ls, site),
  mill %>% select(Date, discharge_ls, site)
)

p <- ggplot(hydro_df, aes(x = Date, y = discharge_ls, color = site)) +
  geom_point(size = 1.2, alpha = 0.6) +
  geom_line(linewidth = 0.5) + 
  labs(
    x = "Date",
    y = "Discharge (L/s)",
    color = "Site"
  ) +
  theme(legend.position = "bottom")
ggplotly(p)

mich <- mich |>
  mutate(rain = ifelse(Snow_depth_cm == 0, Precip_mm, 0))

mill <- mill |>
  mutate(rain = ifelse(Snow_depth_cm == 0, Precip_mm, 0))

mich$Date <- ymd(mich$Date)
mill$Date <- ymd(mill$Date)

xmin <- ymd("2016-10-01")
xmax <- ymd("2018-10-01")

#Plot snow
mich_snow <- ggplot(mich, aes(x = Date, y = Snow_depth_cm)) +
  geom_line() +
  ggtitle('a. Snow Depth') +
  xlab('Date') +
  ylab('Snow Depth (cm)') +
  coord_cartesian(xlim = c(xmin, xmax))
mich_snow
ggplotly(mich_snow)

mill_snow <- ggplot(mill, aes(x = Date, y = Snow_depth_cm)) +
  geom_line() +
  ggtitle('a. Snow Depth') +
  xlab('Date') +
  ylab('Snow Depth (cm)') +
  coord_cartesian(xlim = c(xmin, xmax))
mill_snow
ggplotly(mill_snow)

#Plot rain
mich_rain <- ggplot(mich, aes(x = Date, y = rain)) +
  geom_line() +
  ggtitle('b. Rain') +
  xlab('Date') +
  ylab('Rain (mm)') +
  coord_cartesian(xlim = c(xmin, xmax))
mich_rain
ggplotly(mich_rain)

mill_rain <- ggplot(mill, aes(x = Date, y = rain)) +
  geom_line() +
  ggtitle('b. Rain') +
  xlab('Date') +
  ylab('Rain (mm)') +
  coord_cartesian(xlim = c(xmin, xmax))
mill_rain
ggplotly(mill_rain)

#combine soil VWC in one table and pivot
vwc_mich_long <- mich |>
  pivot_longer(cols = starts_with("VWC"),
               names_to = "Depth",
               values_to = "VWC")

vwc_mill_long <- mill |>
  pivot_longer(cols = starts_with("VWC"),
               names_to = "Depth",
               values_to = "VWC")

#Plot soil moisture
mich_soil <- ggplot(vwc_mich_long, aes(x = Date, y = VWC, color = Depth)) +
  geom_line() +
  ggtitle('c. Volumetric Water Content') +
  xlab('Date') +
  ylab('VWC (cm)') +
  coord_cartesian(xlim = c(xmin, xmax))
mich_soil
ggplotly(mich_soil)

mill_soil <- ggplot(vwc_mill_long, aes(x = Date, y = VWC, color = Depth)) +
  geom_line() +
  ggtitle('c. Volumetric Water Content') +
  xlab('Date') +
  ylab('VWC (cm)') +
  coord_cartesian(xlim = c(xmin, xmax))
mill_soil
ggplotly(mill_soil)

#Plot discharge
mich_discharge <- ggplot(mich, aes(x = Date, y = discharge_ls)) +
  geom_line() +
  ggtitle('d. Stream Discharge') +
  xlab('Date') +
  ylab('Discharge (L/s)') +
  coord_cartesian(xlim = c(xmin, xmax))
mich_discharge
ggplotly(mich_discharge)

mill_discharge <- ggplot(mill, aes(x = Date, y = discharge_ls)) +
  geom_line() +
  ggtitle('d. Stream Discharge') +
  xlab('Date') +
  ylab('Discharge (L/s)') +
  coord_cartesian(xlim = c(xmin, xmax))
mill_discharge
ggplotly(mill_discharge)

#Put plots together into multipanel figure
mich_TSplot <- ggarrange(mich_snow, mich_rain, mich_soil, mich_discharge, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
mich_TSplot

mill_TSplot <- ggarrange(mill_snow, mill_rain, mill_soil, mill_discharge, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
mill_TSplot

#Cause I'm curious, plot temperature
E <- ggplot(mich, aes(x = Date, y = Temperature_C)) +
  geom_line() +
  ggtitle('Temperature') +
  xlab('Date') +
  ylab('Temp (C)') +
  coord_cartesian(xlim = c(xmin, xmax))
ggplotly(E)

R <- ggplot(mill, aes(x = Date, y = Temperature_C)) +
  geom_line() +
  ggtitle('Temperature') +
  xlab('Date') +
  ylab('Temp (C)') +
  coord_cartesian(xlim = c(xmin, xmax))
ggplotly(R)

#Area-normalized discharge
area_mich_km2 <- 4.03
area_mill_km2 <- 2.53


mich <- mich |>
  mutate(mm_day = (discharge_ls * 0.0864) / area_mich_km2)
mill <- mill |>
  mutate(mm_day = (discharge_ls * 0.0864) / area_mill_km2)

mich <- mich |>
  mutate(water_year = year(Date + months(3)))
mill <- mill |>
  mutate(water_year = year(Date + months(3)))

WY_mich <- mich |>
  group_by(water_year) |>
  summarise(Q_mm = sum(mm_day, na.rm = TRUE),
            P_mm = sum(Precip_mm, na.rm = TRUE))
WY_mill <- mill |>
  group_by(water_year) |>
  summarise(Q_mm = sum(mm_day, na.rm = TRUE),
            P_mm = sum(Precip_mm, na.rm = TRUE))


