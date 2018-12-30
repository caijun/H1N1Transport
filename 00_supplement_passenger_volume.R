rm(list = ls())

source("src/R/helper_string.R")

library(foreign)
library(tidyverse)

infile <- strwrap("output/transport/
                  confirmed_pdm_pref_arrival_peak_day_airport_railway_station.dbf")
dat <- read.dbf(infile, as.is = TRUE)
# 输出表格整理数据，尽可能补充各个地级市交通运输客运量数据
pref.pv <- dat %>%
  dplyr::select(GBPref, GBProv, PrefCH, ProvCH, FullName, PTotal, PAviation,
                PRailway, PRoad, PBoat)
library(xlsx)
outfile <- path.expand(strwrap("output/transport/
                               pref_passenger_volume_2009.xlsx"))
write.xlsx(pref.pv, file = outfile)