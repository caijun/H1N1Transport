rm(list = ls())

# source("src/R/helper.R")
source("src/R/helper_string.R")
source("src/R/theme_publication.R")

library(tidyverse)
library(foreign)
pdm <- read.dbf('output/confirmed_pdm.dbf', as.is = TRUE)

# Fang.etal-Am.J.Epidemiol.-2012 drew epidemic curve based on onset date.
# We draw epidemic curve based on diagnosis date
# pandemic period: May 2009 - April 2010
library(incidence)
pdm.inc <- incidence(pdm$diagnose, first_date = as.Date("2009-05-01"), 
                     last_date = as.Date("2010-04-30"))
plot(pdm.inc)
pdm.dec <- as.data.frame(pdm.inc)
pdm.dec$counts[1:9] <- NA

# epidemic curve during May 2009 - August 2009
pdm.early.inc <- incidence(pdm$diagnose, first_date = as.Date("2009-05-01"), 
                           last_date = as.Date("2009-08-31"))
plot(pdm.early.inc)
pdm.early.dec <- as.data.frame(pdm.early.inc)
pdm.early.dec$counts[1:9] <- NA

pdm.death <- pdm %>% 
  dplyr::filter(death == 1)
pdm.death.inc <- incidence(pdm.death$diagnose, first_date = as.Date("2009-05-01"), 
                           last_date = as.Date("2010-04-30"))
pdm.death.dec <- as.data.frame(pdm.death.inc)
pdm.death.dec$counts[1:9] <- NA

p1 <- ggplot(pdm.dec, aes(dates, counts)) + 
  geom_line() + 
  scale_x_date(expand = c(0, 0), 
               limits = c(as.Date("2009-05-01"), as.Date("2010-05-01")), 
               breaks = "1 month", date_labels = "%Y-%b") + 
  labs(x = "", y = "Number of cases") + 
  theme_publication(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        plot.margin = unit(c(0.5, 0.5, 0, 0.5), "cm"), 
        panel.grid.major = element_blank())
print(p1)

p2 <- ggplot(pdm.death.dec, aes(dates, counts)) + 
  geom_line() + 
  scale_x_date(expand = c(0, 0), 
               limits = c(as.Date("2009-05-01"), as.Date("2010-05-01")), 
               breaks = "1 month", date_labels = "%Y-%b") + 
  scale_y_continuous(breaks = seq(0, 30, by = 5)) + 
  labs(x = "", y = "Number of deaths") + 
  theme_publication(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"), 
        panel.grid.major = element_blank())
print(p2)

p3 <- ggplot(pdm.early.dec, aes(dates, counts)) + 
  geom_line() + 
  scale_x_date(expand = c(0, 0), 
               limits = c(as.Date("2009-05-01"), as.Date("2009-09-05")), 
               breaks = "1 month", date_labels = "%Y-%b") + 
  labs(x = "", y = "Number of cases") + 
  theme_publication(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        plot.margin = unit(c(0.5, 0.5, 0, 0.5), "cm"), 
        panel.grid.major = element_blank())
print(p3)

p <- cowplot::plot_grid(p1, p2, nrow = 2, align = "v")

outfile <- "figs/confirmed/pdm/epidemic_curve.pdf"
pdf(file = outfile, width = 6, height = 6)
print(p)
dev.off()

p13 <- cowplot::ggdraw() + 
  cowplot::draw_plot(p1) + 
  cowplot::draw_plot(p3, x = 0.08, y = 0.6, width = 0.35, height = 0.35)

outfile <- "figs/confirmed/pdm/epidemic_curve_embed.pdf"
pdf(file = outfile, width = 10, height = 6)
print(p13)
dev.off()

p <- cowplot::plot_grid(p1, p3, nrow = 2, align = "v")

outfile <- "figs/confirmed/pdm/epidemic_curve1.pdf"
pdf(file = outfile, width = 6, height = 6)
print(p)
dev.off()

# illustration of arrival days and peak days using the epidemic curve of Beijing
pdm.beijing <- pdm %>% 
  dplyr::filter(GBPref1 == "1100")
pdm.beijing.inc <- incidence(pdm.beijing$diagnose, first_date = as.Date("2009-05-01"), 
                             last_date = as.Date("2010-04-30"))
pdm.beijing.dec <- as.data.frame(pdm.beijing.inc)
pdm.beijing.dec$counts[1:9] <- NA

idx.arr <- which(pdm.beijing.dec$counts > 0)[1]
arr.date <- pdm.beijing.dec$dates[idx.arr]
idx.pk <- which.max(pdm.beijing.dec$counts)
pk.date <- pdm.beijing.dec$dates[idx.pk]
pk.counts <- pdm.beijing.dec$counts[idx.pk]

p <- ggplot(pdm.beijing.dec, aes(dates, counts)) + 
  geom_line() + 
  geom_vline(xintercept = as.Date("2009-05-10"), linetype = "dashed", 
             color = "gray") + 
  geom_vline(xintercept = arr.date, linetype = "dashed", 
             color = "green") + 
  geom_vline(xintercept = pk.date, linetype = "dashed", 
             color = "red") + 
  geom_segment(aes(x = as.Date("2009-05-10"), y = 200, xend = arr.date, yend = 200), 
               color = "green", 
               arrow = arrow(angle = 10, ends = "both", type = "closed", 
                             length = unit(0.05, "inches"))) + 
  geom_segment(aes(x = as.Date("2009-05-13"), y = 200, 
                   xend = as.Date("2009-06-01"), yend = 190), 
               color = "green", 
               arrow = arrow(angle = 10, ends = "both", type = "closed", 
                             length = unit(0.05, "inches"))) + 
  annotate("text", x = as.Date("2009-06-22"), y = 190, label = "Arrival days", 
           color = "green") + 
  geom_segment(aes(x = as.Date("2009-05-10"), y = pk.counts, xend = pk.date, yend = pk.counts), 
               color = "red", 
               arrow = arrow(angle = 10, ends = "both", type = "closed", 
                             length = unit(0.05, "inches"))) + 
  annotate("text", x = as.Date("2009-08-01"), y = 230, label = "Peak days", 
           color = "red") + 
  scale_x_date(expand = c(0, 0), 
               limits = c(as.Date("2009-05-01"), as.Date("2010-05-01")), 
               breaks = "1 month", date_labels = "%Y-%b") + 
  labs(x = "", y = "Number of cases") + 
  theme_publication(base_size = 12) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        panel.grid.major = element_blank())
print(p)

outfile <- "figs/confirmed/pdm/arrival_peak_day_illustration.pdf"
pdf(file = outfile, width = 8, height = 6)
print(p)
dev.off()
