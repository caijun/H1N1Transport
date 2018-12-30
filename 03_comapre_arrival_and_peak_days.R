rm(list = ls())

source("R/helper_string.R")
source("R/helper_statistic.R")
source("R/theme_publication.R")

library(foreign)
# final data for analysis
# 尽可能补充地级市交通运输客运量后的数据集
infile <- strwrap("data/confirmed_pdm_pref_arrival_peak_day_airport_railway_station_update_pv.dbf")
dat <- read.dbf(infile, as.is = TRUE)

library(tidyverse)
# exclude 台湾省、香港特别行政区、澳门特别行政区 and 青海藏族自治州（未被感染）
dat <- dat %>% 
  dplyr::filter(!GBPref %in% c("7100", "8100", "8200", "6327"))

# five numbers summary for all 340 prefectures
fivenum(dat$idx_arr)
fivenum(dat$idx_pk)

# no significant association between arrival days and peak days
cor.test(dat$idx_arr, dat$idx_pk, method = "pearson")

# five numbers summary by airports
x <- dat %>% 
  group_by(airport) %>% 
  dplyr::summarise(min = min(idx_arr), 
                   P25 = quantile(idx_arr, 1/4), 
                   P50 = quantile(idx_arr, 2/4), 
                   P75 = quantile(idx_arr, 3/4), 
                   max = max(idx_arr), 
                   n = n(), 
                   IQR = IQR(idx_arr))

x <- dat %>% 
  group_by(airport) %>% 
  dplyr::summarise(min = min(idx_pk), 
                   P25 = quantile(idx_pk, 1/4), 
                   P50 = quantile(idx_pk, 2/4), 
                   P75 = quantile(idx_pk, 3/4), 
                   max = max(idx_pk), 
                   n = n(), 
                   IQR = IQR(idx_pk))

# five numbers summary by railway stations
x <- dat %>% 
  group_by(station) %>% 
  dplyr::summarise(min = min(idx_arr), 
                   P25 = quantile(idx_arr, 1/4), 
                   P50 = quantile(idx_arr, 2/4), 
                   P75 = quantile(idx_arr, 3/4), 
                   max = max(idx_arr), 
                   n = n(), 
                   IQR = IQR(idx_arr))

x <- dat %>% 
  group_by(station) %>% 
  dplyr::summarise(min = min(idx_pk), 
                   P25 = quantile(idx_pk, 1/4), 
                   P50 = quantile(idx_pk, 2/4), 
                   P75 = quantile(idx_pk, 3/4), 
                   max = max(idx_pk), 
                   n = n(), 
                   IQR = IQR(idx_pk))

# Mann-Whitney U test at 95% confidence level is used to determine whether 
# significant differences in arrival days (or peak days) exist between 
# prefectures grouped by presence of airports (or railway stations)
PAY <- dat %>% 
  dplyr::filter(airport == 1) %>% 
  dplyr::select(idx_arr, idx_pk)

PAN <- dat %>% 
  dplyr::filter(airport == 0) %>% 
  dplyr::select(idx_arr, idx_pk)

wilcox.test(PAY$idx_arr, PAN$idx_arr, alternative = "less", conf.int = TRUE)
wilcox.test(PAY$idx_pk, PAN$idx_pk, conf.int = TRUE)

PSY <- dat %>% 
  dplyr::filter(station == 1) %>% 
  dplyr::select(idx_arr, idx_pk)

PSN <- dat %>% 
  dplyr::filter(station == 0) %>% 
  dplyr::select(idx_arr, idx_pk)

wilcox.test(PSY$idx_arr, PSN$idx_arr, alternative = "less", conf.int = TRUE)
wilcox.test(PSY$idx_pk, PSN$idx_pk, conf.int = TRUE)


# violin plot ------------------------------------------------------------------
vp.dat <- dat %>% 
  dplyr::select(airport, station, idx_arr, idx_pk) %>%
  gather(var, val, idx_arr:idx_pk) %>% 
  gather(travel, presence, airport:station) %>% 
  mutate(var = factor(var, levels = c("idx_arr", "idx_pk"))) %>% 
  mutate(presence = ifelse(presence == 0, "without", "with")) %>% 
  mutate(presence = factor(presence, levels = c("with", "without")))

var_names <- c(
  `idx_arr` = "Arrival days", 
  `idx_pk` = "Peak days"
)

travel_names <- c(
  `airport` = "Airport",
  `station` = "Railway station"
)

library(ggsci)
library(ggsignif)
library(scales)
mypal <- pal_npg("nrc", alpha = 0.8)(9)
show_col(mypal)

# annotate significance
(annotation_df <- data.frame(var = c("idx_arr", "idx_arr", "idx_pk", "idx_pk"), 
                             travel = c("airport", "station", "airport", "station"), 
                             presence = rep("with", 4), 
                             start = rep("with", 4), 
                             end = rep("without", 4),
                             y = c(250, 250, 275, 275),
                             label = c("*", "***", "NS.", "NS.")))

p <- ggplot(vp.dat, aes(x = presence, y = val, color = presence)) + 
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = 0.1) + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 1, 
               color = "darkred") +
  geom_signif(data = annotation_df,
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              textsize = 3, vjust = -0.2, manual = TRUE, color = "grey60") + 
  labs(x = "", y = "Days since May 10, 2009") + 
  scale_color_npg(guide = FALSE, alpha = 0.8) +
  # scale_fill_manual(values = mypal[1:2], guide = FALSE) + 
  scale_y_continuous(breaks = seq(0, 300, by = 50), limits = c(0, 300)) + 
  facet_grid(var ~ travel, labeller = labeller(var = as_labeller(var_names), 
                                               travel = as_labeller(travel_names))) + 
  theme_publication(base_size = 12) +
  theme(panel.grid.major = element_blank(), 
        strip.background = element_rect(colour = "gray95", fill = "gray95"))
print(p)

outfile <- "figs/arrival_peak_day_violin_plot.pdf"
pdf(file = outfile, width = 5, height = 5)
print(p)
dev.off()


# multivariate regression to assess the associations between different travel 
# mode for the arrival days of 340 prefectures in mainland China ---------------
# geographic coordinates of administrative center for each prefectures
latlng <- read.dbf("data/China_2010_Prefecture_Admin_Center_省直辖县级行政单位_aggregate.dbf")
latlng <- latlng %>%
  dplyr::select(GBPref, GeoName, lat, lng) %>%
  mutate(GBPref = as.character(GBPref))

# unit for PTotal, PRailway, PRoad, PBoat is # * 10^4 persons
# unit for PAviation is # persons
# 334 prefectures with available passenger volumes
arrival.dat <- dat %>% 
  left_join(latlng, by = "GBPref") %>%
  dplyr::select(GBPref, PrefCH, GBProv, ProvCH, lat, lng, 
                idx_arr, PTotal, PAviation, PRailway, PRoad, PBoat) %>% 
  dplyr::mutate(PAviation = PAviation / 10000) %>% 
  dplyr::filter(PTotal > 0)

p <- ggplot(arrival.dat, aes(PTotal, idx_arr)) + 
  geom_point() + 
  scale_x_log10()
print(p)
# R^2 = 0.3019
m <- lm(idx_arr ~ log(PTotal), data = arrival.dat)
summary(m)

# because PTotal is highly correlated with PRoad, omit PTotal in following 
# analysis
# correlations between variables
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

cor.var <- arrival.dat %>% 
  dplyr::select(lat:PBoat)
res <- cor.mtest(cor.var, 0.95)

library(corrplot)
M <- cor(cor.var)
outfile <- "figs/arrival_travel_corrplot.pdf"
pdf(file = outfile, width = 5, height = 5)
cex.old <- par("cex")
par(mfrow = c(1, 1), cex = 0.8)

corrplot(M, method = "number", type = "upper", tl.cex = 1/par("cex"),
         cl.cex = 1/par("cex"), p.mat = res[[1]], sig.level = 0.05)

par(cex = cex.old)
dev.off()


# 135 prefectures with air passenger volume ------------------------------------
arrival.dat1 <- arrival.dat %>% 
  dplyr::filter(PAviation > 0)

p <- ggplot(arrival.dat1, aes(PAviation, idx_arr)) + 
  geom_point() + 
  scale_x_log10()
print(p)
# R^2 = 0.3246
m <- lm(idx_arr ~ log(PAviation), data = arrival.dat1)
summary(m)

# 255 prefectures with rail passenger volume -----------------------------------
arrival.dat2 <- arrival.dat %>% 
  dplyr::filter(PRailway > 0)

p <- ggplot(arrival.dat2, aes(PRailway, idx_arr)) + 
  geom_point() + 
  scale_x_log10()
print(p)
# R^2 = 0.1434
m <- lm(idx_arr ~ log(PRailway), data = arrival.dat2)
summary(m)

# 334 prefectures with road passenger volume -----------------------------------
arrival.dat3 <- arrival.dat %>% 
  dplyr::filter(PRoad > 0)

# 中国城市统计年鉴2010显示拉萨市2009年公路客运量只有0.68万人，参考拉萨市2008年的
# 交通运输客运量统计数据，认为68.3万人更合理，据此对数据进行了订正。
p <- ggplot(arrival.dat3, aes(PRoad, idx_arr)) + 
  geom_point() + 
  scale_x_log10()
print(p)
# R^2 = 0.2804
m <- lm(idx_arr ~ log(PRoad), data = arrival.dat3)
summary(m)

# 剔除那曲地区(5424)
arrival.dat3 <- arrival.dat %>% 
  dplyr::filter(PRoad > 0 & GBPref != "5424")

p <- ggplot(arrival.dat3, aes(PRoad, idx_arr)) + 
  geom_point() + 
  geom_hline(yintercept = 90, color = "red", linetype = "dashed") + 
  scale_x_log10()
print(p)
# R^2 = 0.29
m <- lm(idx_arr ~ log(PRoad), data = arrival.dat3)
summary(m)

# 151 prefectures with boat passenger volume -----------------------------------
arrival.dat4 <- arrival.dat %>% 
  dplyr::filter(PBoat > 0)

p <- ggplot(arrival.dat4, aes(PBoat, idx_arr)) + 
  geom_point() + 
  scale_x_log10()
print(p)
# R^2 = 0.03221
# although the association between arrival days and boat passenger volume was 
# significant, boat passenger volume only explained < 0.04 of variance of 
# arrival days
m <- lm(idx_arr ~ log(PBoat), data = arrival.dat4)
summary(m)

# Venn Diagram of prefectures with different travel modes ----------------------
# 个别城市不同交通方式的客运量数据收集不全，只能是尽可能收集
# 若未收集到数据，仍用0填充，不一定表示该种交通方式客运量为0
# 注意这种情况只是极少数，不影响分析结论
# design matrix
tm <- arrival.dat %>% 
  dplyr::select(PAviation, PRailway, PRoad, PBoat) %>% 
  dplyr::mutate(PAviation = ifelse(PAviation > 0, 1, 0), 
                PRailway = ifelse(PRailway > 0, 1, 0), 
                PRoad = ifelse(PRoad > 0, 1, 0), 
                PBoat = ifelse(PBoat > 0, 1, 0)) %>% 
  dplyr::rename(Aviation = PAviation, Railway = PRailway, Road = PRoad, 
                Boat = PBoat)
library(limma)
(vc <- vennCounts(tm))
vennDiagram(vc)

# ignore travel by boat
tm <- tm %>% 
  dplyr::select(Aviation, Railway, Road)
(vc <- vennCounts(tm))
vennDiagram(vc)

# correlations between arrival days and different transport modes --------------
# 115 prefectures with air, rail, and road passenger volumes -------------------
arrival.dat5 <- arrival.dat %>% 
  dplyr::filter(PAviation > 0 & PRailway > 0 & PRoad > 0)
# save this data for following regression analysis
save(arrival.dat5, file = "output/115_prefectures_for_regression.rda")

cor.test(arrival.dat5$idx_arr, log(arrival.dat5$PAviation))
cor.test(arrival.dat5$idx_arr, log(arrival.dat5$PRailway))
cor.test(arrival.dat5$idx_arr, log(arrival.dat5$PRoad))

# 140 prefectures with rail and road passenger volumes -------------------------
arrival.dat6 <- arrival.dat %>% 
  dplyr::filter(PAviation == 0 & PRailway > 0 & PRoad > 0)

cor.test(arrival.dat6$idx_arr, log(arrival.dat6$PRailway))
cor.test(arrival.dat6$idx_arr, log(arrival.dat6$PRoad))

# 20 prefectures with air and road passenger volumes ---------------------------
arrival.dat7 <- arrival.dat %>% 
  dplyr::filter(PAviation > 0 & PRailway == 0 & PRoad > 0)

# insignificant relationship
cor.test(arrival.dat7$idx_arr, log(arrival.dat7$PAviation))
cor.test(arrival.dat7$idx_arr, log(arrival.dat7$PRoad))

# 59 prefectures with only road passenger volume -------------------------------
arrival.dat8 <- arrival.dat %>% 
  dplyr::filter(PAviation == 0 & PRailway == 0 & PRoad > 0)

cor.test(arrival.dat8$idx_arr, log(arrival.dat8$PRoad))
