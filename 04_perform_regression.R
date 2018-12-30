rm(list = ls())

source("R/helper_string.R")
source("R/theme_publication.R")

load("output/115_prefectures_for_regression.rda")

# 115 prefectures with passenger volumes by air + train + road -----------------
Fn <- ecdf(arrival.dat5$idx_arr)
plot(Fn)
Fn(4)
Fn(38)
Fn(80)
Fn(125)

# OLS regression ---------------------------------------------------------------
# R^2 = 0.6019
m <- lm(idx_arr ~ lat + lng + log(PAviation) + log(PRailway) + log(PRoad), 
        data = arrival.dat5)
summary(m)
confint(m)
# plot(m)
library(car)
vif(m)
# extract coefficients for OLS
olscf <- summary(m)$coefficients
olscf <- cbind(olscf[, 1], confint(m))
colnames(olscf) <- c("coefficients", "lower.bd", "upper.bd")

# quantile regression
library(quantreg)
qr1 <- rq(idx_arr ~ lat + lng + log(PAviation) + log(PRailway) + log(PRoad), 
          data = arrival.dat5, tau = 0.5)
summary(qr1)

quantile(arrival.dat5$idx_arr, prob = seq(0, 1, by = 0.05))
(idx_arr.mean <- mean(arrival.dat5$idx_arr))
(qs1 <- quantile(arrival.dat5$idx_arr, prob = 0.25))
(qs2 <- quantile(arrival.dat5$idx_arr, prob = 0.5))
(qs3 <- quantile(arrival.dat5$idx_arr, prob = 0.75))

den <- density(arrival.dat5$idx_arr, from = min(arrival.dat5$idx_arr), 
               to = max(arrival.dat5$idx_arr))
den.df <- data.frame(x = den$x, y = den$y)

library(latex2exp)
library(ggsci)
library(scales)
mypal <- pal_npg("nrc", alpha = 0.7)(9)
show_col(mypal)

p1 <- ggplot(arrival.dat5, aes(x = idx_arr)) +   
  geom_histogram(aes(y = ..density..), breaks = seq(3, 173, by = 10), 
                 fill = "grey80", colour = "grey50") + 
  geom_line(data = den.df, aes(x, y), color = mypal[9]) + 
  geom_segment(aes(x = qs1, y = 0, xend = qs1, yend = 0.018), color = "red", 
               linetype = "dashed") + 
  geom_segment(aes(x = qs2, y = 0, xend = qs2, yend = 0.018), color = "green", 
               linetype = "dashed") + 
  geom_segment(aes(x = qs3, y = 0, xend = qs3, yend = 0.018), color = "blue", 
               linetype = "dashed") + 
  geom_vline(aes(xintercept = idx_arr.mean), color = "darkgreen", linetype = "dashed") + 
  geom_segment(aes(x = 0.1, y = 0.0182, xend = idx_arr.mean - 0.1, yend = 0.0182), 
               color = "gray", 
               arrow = arrow(angle = 10, ends = "both", type = "closed", 
                             length = unit(0.1, "inches"))) + 
  geom_segment(aes(x = idx_arr.mean + 0.1, y = 0.0182, xend = 175, yend = 0.0182), 
               color = "gray", 
               arrow = arrow(angle = 10, ends = "both", type = "closed", 
                             length = unit(0.1, "inches"))) + 
  annotate("text", x = qs1, y = 0.019, label = "Phase I", color = "black", fontface = 2) + 
  annotate("text", x = qs3, y = 0.019, label = "Phase II", color = "black", fontface = 2) + 
  annotate("text", x = qs1 + 14, y = 0.014, label = TeX("$\\tau = 0.25$", output = "character"), 
           color = "red", parse = TRUE) + 
  annotate("text", x = qs2 - 12, y = 0.012, label = TeX("$\\tau = 0.5$", output = "character"), 
           color = "green", parse = TRUE) + 
  annotate("text", x = qs3 + 14, y = 0.014, label = TeX("$\\tau = 0.75$", output = "character"), 
           color = "blue", parse = TRUE) + 
  annotate("text", x = idx_arr.mean + 12, y = 0.012, label = "mean", color = "darkgreen") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 175), breaks = seq(0, 180, by = 20)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.02)) + 
  labs(x = "Arrival days", y = "Density") + 
  theme_publication() + 
  theme(panel.grid.major = element_blank())
print(p1)

qs <- 1:3/4
qr2 <- rq(idx_arr ~ lat + lng + log(PAviation) + log(PRailway) + log(PRoad), 
          data = arrival.dat5, tau = qs)
# default alpha level is 0.1
# set alpha level to 0.05, 95% confidence interval
summary(qr2, alpha = 0.05)
coef(qr2)

# pseudo R squared
# https://stackoverflow.com/questions/19861194/extract-r2-from-quantile-regression-summary
fit0 <- rq(idx_arr ~ 1, 
           data = arrival.dat5, tau = 0.25)
fit1 <- rq(idx_arr ~ lat + lng + log(PAviation) + log(PRailway) + log(PRoad), 
           data = arrival.dat5, tau = 0.25)

rho <- function(u, tau = .5) {
  u * (tau - (u < 0))
}

V0 <- sum(rho(fit0$resid, fit0$tau))
V1 <- sum(rho(fit1$resid, fit1$tau))
(R1 <- 1 - V1/V0)
(R1 <- 1 - fit1$rho/fit0$rho)

# quantile process regression plots
qs <- 1:19/20
qr3 <- rq(idx_arr ~ lat + lng + log(PAviation) + log(PRailway) + log(PRoad), 
          data = arrival.dat5, tau = qs)
x <- summary(qr3, alpha = 0.05)

taus <- sapply(x, function(x) x$tau)
cf <- lapply(x, coef)

# quantile regression coefficients plot for log(PAviation)
parm <- "log(PAviation)"
cf1 <- lapply(cf, function(x) x[parm, , drop = FALSE])
cf.df <- do.call("rbind", cf1)
df <- data.frame(taus = taus, cf.df)
rownames(df) <- NULL

p2 <- ggplot(df, aes(taus, coefficients)) + 
  geom_ribbon(aes(ymin = lower.bd, ymax = upper.bd), fill = "#dce7f2") + 
  geom_line(color = "#2752a4") + 
  geom_hline(yintercept = 0, color = "gray") + 
  geom_hline(yintercept = olscf[parm, "coefficients"], color = mypal[8]) + 
  geom_hline(yintercept = olscf[parm, "lower.bd"], color = mypal[8], 
             linetype = "dashed") + 
  geom_hline(yintercept = olscf[parm, "upper.bd"], color = mypal[8], 
             linetype = "dashed") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-25, 10), 
                     breaks = seq(-25, 10, by = 5)) +  
  labs(x = "Quantile level", y = "Coefficients", 
       title = "log(PAir)") + 
  theme_publication() + 
  theme(panel.grid.major = element_blank())
print(p2)

# quantile regression coefficients plot for log(PRailway)
parm <- "log(PRailway)"
cf1 <- lapply(cf, function(x) x[parm, , drop = FALSE])
cf.df <- do.call("rbind", cf1)
df <- data.frame(taus = taus, cf.df)
rownames(df) <- NULL

p3 <- ggplot(df, aes(taus, coefficients)) + 
  geom_ribbon(aes(ymin = lower.bd, ymax = upper.bd), fill = "#dce7f2") + 
  geom_line(color = "#2752a4") + 
  geom_hline(yintercept = 0, color = "gray") + 
  geom_hline(yintercept = olscf[parm, "coefficients"], color = mypal[8]) + 
  geom_hline(yintercept = olscf[parm, "lower.bd"], color = mypal[8], 
             linetype = "dashed") + 
  geom_hline(yintercept = olscf[parm, "upper.bd"], color = mypal[8], 
             linetype = "dashed") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-25, 10), 
                     breaks = seq(-25, 10, by = 5)) + 
  labs(x = "Quantile level", y = "Coefficients", 
       title = "log(PRail)") + 
  theme_publication() + 
  theme(panel.grid.major = element_blank())
print(p3)

# quantile regression coefficients plot for log(PRoad)
parm <- "log(PRoad)"
cf1 <- lapply(cf, function(x) x[parm, , drop = FALSE])
cf.df <- do.call("rbind", cf1)
df <- data.frame(taus = taus, cf.df)
rownames(df) <- NULL

p4 <- ggplot(df, aes(taus, coefficients)) + 
  geom_ribbon(aes(ymin = lower.bd, ymax = upper.bd), fill = "#dce7f2") + 
  geom_line(color = "#2752a4") + 
  geom_hline(yintercept = 0, color = "gray") + 
  geom_hline(yintercept = olscf[parm, "coefficients"], color = mypal[8]) + 
  geom_hline(yintercept = olscf[parm, "lower.bd"], color = mypal[8], 
             linetype = "dashed") + 
  geom_hline(yintercept = olscf[parm, "upper.bd"], color = mypal[8], 
             linetype = "dashed") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-25, 10), 
                     breaks = seq(-25, 10, by = 5)) + 
  labs(x = "Quantile level", y = "Coefficients", 
       title = "log(PRoad)") + 
  theme_publication() + 
  theme(panel.grid.major = element_blank())
print(p4)

# arrange four plots
p <- cowplot::plot_grid(p1, p2, p3, p4, nrow = 2, align = "v",
                        labels = c('(a)', '(b)', '(c)', '(d)'))

outfile <- "figs/arrival_peak_day_quantile_regression_plot.pdf"
pdf(file = outfile, width = 10, height = 8)
print(p)
dev.off()
