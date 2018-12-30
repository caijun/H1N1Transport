rm(list = ls())

load("output/confirmed_pdm_pref.rda")
# daily epidemic curve for each prefecture
# arrival day and peak day
arr.pk <- pref.dec$arr.pk
# now starting on 2009-05-01
# change the starting date to 2009-05-09, when the first case reported in 
# mainland China
arr.pk1 <- arr.pk %>% 
  mutate(idx.arr = idx.arr - 9, 
         idx.pk = idx.pk - 9)
# check arrival and peak days
summary(arr.pk1$idx.arr)
summary(arr.pk1$idx.pk)

# output arrival and peak days for visualizing on map
outfile <- "output/confirmed_pdm_pref_arrival_peak_day.csv"
write.csv(arr.pk1, file = outfile, row.names = F, quote = F)
