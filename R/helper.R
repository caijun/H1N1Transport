library(incidence)

# daily epidemic curve
calc.dec <- function(linelist) {
  dec <- incidence(linelist$diagnose, first_date = as.Date("2009-05-01"), 
                   last_date = as.Date("2010-04-30"))
  return(dec)
}

est.arr.pk.day <- function(dec) {
  idx.arr <- which(dec$counts > 0)[1]
  arr <- as.character(dec$dates)[idx.arr]
  idx.pk <- which.max(dec$counts)
  pk <- as.character(dec$dates)[idx.pk]
  return(data.frame(idx.arr, arr, idx.pk, pk, stringsAsFactors = F))
}

# calculate daily epidemic curve, and estimate arrival and peak day 
# for each prefecture
calc.dec.pref <- function(linelist) {
  dec.pref <- linelist %>% 
    dplyr::group_by(GBPref, FullPref) %>% 
    do(as.data.frame(calc.dec(.))) %>% 
    dplyr::rename(PrefName = FullPref) %>% 
    as.data.frame()
  
  arr.pk <- dec.pref %>% 
    dplyr::group_by(GBPref, PrefName) %>% 
    do(est.arr.pk.day(.)) %>% 
    as.data.frame()
  
  return(list(dec.pref = dec.pref, arr.pk = arr.pk))
}