# helper functions for processing strings

# test case:
# str <- "~/Documents/R/H1N1/figs/confirmed/entire/GBProv/
#         daily_epidemic_curve/"
# strwrap(str)

strwrap <- function(str) {
  str <- base::strwrap(str, width = 1000)
  str <- stringr::str_replace_all(string = str, pattern = "\\s", replacement = "")
  return(str)
}

# type:{all, confirmed}
# period: {entire, pdm, post-pdm, pnd}
# SR (spatial resolution): {nation, GBProv, GBPref}
# directory template:parent/type/period/SR/topic/method/
create.dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = T)
  return(path)
}
