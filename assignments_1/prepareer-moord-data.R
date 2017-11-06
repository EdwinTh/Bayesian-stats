library(tidyverse)
library(padr)
library(stringr)

splitted_to_date <- function(splitted) {
  lookup <- 1:12
  names(lookup) <- c("jan.", "feb.", "mrt.", "apr.", "mei", "jun.", "jul.", "aug.", 
                     "sep.", "okt.", "nov.", "dec.")
  paste(splitted[[1]], lookup[splitted[[2]]], splitted[[3]]) %>% 
    as.Date(format = "%e %m %Y")
}


moorden <- read_csv("~/Desktop/moorddata.csv", col_names = FALSE)
moorden_count <- str_split(moorden %>% pull(), " ") %>% 
  map(splitted_to_date) %>% do.call(c, .) %>% 
  data_frame(d = .) %>% 
  arrange(d) %>% 
  thicken("week", "week") %>% 
  count(week) %>% 
  pad() %>% 
  fill_by_value()

write_csv(moorden_count, "~/Bayesian-stats/assignments_1/moorddata.csv")
