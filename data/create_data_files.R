# Gather base data for server

library(rIMPD)
library(tidyverse)
library(sf)


impd_meta <- search_impd() %>%
  write.csv("impd_meta.csv")


quga_dat <- search_impd(species = "QUGA") %>%
  mutate(FHX = map(studyCode, get_impd_fhx))

save(quga_dat, file = "quga_dat.rda")
