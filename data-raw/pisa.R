# PISA data processing
# The 2018 OECD PISA data can be accessed at
# https://webfs.oecd.org/pisa2018/SPSS_STU_QQQ.zip,
# read into R, subset and save, as follows:
library(tidyverse)
library(haven)
stu_qqq <- read_sav(here::here("data", "SPSS_STU_QQQ.zip"))
pisa <- stu_qqq %>%
	select(CNT, PV1MATH:PV10SCIE) %>%
	filter(CNT %in% c("AUS", "IDN"))

# Keep country, and scores
save(pisa,
		 file=here::here("data", "PISA_scores_2018.rda"))
