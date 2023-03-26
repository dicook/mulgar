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
		 file=here::here("data", "pisa.rda"))

# Alternative source for the data is from the
# learningtower package. But only one test score
# is provided for each student, for each of read, math, science
library(learningtower)
all2018 <- load_student("2018")
aus_2018 <- all2018 %>%
	filter(country == "AUS")
