# Texas Political Data

#Package load
library(ggplot2)
library(tidyverse)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

earlyvoting <- read.csv("Texas Early Voting 2020.csv")
countyresults <- read.csv("County Level Presidential Election Results 2000-2016.csv")
txsenate2018 <- read.csv("Texas Senate Election 2018.csv")

# Pulling together columns
early.cumulative <- earlyvoting %>%
  select(County, Cumulative.In.Person.Voters) %>%
  rename(earlyvote2020 = Cumulative.In.Person.Voters,
         county = County) %>%
  mutate(county = tolower(county))

tx.countyresults.2016 <- countyresults %>%
  filter(year == "2016" & state_po == "TX") %>%
  spread(key = candidate, value = candidatevotes) %>%
  group_by(county) %>%
  summarize(trump = sum(`Donald Trump`, na.rm = T),
            clinton = sum(`Hillary Clinton`, na.rm = T),
            other = sum(`Other`, na.rm = T)) %>%
  mutate(totalvotes = trump + clinton + other,
         trump_pct = trump/totalvotes,
         clinton_pct = clinton/totalvotes,
         other_pct = other/totalvotes,
         county = tolower(county))

txsenate2018 <- txsenate2018 %>%
  rename(county = ï..County) %>%
  mutate(orourke_pct = DEM/Votes,
         cruz_pct = REP/Votes,
         county = tolower(county)) %>%
  select(county, orourke_pct, cruz_pct)


tx.summary <- merge(tx.countyresults.2016, txsenate2018, all.x = T)
tx.summary <- merge(tx.summary, early.cumulative, all.x = T)

#Imputing Trump vote
tx.imputed <- tx.summary %>%
  mutate(imputedtrump2020.2016 = earlyvote2020*trump_pct,
         imputedbiden2020.2016 = earlyvote2020*clinton_pct,
         imputedtrump2020.2018 = earlyvote2020*cruz_pct,
         imputedbiden2020.2018 = earlyvote2020*orourke_pct)

#Imputed results
biden.2016 <- sum(tx.imputed$imputedbiden2020.2016, na.rm = T)
trump.2016 <- sum(tx.imputed$imputedtrump2020.2016, na.rm = T)

biden.2016/sum(tx.imputed$earlyvote2020, na.rm = T)
trump.2016/sum(tx.imputed$earlyvote2020, na.rm = T)

biden.2018 <- sum(tx.imputed$imputedbiden2020.2018, na.rm = T)
trump.2018 <- sum(tx.imputed$imputedtrump2020.2018, na.rm = T)

biden.2018/sum(tx.imputed$earlyvote2020, na.rm = T)
trump.2018/sum(tx.imputed$earlyvote2020, na.rm = T)