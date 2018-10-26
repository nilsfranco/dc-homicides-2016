library(readr); library(dplyr); library(reshape2); library(ggplot2); library(tidyr); library(lubridate); library(forcats) 
setwd("~/Offline/dc-weapons-2016")
currently.dst <- dst(Sys.time())
crimes_anon <- read_csv("crimes_anon.csv")
weapons <- crimes_anon %>% 
  filter(dmpsj_category == "Weapons", person_type=="SUSPECT IN OFFENSE") %>% 
  select(crime_id = ccn, age, sex, ethnicity, race, offense_location_psa, offense_report_date) %>% 
  mutate(identified = (!is.na(age) | sex!="Unknown" | ethnicity!="Unknown" | race!="Unknown")) %>%
  mutate(offense_report_date_dst = dst(with_tz(offense_report_date, tzone = "America/New_York"))) %>%
  mutate(offense_report_date_est = case_when(offense_report_date_dst == FALSE ~ with_tz(offense_report_date, tzone = "Etc/GMT+5"))) %>%
  mutate(offense_report_date_edt = case_when(offense_report_date_dst == TRUE ~ with_tz(offense_report_date, tzone = "Etc/GMT+4"))) %>%
  mutate(offense_report_time_est = as.character(strftime(offense_report_date_est, format = "%H00"))) %>%
  mutate(offense_report_time_edt = as.character(strftime(offense_report_date_edt, format = "%H00"))) %>%
  mutate(offense_report_time = case_when(offense_report_date_dst == TRUE ~ offense_report_time_edt, TRUE ~ offense_report_time_est)) %>%
  mutate(offense_report_mo = month(offense_report_date, label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME"))) %>%
  select(-starts_with("offense_report_date_"), -starts_with("offense_report_time_"))

weapons_hr <- weapons %>%
  group_by(offense_report_time) %>% 
  summarize(count = n())

weapons_mo <- weapons %>%
  group_by(offense_report_mo) %>% 
  summarize(count = n())

#weapons_race <- weapons %>%
#  group_by(race) %>% 
#  summarize(count = n()) %>%
#  arrange(desc(-count))
#
#weapons_hr$race = factor(weapons_hr$race, levels = weapons_race$race)
#
#levels(weapons_hr$race)

weapons_hr_plot <- weapons_hr %>% 
  ggplot(aes(x = offense_report_time, y = count))  + 
  theme_minimal() + 
  geom_bar(stat = "identity", width = 0.98) + 
  ylab("Weapons Offenses") + 
  xlab("Hour Reported") +
  ggtitle("Hour Weapons Offenses Occur or are Reported", subtitle = "District of Columbia, 2016 - NEAR Act Data") +
  coord_polar()
weapons_hr_plot

weapons_mo_plot <- weapons_mo %>% 
  ggplot(aes(x = offense_report_mo, y = count))  + 
  theme_minimal() +
  theme(panel.grid = element_line(colour = "red"), axis.text.x = element_text(size = 12)) +
  ylim(0,140) +
  geom_bar(stat = "identity", width = 0.96) +
  ylab("Weapons Offenses") +
  xlab("Hour Reported") +
  ggtitle("Weapons Offense Reports by Month", subtitle = "2016 NEAR Act Data for the District of Columbia") +
  coord_polar()
weapons_mo_plot