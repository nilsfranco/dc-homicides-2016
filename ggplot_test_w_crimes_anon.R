library(readr); library(dplyr); library(reshape2); library(ggplot2); library(tidyr); library(lubridate); library(forcats)
setwd("~/Offline/dc-homicides-2016")
currently.dst <- dst(Sys.time())
crimes_anon <- read_csv("crimes_anon.csv")
homicides <- crimes_anon %>% 
  filter(offense_category == "Homicide", person_type=="SUSPECT IN OFFENSE") %>% 
  select(crime_id = ccn, age, sex, ethnicity, race, offense_location_psa, offense_report_date) %>% 
  mutate(identified = (!is.na(age) | sex!="Unknown" | ethnicity!="Unknown" | race!="Unknown")) %>%
  mutate(offense_report_date_dst = dst(with_tz(offense_report_date, tzone = "America/New_York"))) %>%
  mutate(offense_report_date_est = case_when(offense_report_date_dst == FALSE ~ with_tz(offense_report_date, tzone = "Etc/GMT+5"))) %>%
  mutate(offense_report_date_edt = case_when(offense_report_date_dst == TRUE ~ with_tz(offense_report_date, tzone = "Etc/GMT+4"))) %>%
  mutate(offense_report_time_est = as.character(strftime(offense_report_date_est, format = "%H00"))) %>%
  mutate(offense_report_time_edt = as.character(strftime(offense_report_date_edt, format = "%H00"))) %>%
  mutate(offense_report_time = case_when(offense_report_date_dst == TRUE ~ offense_report_time_edt, TRUE ~ offense_report_time_est)) %>%
  select(-starts_with("offense_report_date_"), -starts_with("offense_report_time_"))

homicides_hr <- homicides %>%
  group_by(offense_report_time, identified) %>% 
  summarize(count = n())

plot <- homicides_hr %>% 
  ggplot(aes(x = offense_report_time, y = count, fill = identified))  + 
  ylim(-10,12) + 
  theme_minimal() + 
  geom_bar(stat = "identity", width = .98) + 
  ylab("Homicides") + 
  xlab("Hour Offense Reported") +
  ggtitle("Homicide Suspect Identification by Hour Reported", subtitle = "Reported in Washington, DC, in 2016") +
  coord_polar() + 
  labs(fill = "Suspect\n Details\nIdentified")

homicides_mo <- homicides %>%
  group_by(offense_mo, identified) %>% 
  summarize(count = n())
plot <- homicides_mo %>% 
  ggplot(aes(x = offense_mo, y = count, fill = identified))  + 
  theme_minimal() + 
  ylim(-10,12) + 
  geom_bar(stat = "identity", width = .98) + 
# ylim(0, 1) + 
# geom_bar(stat =)
  coord_polar()

#  ylim(-0.7,1) + 
#  coord_polar()
## I want a ggplot viz that shows [x] hour [y] proportion over time [z] number of crimes,  in a circular viz where midnight is at top with 2300 counterclockwise and 0100 clockwise.

  #unidentified_homicides <- homicides %>% 
  #  filter(is.na(age), sex=="Unknown", ethnicity=="Unknown", race=="Unknown")
  #noage_ided_homicides <- homicides %>% 
  #  filter(is.na(age), sex!="Unknown", ethnicity!="Unknown", race!="Unknown")
  #identified_homicides <- homicides %>% 
  #  filter(!is.na(age), sex!="Unknown", ethnicity!="Unknown", race!="Unknown")