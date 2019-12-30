#### Explore VAERS---------

#load data & packages
library(tidyverse)
library(dplyr)
library(gdata)

###NO Code in RMD

#read in data
data <- read.csv("Rev_Juliana_Final/data/unprocessed/2014VAERSData/2014VAERSDATA.csv")

vax_data <- read.csv("Rev_Juliana_Final/data/unprocessed/2014VAERSData/2014VAERSVAX.csv")

state_pop <- read.csv("Rev_Juliana_Final/data/unprocessed/PEP_2014_PEPSR6H/state_pop.csv")


#merge data
data_vax_2 <- merge(data, vax_data)%>%
  mutate(DIED = as.character(DIED),
         DIED = case_when(DIED == "" ~ NA_character_,
                          TRUE ~ DIED),
         DATEDIED = as.character(DATEDIED),
         DATEDIED = case_when(DATEDIED == "" ~ NA_character_,
                          TRUE ~ DATEDIED),
         L_THREAT = as.character(L_THREAT),
         L_THREAT = case_when(L_THREAT == "" ~ NA_character_,
                          TRUE ~ L_THREAT),
         ER_VISIT = as.character(ER_VISIT),
         ER_VISIT = case_when(ER_VISIT == "" ~ NA_character_,
                          TRUE ~ ER_VISIT),
         HOSPITAL = as.character(HOSPITAL),
         HOSPITAL = case_when(HOSPITAL == "" ~ NA_character_,
                          TRUE ~ HOSPITAL),
         RPT_DATE = as.character(RPT_DATE),
         RPT_DATE = case_when(RPT_DATE == "" ~ NA_character_,
                          TRUE ~ RPT_DATE),
         STATE = as.character(STATE),
         STATE = case_when(STATE == "" ~ NA_character_,
                          TRUE ~ STATE),
         X_STAY = as.character(X_STAY),
         X_STAY = case_when(X_STAY == "" ~ NA_character_,
                          TRUE ~ X_STAY),
         DISABLE = as.character(DISABLE),
         DISABLE = case_when(DISABLE == "" ~ NA_character_,
                          TRUE ~ DISABLE),
         ONSET_DATE = as.character(ONSET_DATE),
         ONSET_DATE = case_when(ONSET_DATE == "" ~ NA_character_,
                          TRUE ~ ONSET_DATE),
         LAB_DATA = as.character(LAB_DATA),
         LAB_DATA = case_when(LAB_DATA == "" ~ NA_character_,
                          TRUE ~ LAB_DATA),
         OTHER_MEDS = as.character(OTHER_MEDS),
         OTHER_MEDS = case_when(OTHER_MEDS == "" ~ NA_character_,
                          TRUE ~ OTHER_MEDS),
         HISTORY = as.character(HISTORY),
         HISTORY = case_when(HISTORY == "" ~ NA_character_,
                          TRUE ~ HISTORY),
         PRIOR_VAX = as.character(PRIOR_VAX),
         PRIOR_VAX = case_when(PRIOR_VAX == "" ~ NA_character_,
                          TRUE ~ PRIOR_VAX),
         SPLTTYPE = as.character(SPLTTYPE),
         SPLTTYPE = case_when(SPLTTYPE == "" ~ NA_character_,
                          TRUE ~ SPLTTYPE),
         VAX_LOT = as.character(VAX_LOT),
         VAX_LOT = case_when(VAX_LOT == "" ~ NA_character_,
                          TRUE ~ VAX_LOT),
         VAX_ROUTE = as.character(VAX_ROUTE),
         VAX_ROUTE = case_when(VAX_ROUTE == "" ~ NA_character_,
                          TRUE ~ VAX_ROUTE),
         VAX_SITE = as.character(VAX_SITE),
         VAX_SITE = case_when(VAX_SITE == "" ~ NA_character_,
                          TRUE ~ VAX_SITE))

#change data into dates
data_vax_2$RPT_DATE <- as.Date(data_vax_2$RPT_DATE, "%m/%d/%Y")
data_vax_2$VAX_DATE <- as.Date(data_vax_2$VAX_DATE, "%m/%d/%Y")
data_vax_2$ONSET_DATE <- as.Date(data_vax_2$ONSET_DATE, "%m/%d/%Y")

#find difference between report and vax/onset
data_vax_2$NUMDAYS_RPT_VAX <- difftime(data_vax_2$RPT_DATE, data_vax_2$VAX_DATE, units = c("days"))
data_vax_2$NUMDAYS_RPT_ONSET <- difftime(data_vax_2$RPT_DATE, data_vax_2$ONSET_DATE, units = c("days"))
data_vax_2$NUMDAYS_VAX_ONSET <- difftime(data_vax_2$ONSET_DATE, data_vax_2$VAX_DATE, units = c("days"))

data_vax_2 <- data_vax_2%>%
  mutate(NUMDAYS_RPT_VAX = abs(as.numeric(NUMDAYS_RPT_VAX)),
         NUMDAYS_RPT_ONSET = abs(as.numeric(NUMDAYS_RPT_ONSET)),
         NUMDAYS_VAX_ONSET = abs(as.numeric(NUMDAYS_VAX_ONSET)))

#do by state and sex
states <- as.data.frame((cbind(state.abb, state.name)))

#plot by state and sex
plot_1 <- ggplot(data_vax_2, aes(STATE))+
  geom_bar(aes(fill = SEX))+
  ggtitle("VAERS Reports by State and Sex")+
  labs(x = "State")+
  theme_minimal()

#state adjusted for pop
data_vax_state <- state_pop%>%
  filter(Year.id == "est72014",
         Hisp.display.label == "Total",
         Sex.id == "totsex")%>%
  left_join(states,by = c("GEO.display.label" = "state.name"))%>%
  left_join(data_vax_2, by = c("state.abb" = "STATE"))%>%
  select(totpop,state.abb, RECVDATE:NUMDAYS_VAX_ONSET)

#plot by state and proportion of pop injured
woof <- plot_2 <- data_vax_state%>%
  group_by(state.abb)%>%
  count(state.abb)%>%
  left_join(data_vax_state)%>%
  group_by(state.abb)%>%
  mutate(totpop = as.numeric(as.character(totpop)), 
         report_pop = (n/totpop))%>%
  filter(!is.na(state.abb))%>%
  select(report_pop, everything())

plot_2 <- data_vax_state%>%
  group_by(state.abb)%>%
  count(state.abb)%>%
  left_join(data_vax_state)%>%
  group_by(state.abb)%>%
  mutate(totpop = as.numeric(as.character(totpop)), 
         report_pop = (n/totpop))%>%
  filter(!is.na(state.abb))%>%
  select(report_pop, everything())%>%
  ggplot(aes(state.abb, report_pop))+
  geom_bar(stat = "identity", fill = "blue")+
  labs(x = "State", y = "Number of VAERS Reports as Proportion\n of State Population",
       title = "Proportion of VAERS Report per Population (2014)")
plot_27 <- data_vax_fct%>%
  group_by(AGES, SEX)%>%
  count()%>%
  ggplot(aes(SEX, AGES))+
  geom_tile(aes(fill = n))
  
#by state
plot_3 <- ggplot(data_vax_2, aes(STATE))+
  geom_bar()+
  ggtitle("VAERS Reports by State")+
  labs(x = "State")+
  theme_minimal()

#by sex
plot_4 <- ggplot(data_vax_2, aes(SEX))+
  geom_bar(fill = "red")+
  ggtitle("VAERS Reports by Sex")+
  labs(x = "Sex")+
  theme_minimal()

#by Vax type

plot_5 <- data_vax_2%>%
  count(VAX_TYPE)%>%
  filter(n > 500)%>%
  ggplot(aes(VAX_TYPE, n))+
  geom_bar(fill = "blue", stat = "identity")+
  ggtitle("VAERS Reports by Vaccine for Reports > 500")+
  labs(x = "Vaccine", y = "Count")+
  theme_minimal()

#num of Days 

  
  plot_6 <- data_vax_2%>%
  filter(NUMDAYS_RPT_VAX <= 7)%>%
  ggplot(aes(NUMDAYS_RPT_VAX))+
  geom_histogram(bins = 20)+
  labs(x = "Days After Vaccination", y = "Count",
       title = "VAERS Reports Up to 1 Week Post-Vaccination")+
    scale_x_continuous(limits = c(-1, 7))+
  theme_minimal()
  
woof <- data_vax_2%>%
    filter(NUMDAYS_RPT_VAX <= 7)
  stat_6 <- summary(woof$NUMDAYS_RPT_VAX)
  
#num of Days - by sex
plot_7 <- data_vax_2%>%
  ggplot(aes(NUMDAYS_RPT_VAX, NUMDAYS_RPT_ONSET))+
  geom_point()+
  labs(title = "Number of Days Post-Vaccination and Post-Onset Before VAERS Report",
       x = "Numberr of Days Between Vaccination and VAERS Report",
       y = "Number of Days Between Injury Onset and VAERS Report")

woof2 <- data_vax_2%>%
  filter(NUMDAYS_RPT_VAX > 7)
stat_7 <- summary(woof2$NUMDAYS_RPT_VAX)

#num of days > 1 month
plot_8 <- data_vax_2%>%
  filter(!is.na(NUMDAYS_RPT_VAX))%>%
  filter(NUMDAYS_RPT_VAX <= 30 & NUMDAYS_RPT_VAX >7 )%>%
  ggplot(aes(NUMDAYS_RPT_VAX))+
  geom_histogram(bins = 21)+
  labs(x = "Days After Vaccination", y = "Count",
       title = "VAERS Reports Between 1 Week and 1 Month Post-Vaccination")+
  theme_minimal()

woof2 <- data_vax_2%>%
  filter(!is.na(NUMDAYS_RPT_VAX))%>%
  filter(NUMDAYS_RPT_VAX <= 30 & NUMDAYS_RPT_VAX >7 )

stat_8 <- summary(woof2$NUMDAYS_RPT_VAX)
#num of days > 1 month, by sex
plot_9 <- data_vax_2%>%
  filter(!is.na(NUMDAYS))%>%
  filter(NUMDAYS <= 30 & NUMDAYS >7 )%>%
  ggplot(aes(NUMDAYS, fill = SEX))+
  geom_histogram(bins = 21)+
  labs(x = "Days After Vaccination", y = "Count",
       title = "VAERS Reports Between 1 Week and 1 Month Post-Vaccination, by Sex")+
  theme_minimal()

#avg days after reporting

plot_10 <- data_vax_2%>%
  group_by( VAX_TYPE)%>%
  count(VAX_TYPE)%>%
  left_join(data_vax_2)%>%
  filter(n >50)%>%
  group_by(VAX_TYPE, SEX)%>%
  filter(SEX != "U")%>%
  filter(!is.na(NUMDAYS_RPT_VAX))%>%
  mutate(mean_numday = mean(NUMDAYS_RPT_VAX))%>%
  ggplot(aes(SEX, VAX_TYPE))+
  geom_tile(aes(fill = mean_numday))+
  labs(x = "Sex", y = "Vaccine", 
       title = "Mean Difference between Date of Vaccination\n and VAERS Report per Vaccine and Sex\n (n > 50)",
       fill = "Mean Number of Days\n Until Reporting")
#skewed by one person reporting shingles from a vaccine they recieved nearly 50 years prior

#factorize age
data_vax_fct <- data_vax_2%>%
  mutate(CAGE_YR = as.factor(CAGE_YR))%>%
  mutate(AGES = fct_collapse(CAGE_YR,
                                newborn = c("0"),
                                toddler = c("1", "2", "3"),
                                child = c("4","5", "6", "7", "8", "9"),
                             tween = c("10", "11", "12"),
                            teen = c("13", "14", "15", "16", "17", "18", "19"),
                            twenties = c("20", "21", "22", "23", "24", "25", "26", "27", "28", "29"),
                            thirites =  c("30", "31", "32", "33", "34", "35", "36", "37", "38", "39"),
                            fourties =  c("40", "41", "42", "43", "44", "45", "46", "47", "48", "49"),
                            fifties =  c("50", "51", "52", "53", "54", "55", "56","57", "58", "59"),
                            sixties = c("60", "61", "62", "63", "64", "65", "66", "67", "68", "69"),
                            seventies = c("70", "71", "72", "73", "74", "75", "76", "77", "78", "79"),
                            eighties =  c("80", "81", "82", "83", "84", "85", "86", "87", "88", "89"),
                            ninties =  c("90", "91", "92", "93", "94", "95", "96", "97", "98", "99")))%>%
  select(AGES, everything())%>%
  filter(!is.na(AGES))

#plot factors for age
plot_11 <- ggplot(data_vax_fct, aes(AGES))+
  geom_bar(fill = "orange")+
  labs(title = "VAERS Injuries by Age", x = "Age", y = "Number of VAERS Reports")+
  theme_minimal()


#age vaccine relationship
plot_23 <- data_vax_fct%>%
  group_by(VAX_TYPE, AGES)%>%
  count(VAX_TYPE)%>%
  filter(n >50)%>%
  ggplot(aes(AGES, VAX_TYPE))+
  geom_tile(aes(fill = n))+
  labs(title = "VAERS Reports by Vaccine and Age (for n > 50)", y = "Vaccine",
       x = "Age Group", fill = "Number of Reports")

#ages by sex
plot_12 <- ggplot(data_vax_fct, aes(AGES))+
  geom_bar(aes(fill = SEX))+
  labs(title = "Number of VAERS Reports by Age Group", x = "Age Group",
       y = "Number of VAERS Reports", fill = "Sex")+
  theme_minimal()

#vaccine reports by gender
plot_13 <- data_vax_fct%>%
  group_by(VAX_TYPE)%>%
  count(VAX_TYPE)%>%
  left_join(data_vax_2)%>%
  filter(n >50)%>%
  group_by(VAX_TYPE, SEX)%>%
  filter(SEX != "U")%>%
  ggplot(aes(VAX_TYPE))+
  geom_bar(aes(fill = SEX))+
  coord_flip()+
  labs(title = "VAERS Reports by Gender per Vaccine (n > 50)", x = "Vaccine",
       y = "Number of Reports", fill = "Sex")
#vaccine reports by sex and month
plot_28 <- data_vax_fct%>%
  mutate(RECVDATE = lubridate::mdy(RECVDATE),
         month_recv = lubridate::month(RECVDATE, label = TRUE),
         day_recv = lubridate::day(RECVDATE),
         RPT_DATE = lubridate::ymd(RPT_DATE),
         month_rpt =  lubridate::month(RPT_DATE, label = TRUE),
         RPT_DATE = lubridate::day(RPT_DATE))%>%
  ggplot(aes(month_recv))+
  geom_bar(fill = "green")+
  labs(title = "VAERS Reports per Month Vaccine was Recieved ",
       x = "Month Vaccine was Recieved", y = "Number of VAERS Reports")+
  theme_minimal()


plot_14 <- data_vax_fct%>%
  mutate(RECVDATE = lubridate::mdy(RECVDATE),
         month_recv = lubridate::month(RECVDATE, label = TRUE),
         day_recv = lubridate::day(RECVDATE),
         RPT_DATE = lubridate::ymd(RPT_DATE),
         month_rpt =  lubridate::month(RPT_DATE, label = TRUE),
         RPT_DATE = lubridate::day(RPT_DATE))%>%
  ggplot(aes(month_recv))+
  geom_bar(aes(fill = SEX))+
  labs(title = "VAERS Reports per Month Vaccine was Recieved by Sex",
       x = "Month Vaccine was Recieved", y = "Number of VAERS Reports", fill = "Sex")+
  theme_minimal()
  

data_vax_date <- data_vax_fct%>%
  mutate(RECVDATE = lubridate::mdy(RECVDATE),
         month_recv = lubridate::month(RECVDATE, label = TRUE),
         day_recv = lubridate::day(RECVDATE),
         RPT_DATE = lubridate::ymd(RPT_DATE),
         month_rpt =  lubridate::month(RPT_DATE, label = TRUE),
         RPT_DATE = lubridate::day(RPT_DATE))

#vaers reports by vaccine and month recieved
plot_15 <- data_vax_date%>%
  group_by(VAX_TYPE, month_recv)%>%
  count(VAX_TYPE)%>%
  left_join(data_vax_date)%>%
  filter(n >50)%>%
  ggplot(aes(month_recv, VAX_TYPE))+
  geom_tile(aes(fill = n))+
  labs(x = "Month Recieved", y = "Vaccine",
       title = "VAERS Reports Per Vaccine per Month (n > 50)", fill = "Number of Reports")
#reports by age and month
plot_16 <- data_vax_date%>%
  group_by(AGES, month_recv)%>%
  count(AGES)%>%
  left_join(data_vax_date)%>%
  ggplot(aes(month_recv, AGES))+
  geom_tile(aes(fill = n))+
  scale_fill_gradient(low = "orange", high = "black")+
  labs(x = "Month Recieved", y = "Age Group",
       title = "VAERS Reports for Each Age Group per Month ", fill = "Number of Reports")
#age and state
plot_17 <- data_vax_date%>%
  group_by(AGES, STATE)%>%
  count(AGES)%>%
  left_join(data_vax_date)%>%
  ggplot(aes(AGES, STATE))+
  geom_tile(aes(fill = n))+
  scale_fill_gradient(low = "orange", high = "black")+
  labs(y = "State", x = "Age Group",
       title = "VAERS Reports for Each Age Group per State ", fill = "Number of Reports")

#age and state - adjusted for population
plot_18 <- data_vax_date%>%
  group_by(AGES, STATE)%>%
  count(AGES)%>%
  left_join(data_vax_date)%>%
  ggplot(aes(AGES, STATE))+
  geom_tile(aes(fill = n))+
  scale_fill_gradient(low = "orange", high = "black")+
  labs(y = "State", x = "Age Group",
       title = "VAERS Reports for Each Age Group per State ", fill = "Number of Reports")
#mean days rpted after onset
plot_19 <- data_vax_fct%>%
  group_by(VAX_TYPE, AGES)%>%
  count(VAX_TYPE)%>%
  left_join(data_vax_fct)%>%
  filter(n >50)%>%
  group_by(VAX_TYPE, AGES)%>%
  filter(!is.na(NUMDAYS_RPT_ONSET))%>%
  mutate(mean_days = mean(as.numeric(NUMDAYS_RPT_ONSET)))%>%
  select(mean_days, everything())%>%
  ggplot(aes(AGES, VAX_TYPE))+
  geom_tile(aes(fill = mean_days))+
  labs(title = "Mean Number of Days VAERS Report is Made After\n Onset by Vaccine (n >50) and Age Group",
       x = "Age Group", y = "Vaccine", fill = "Mean Number of Days\n Report Made\n After Onset")
#median days after onset
plot_20 <- data_vax_fct%>%
  group_by(VAX_TYPE, AGES)%>%
  count(VAX_TYPE)%>%
  left_join(data_vax_fct)%>%
  filter(n >50)%>%
  group_by(VAX_TYPE, AGES)%>%
  filter(!is.na(NUMDAYS_RPT_ONSET))%>%
  mutate(median_days = median(as.numeric(NUMDAYS_RPT_ONSET)))%>%
  select(median_days, everything())%>%
  ggplot(aes(AGES, VAX_TYPE))+
  geom_tile(aes(fill = median_days))+
  labs(title = "Median Number of Days VAERS Report is Made After\n Onset by Vaccine (n >50) and Age Group",
       x = "Age Group", y = "Vaccine", fill = "Median Number of Days\n Report Made\n After Onset")


#mean days rpted after onset
plot_21 <- data_vax_fct%>%
  group_by(VAX_TYPE, AGES)%>%
  count(VAX_TYPE)%>%
  left_join(data_vax_fct)%>%
  filter(n >50)%>%
  group_by(VAX_TYPE, AGES)%>%
  filter(!is.na(NUMDAYS_VAX_ONSET))%>%
  mutate(mean_days = mean(as.numeric(NUMDAYS_VAX_ONSET)))%>%
  select(mean_days, everything())%>%
  ggplot(aes(AGES, VAX_TYPE))+
  geom_tile(aes(fill = mean_days))+
  scale_fill_gradient(minor_breaks = c(50, 100, 150))+
  labs(title = "Mean Number of Days Onset is Reported After\n Vaccination by Vaccine (n >50) and Age Group",
       x = "Age Group", y = "Vaccine", fill = "Mean Number of Days\n Onset Reported\n After Vaccination")

#median days after onset
plot_22 <- data_vax_fct%>%
  group_by(VAX_TYPE, AGES)%>%
  count(VAX_TYPE)%>%
  left_join(data_vax_fct)%>%
  filter(n >50)%>%
  group_by(VAX_TYPE, AGES)%>%
  filter(!is.na(NUMDAYS_VAX_ONSET))%>%
  mutate(median_days = median(as.numeric(NUMDAYS_VAX_ONSET)))%>%
  select(median_days, everything())%>%
  ggplot(aes(AGES, VAX_TYPE))+
  geom_tile(aes(fill = median_days))+
  labs(title = "Median Number of Days Onset is Reported After\n Vaccination by Vaccine (n >50) and Age Group",
       x = "Age Group", y = "Vaccine", fill = "Median Number of Days\n Onset Reported\n After Vaccination")


# make RDS  
write_rds(data_vax_state, "data/processed/data_vax_state.rds")
write_csv(data_vax_state, "Rev_Juliana_Final/data/processed/data_vax_state.csv")
write_csv(data_vax_fct, "Rev_Juliana_Final/data/processed/data_vax_fct.csv")
write_csv(data_vax_date, "Rev_Juliana_Final/data/processed/data_vax_date.csv")
write_rds(data_vax_2, "data/processed/data_vax_merge.rds")
write_rds(data_vax_fct, "data/processed/data_vax_factor_age.rds")
write_rds(data_vax_date, "data/processed/data_vax_date.rds")
write_rds(plot_1, "data/processed/plot_state_sex.rds")
write_rds(plot_2, "data/processed/plot_state_prop.rds")
write_rds(plot_3, "data/processed/plot_state.rds")
write_rds(plot_4, "data/processed/plot_sex.rds")
write_rds(plot_5, "data/processed/plot_vaccine.rds")
write_rds(plot_6, "data/processed/plot_numdays.rds")
write_rds(stat_6, "data/processed/stat_numdays.rds")
write_rds(plot_7,"data/processed/plot_numdays2.rds" )
write_rds(plot_8, "data/processed/plot_numdaysplus7.rds")
write_rds(stat_8, "data/processed/stat_numdaysplus7.rds")
write_rds(plot_9, "data/processed/plot_days_1month.rds")
write_rds(plot_10, "data/processed/plot_days_1month_sex.rds")
write_rds(plot_11, "data/processed/plot_age.rds")
write_rds(plot_23, "data/processed/plot_age_vaccine.rds")
write_rds(plot_12, "data/processed/plot_age_sex.rds")
write_rds(plot_13, "data/processed/plot_vaccine_sex.rds")
write_rds(plot_14, "data/processed/plot_month_sex.rds")
write_rds(plot_15, "data/processed/plot_vaccine_month.rds")
write_rds(plot_16, "data/processed/plot_age_month.rds")
write_rds(plot_17, "data/processed/plot_state_age.rds")
write_rds(plot_18, "data/processed/plot_age_prop.rds")
write_rds(plot_19, "data/processed/plot_vaccine_age_meanrpt.rds")
write_rds(plot_20, "data/processed/plot_vaccine_age_medianrpt.rds")
write_rds(plot_21, "data/processed/plot_vaccine_age_meanonset.rds")
write_rds(plot_22, "data/processed/plot_vaccine_age_medianonset.rds")
write_rds(plot_27, "data/processed/plot_sex_age.rds")
write_rds(plot_28, "data/processed/plot_month.rds")
write_rds(data_vax_2, "data/processed/data_vax_2.rds")

