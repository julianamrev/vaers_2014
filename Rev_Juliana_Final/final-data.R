####Final data, percent of vaccine injury-----
#load data & packages
library(tidyverse)
library(dplyr)
library(gdata)

###NO Code in RMD

#read in data
data <- read.csv("Rev_Juliana_Final/data/unprocessed/2014VAERSData/2014VAERSDATA.csv")
vax_data <- read.csv("Rev_Juliana_Final/data/unprocessed/2014VAERSData/2014VAERSVAX.csv")

#get nis-child
nis_child <- read.csv("Rev_Juliana_Final/data/unprocessed/CountNIS.csv", skip = 4)%>%
  select(X, X.1)%>%
  rename("state.name" = X, "vaccine_rate_child" = X.1 )
nis_child <- nis_child[2:63,]
names_and_abb <- cbind(state.abb, state.name)
nis_child <-nis_child%>%
  merge(names_and_abb)%>%
  rename("STATE" = state.abb)%>%
  separate(vaccine_rate_child, c("vaccine_rate_child", "sd_child"), sep = "±")%>%
  mutate(vaccine_rate_child = as.numeric(vaccine_rate_child),
         sd_child = as.numeric(sd_child))

#get population data
child_pop_child <- read_csv("Rev_Juliana_Final/data/unprocessed/Child_pop.csv")%>%
  filter(DataFormat == "Number", TimeFrame == "2014", LocationType == "State")%>%
  rename.vars(from = 'Age group', to = "AgeGroup")%>%
  filter(AgeGroup == "0 to 4")%>%
  mutate(Data = Data %/%  3)%>%
  select(Location, Data)%>%
  rename.vars("Data", "pop")


#merge pop data to NIS data
nis_child <- left_join(nis_child, child_pop_child, by = c("state.name" = "Location"))%>%
  mutate(vaccine_rate_child = vaccine_rate_child/100, sd_child = sd_child /100)%>%
  mutate(pop_vax_child = pop*vaccine_rate_child)%>%
  select(STATE,vaccine_rate_child, sd_child, pop_vax_child, pop)

# filter data
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
                              TRUE ~ VAX_SITE))%>%
  filter(VAX_TYPE == "MMR")

  select(c(RECVDATE, STATE, CAGE_YR, SEX, RPT_DATE, DIED, DATEDIED, L_THREAT, ER_VISIT,
           HOSPITAL, HOSPDAYS, VAX_DATE, ONSET_DATE, NUMDAYS, VAX_DATE, VAX_TYPE))%>%
  filter(1.5<=AGE_YRS & AGE_YRs<3)%>%
  filter(VAX_TYPE == "MMR")%>%
  filter(!is.na(STATE))
#get total num
data_vax_child <- data_vax_2%>%
  filter(1.5<=AGE_YRS & AGE_YRS<3)%>%
  group_by(STATE)%>%
  summarise(n())%>%
  left_join(nis_child)%>%
  rename.vars("n()", "num_vaers_child")%>%
  mutate(pop_vax_child = pop_vax_child * (17/48),
         perc_vaers_child = num_vaers_child/pop_vax_child)%>%
  filter(!is.na(vaccine_rate_child))%>%
  mutate(exp_perc_itp = (.00005 * pop_vax_child)/pop_vax_child,
         exp_perc_severe = (.000001 * pop_vax_child)/pop_vax_child)

#https://stackoverflow.com/questions/13254441/add-a-horizontal-line-to-plot-and-legend-in-ggplot2

#perc of vax children by state
plot24 <- ggplot(data_vax_child, aes(STATE, perc_vaers_child))+
  geom_bar(stat = "identity", fill = "grey")+
  labs(x = "State", y = "Proportion of Vaccinated Children Reported to VAERS")+
  ggtitle("Proportion of Children (18-35 Months) Vaccinated for MMR\n Reporting to VAERS by State")+
  theme_minimal()

data_vax_child_severe <- data_vax_2%>%
  filter(1.5<=AGE_YRS & AGE_YRS<3)%>%
  filter(!is.na(DIED)|!is.na(ER_VISIT)|!is.na(HOSPITAL))%>%
    group_by(STATE)%>%
  summarise(n())%>%
  left_join(nis_child)%>%
  rename.vars("n()", "num_vaers_child")%>%
  mutate(pop_vax_child = pop_vax_child * (17/48),
         perc_vaers_child = num_vaers_child/pop_vax_child)%>%
  filter(!is.na(vaccine_rate_child))

plot25 <- ggplot(data_vax_child_severe, aes(STATE, perc_vaers_child))+
  geom_bar(stat = "identity", fill = "grey")+
  labs(x = "State", y = "Proportion of Vaccinated Children With\n Severe Injuries Reported to VAERS")+
  ggtitle("Percentage of Children (18-35 Months) Vaccinated for MMR\n Reporting Severe Injury to VAERS by State")+
  theme_minimal()


#died
data_vax_child_dead <- data_vax_2%>%
  filter(1.5<=AGE_YRS & AGE_YRS<3)%>%
  filter(!is.na(DIED))%>%
  group_by(STATE)%>%
  summarise(n())%>%
  left_join(nis_child)%>%
  rename.vars("n()", "num_vaers_child")%>%
  mutate(pop_vax_child = pop_vax_child * (17/48),
         perc_vaers_child = num_vaers_child/pop_vax_child)%>%
  filter(!is.na(vaccine_rate_child))
stat_dead <- sum(data_vax_child_dead$num_vaers_child)/sum(data_vax_child$pop_vax_child)

plot_29 <- ggplot(data_vax_child_dead, aes(STATE, perc_vaers_child))+
  geom_bar(stat = "identity", fill = "grey")+
  labs(x = "State", y = "Proportion of Vaccinated Children With\n Deaths Reported to VAERS")+
  ggtitle("Proportion of Children (18-35 Months) Vaccinated for MMR\n Reporting Deaths to VAERS by State")+
  geom_hline(yintercept = .000001, color = "orange")+
  theme_minimal()

#hospital
data_vax_child_hosp <- data_vax_2%>%
  filter(1.5<=AGE_YRS & AGE_YRS<3)%>%
  filter(!is.na(HOSPITAL) | !is.na(DIED))%>%
  group_by(STATE)%>%
  summarise(n())%>%
  left_join(nis_child)%>%
  rename.vars("n()", "num_vaers_child")%>%
  mutate(pop_vax_child = pop_vax_child * (17/48),
         perc_vaers_child = num_vaers_child/pop_vax_child)%>%
  filter(!is.na(vaccine_rate_child))

stat_hosp <- sum(data_vax_child_hosp$num_vaers_child)/sum(data_vax_child$pop_vax_child)



data_vax_2%>%
  filter(1.5<=AGE_YRS & AGE_YRS<3)%>%
  filter(!is.na(HOSPITAL))%>%
  skimr::skim()



plot26 <- ggplot(data_vax_child_hosp, aes(STATE, perc_vaers_child))+
  geom_bar(stat = "identity", fill = "grey")+
  labs(x = "State", y = "Proportion of Vaccinated Children With\n Hospitalizations Reported to VAERS")+
  ggtitle("Proportion of Children (18-35 Months) Vaccinated for MMR\n Reporting Hospitalizations  to VAERS by State")+
  geom_hline(yintercept = .000001, color = "orange")+
  theme_minimal()


stat_severe <- sum(data_vax_child_severe$num_vaers_child)/sum(data_vax_child$pop_vax_child)

#make rds
write_rds(data_vax_child, "Rev_Juliana_Final/data/processed/data_vax_child.rds")
write.csv(data_vax_child, "Rev_Juliana_Final/data/processed/data_vax_child.csv")
write.csv(data_vax_child_severe, "Rev_Juliana_Final/data/processed/data_vax_child_severe.csv")
write.csv(data_vax_child_hosp, "Rev_Juliana_Final/data/processed/data_vax_child_hosp.csv")
write.csv(data_vax_child_dead, "Rev_Juliana_Final/data/processed/data_vax_child_dead.csv")
write_rds(data_vax_child_severe, "Rev_Juliana_Final/data/processed/data_vax_child_severe.rds")
write_rds(data_vax_child_hosp, "Rev_Juliana_Final/data/processed/data_vax_child_hospital.rds")
write_rds(nis_child, "Rev_Juliana_Final/data/processed/nis_child.rds")
write_csv(nis_child, "Rev_Juliana_Final/data/processed/nis_child.csv")
write_rds(plot24, "Rev_Juliana_Final/data/processed/plot_state_perc.rds")
write_rds(plot25, "Rev_Juliana_Final/data/processed/plot_state_perc_severe.rds")
write_rds(plot26, "Rev_Juliana_Final/data/processed/plot_state_perc_hosp.rds")
write_rds(plot_29, "Rev_Juliana_Final/data/processed/plot_state_perc_dead.rds")
write_rds(stat_hosp,"Rev_Juliana_Final/data/processed/stat_hosp.rds" )
write_rds(stat_dead,"Rev_Juliana_Final/data/processed/stat_dead.rds" )
write_rds(stat_severe,"Rev_Juliana_Final/data/processed/stat_severe.rds" )
