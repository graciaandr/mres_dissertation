#### combining all long data frames to get the final number of BC patients and RT patients

library(dplyr)
library(magrittr)
library(data.table)
library(tidyr)
library(ggplot2)
library(stringr)

setwd("/workspace/home/gandriamiadana/mres_dissertation/")

all_patients_BC_status_df <- read.table(file = "only_BC_patients_icd_and_opcs.tsv", sep = "\t", header = T)
all_patients_RT_status_df <- read.table(file = "only_RT_patients_icd_and_opcs.tsv", sep = "\t", header = T)

# RT_patients_df <- all_patients_RT_status_df %>% filter(RT_status ==1)
# BC_patients_df <- all_patients_BC_status_df %>% filter(Breast_status ==1)

# patients_with_BC_and_RT <- (intersect(RT_patients_df$f.eid, BC_patients_df$f.eid))

demographics_df <- read.table(file = "demographics_dataframe_UKB.tsv", sep = "\t", header = T)

# all_BC_patients_icd10 <- read.table(file = "all_BC_patients_icd10.tsv", sep = "\t", header = T)
# all_BC_patients_icd9 <- read.table(file = "all_BC_patients_icd9.tsv", sep = "\t", header = T)
# all_BC_patients_opcs4 <- read.table(file = "all_BC_patients_icd9.tsv", sep = "\t", header = T)

# all_RT_sessions_opcs4 <- read.table(file = "all_RT_sessions_opcs4.tsv", sep = "\t", header = T)
# all_RT_sessions_icd10 <- read.table(file = "all_RT_sessions_icd10.tsv", sep = "\t", header = T)


# duplicates_RT_sessions <- all_RT_sessions_icd10[duplicated(all_RT_sessions_icd10$f.eid) | 
#                     duplicated(all_RT_sessions_icd10$f.eid, 
#                     fromLast=TRUE),]


# patients_w_multiple_RT_sessions <- all_RT_sessions_icd10 %>% group_by(f.eid) %>%
#    filter(n() > 1) %>% ungroup
# View((patients_w_multiple_RT_sessions))

# test <- left_join(BC_patients_df, all_BC_patients_icd10, by = "f.eid")

# all_BC_patients_icd10$f.eid
# BC_patients_df
demographics_all_patients_BC_status_df <- full_join(all_patients_BC_status_df,
                                                     demographics_df)
# demographics_all_patients_RT_status_df <- inner_join(all_patients_RT_status_df,
#                                                      demographics_df)
View(head(demographics_all_patients_BC_status_df))

demographics_all_patients_BC_status_df %>% filter(f.eid == "1000041") %>% select(alc_yday_online_cycle3)
demographics_df %>% select(alc_yday_online_cycle3) %>% head(10)

demographics_all_patients_BC_status_df <- full_join(all_patients_BC_status_df,
                                                     demographics_df)