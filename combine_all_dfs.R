#### combining all long data frames to get the final number of BC patients and RT patients

library(dplyr)
library(magrittr)
library(data.table)
library(tidyr)
library(ggplot2)
library(stringr)

all_RT_patients_df <- read.table(file = "only_BC_patients_icd_and_opcs.tsv", sep = "\t", header = T)
all_BC_patients_df <- read.table(file = "only_BC_patients_icd_and_opcs.tsv", sep = "\t", header = T)
demographics_df <- read.table(file = "demographics_dataframe_UKB.tsv", sep = "\t", header = T)


all_BC_patients_icd10 <- read.table(file = "all_BC_patients_icd10.tsv", sep = "\t", header = T)
all_BC_patients_icd9 <- read.table(file = "all_BC_patients_icd9.tsv", sep = "\t", header = T)
all_BC_patients_opcs4 <- read.table(file = "all_BC_patients_icd9.tsv", sep = "\t", header = T)


print(head(demographics_df))

test_df <- inner_join(all_BC_patients_icd10, demographics_df)
head(test_df)
