### array data frames reshaping

library(dplyr)
library(magrittr)
library(data.table)
library(tidyr)
library(stringr)

setwd("/workspace/home/gandriamiadana/mres_dissertation/")

# 20002
# clinical_codes <- readxl::read_excel("/workspace/home/gandriamiadana/mres_dissertation/clinical_codes.xlsx", sheet="Sheet1")

# df_20002 <- fread(file = "/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi/f.20002.tab", header = TRUE,  sep = "\t")
# coding_20002 <- fread("coding_for_20002.tsv") %>% select(coding, meaning)

# df_20002 <- fread(file = "/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi/f.6177.tab", header = TRUE,  sep = "\t")
# coding_20002 <- fread("coding-for-6177.tsv") %>% select(coding, meaning)

# df_20002 <- fread(file = "/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi/f.6153.tab", header = TRUE,  sep = "\t")
# coding_20002 <- fread("coding-for-6153.tsv") %>% select(coding, meaning)

# df_20002 <- fread(file = "/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi/f.6150.tab", header = TRUE,  sep = "\t")
# coding_20002 <- fread("coding-for-6150.tsv") %>% select(coding, meaning)

# all_f.eids_20002 <- df_20002 %>% select(f.eid) 

# datatbl_icd10 <- fread(file = "/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi/f.41270.tab", 
# header = TRUE,  sep = "\t")
# coding_icd10 <- fread("coding_icd10.tsv") %>% select(coding, meaning)
# all_f.eids_icd10 <- datatbl_icd10 %>% select(f.eid)

# date_records_icd10 <- fread("/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi//f.41280.tab") 

# codes_long_icd10 <- datatbl_icd10 %>%                    
#                melt(id.vars = "f.eid") %>%     # Convert wide format to long format (retain f.eid)
#               .[value != ""]                   # Filter out empty space (this is a data.table filter)


# datatbl_icd9 <- fread(file = "/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi/f.41203.tab", 
# header = TRUE,  sep = "\t")
# all_f.eids_icd9 <- datatbl_icd9 %>% select(f.eid)
# coding_icd9 <- fread("coding_icd9.tsv") %>% select(coding, meaning)   
# date_records_icd9 <- fread("/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi//f.41263.tab") 

datatbl_opcs4 <- fread(file = "/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi/f.41200.tab", 
header = TRUE,  sep = "\t")
all_f.eids_opcs4 <- datatbl_opcs4 %>% select(f.eid)
coding_opcs4 <- fread("coding_opcs4.tsv") %>% 
                select(coding, meaning)

date_records_opcs4 <- fread("/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi//f.41260.tab") 

                 # Filter out empty space (this is a data.table filter)

# codes_long_icd9 <- datatbl_icd9 %>%                    
#                melt(id.vars = "f.eid") %>%     # Convert wide format to long format (retain f.eid)
#               .[value != ""]                   # Filter out empty space (this is a data.table filter)


# mini_icd9 <- codes_long_icd9 
# mini_icd9 <- mini_icd9 %>% mutate(array_idx= stringr::str_sub(variable, 8)) %>%
#                 rename(coding= value) %>%
#                 select(-variable)

# mini_icd9 <- left_join(mini_icd9, coding_icd9)

# dates_long_icd9 <-  date_records_icd9 %>% 
#                    melt(id.vars = "f.eid") %>%                  # Convert wide format to long format
#                    .[!is.na(value)] %>%                         # filter out empty space
#                    mutate(array_idx= stringr::str_sub(variable, 8),      # extract array index for joining
#                                date= lubridate::ymd(value)) %>%            # convert text to date
#                    select(-variable)
# combined_icd9 <- inner_join(mini_icd9, dates_long_icd9); 

codes_long_opcs4 <- datatbl_opcs4 %>%                    
               melt(id.vars = "f.eid") %>%     # Convert wide format to long format (retain f.eid)
              .[value != ""]  

mini_opcs4 <- codes_long_opcs4 
mini_opcs4 <- mini_opcs4 %>% mutate(array_idx= stringr::str_sub(variable, 8)) %>%
                rename(coding= value) %>%
                select(-variable)

dates_long_opcs4 <-  date_records_opcs4 %>% 
                   melt(id.vars = "f.eid") %>%                  # Convert wide format to long format
                   .[!is.na(value)] %>%                         # filter out empty space
                   mutate(array_idx= stringr::str_sub(variable, 8),      # extract array index for joining
                               date= lubridate::ymd(value)) %>%            # convert text to date
                   select(-variable)

mini_opcs4 <- left_join(mini_opcs4, coding_opcs4)
combined_opcs4 <- inner_join(mini_opcs4, dates_long_opcs4); 

# mini_icd10 <- codes_long_icd10 # can I just get them all as one and filter in RStudio later? 
# mini_icd10 <- mini_icd10 %>% mutate(array_idx= stringr::str_sub(variable, 8)) %>%
#                 rename(coding= value) %>%
#                 select(-variable)

# mini_icd10 <- left_join(mini_icd10, coding_icd10)


# dates_long_icd10 <-  date_records_icd10 %>% 
#                    melt(id.vars = "f.eid") %>%                  # Convert wide format to long format
#                    .[!is.na(value)] %>%                         # filter out empty space
#                    mutate(array_idx= stringr::str_sub(variable, 8),      # extract array index for joining
#                                date= lubridate::ymd(value)) %>%            # convert text to date
                #    select(-variable)

# combined_icd10 <- inner_join(mini_icd10, dates_long_icd10); 

View(head(combined_opcs4))
# write.table(combined_icd10, file = "complete_icd10_table_as_long_Df.tsv", sep = "\t")
# write.table(combined_icd9, file = "complete_icd9_table_as_long_Df.tsv", sep = "\t")
# write.table(combined_opcs4, file = "complete_opcs4_table_as_long_Df.tsv", sep = "\t")


# codes_long <- df_20002 %>%                    
#                melt(id.vars = "f.eid") %>%     # Convert wide format to long format (retain f.eid)
#               .[value != ""]   

# mini <- codes_long %>% mutate(array_idx= stringr::str_sub(variable, 8)) %>%
#                 rename(coding= value) %>%
#                 select(-variable) 

# mini <- left_join(mini, coding_20002)

# Self-reported diagnosis 
# out_temp <- full_join(all_f.eids_20002, mini)
# View(head(out_temp, 20))

# write.table(out_temp, file = "long_df_selfreport_20002.tsv", sep = "\t")
# write.table(out_temp, file = "long_df_medications_6153.tsv", sep = "\t")
# write.table(out_temp, file = "long_df_diagnoses_6150.tsv", sep = "\t")
# write.table(out_temp, file = "long_df_medications_6177.tsv", sep = "\t")
