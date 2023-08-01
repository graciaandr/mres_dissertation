library(dplyr)
library(magrittr)
library(data.table)
library(tidyr)
library(stringr)

setwd("/workspace/home/gandriamiadana/mres_dissertation/")

clinical_codes <- readxl::read_excel("/workspace/home/gandriamiadana/mres_dissertation/clinical_codes.xlsx", sheet="Sheet1")

datatbl_icd10 <- fread(file = "/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi/f.41270.tab", 
header = TRUE,  sep = "\t")
coding_icd10 <- fread("coding_icd10.tsv") %>% select(coding, meaning)
all_f.eids_icd10 <- datatbl_icd10 %>% select(f.eid)

datatbl_icd9 <- fread(file = "/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi/f.41203.tab", 
header = TRUE,  sep = "\t")
all_f.eids_icd9 <- datatbl_icd9 %>% select(f.eid)
coding_icd9 <- fread("coding_icd9.tsv") %>% select(coding, meaning)   

datatbl_opcs4 <- fread(file = "/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi/f.41200.tab", 
header = TRUE,  sep = "\t")
all_f.eids_opcs4 <- datatbl_opcs4 %>% select(f.eid)
coding_opcs4 <- fread("coding_opcs4.tsv") %>% 
                select(coding, meaning)

date_records_icd10 <- fread("/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi//f.41280.tab") 
date_records_opcs4 <- fread("/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi//f.41260.tab") 
date_records_icd9 <- fread("/data/biobank/biobank_data_January2023_withdrawals_May2023/by_udi//f.41263.tab") 


codes_long_icd10 <- datatbl_icd10 %>%                    
               melt(id.vars = "f.eid") %>%     # Convert wide format to long format (retain f.eid)
              .[value != ""]                   # Filter out empty space (this is a data.table filter)

codes_long_opcs4 <- datatbl_opcs4 %>%                    
               melt(id.vars = "f.eid") %>%     # Convert wide format to long format (retain f.eid)
              .[value != ""]                   # Filter out empty space (this is a data.table filter)

codes_long_icd9 <- datatbl_icd9 %>%                    
               melt(id.vars = "f.eid") %>%     # Convert wide format to long format (retain f.eid)
              .[value != ""]                   # Filter out empty space (this is a data.table filter)

icds <- clinical_codes %>% filter(code_type == "ICD-10" | code_type == "ICD-9")
opcs4s <- clinical_codes %>% filter(code_type == "OPCS-4")

RT_icd_codes <- c("Y842", "Z081", "Z091", "Z510", "Z541", "V580", # ICD-9 and -10 codes that coded RT 
                "V661", "V6611", "V6612", "V6619", "V671")

bc_opcs4_codes = c("B27", "B271", "B272" , "B273", "B274", # OPCS-4 codes reg. Total excision of breast --> not Radiotherapy codes
                  "B275", "B276", "B278", "B279")
 
vector_clinical_codes_icds <- icds %>% filter(!icds$clinical_code %in% RT_icd_codes) ## remove ICD codes that do not code BC but RT 
vector_clinical_codes_opcs4 <- opcs4s %>% filter(opcs4s$clinical_code %in% bc_opcs4_codes) ## add opcs4 codes coding BC
BC_vector_clinical_codes <- c(vector_clinical_codes_icds$clinical_code, vector_clinical_codes_opcs4$clinical_code) # merge all BC codes
BC_updated_vector_clinical_codes <- sprintf(paste0(BC_vector_clinical_codes, collapse = '|'))
print(BC_updated_vector_clinical_codes)

mini_icd10 <- codes_long_icd10[value %like% BC_updated_vector_clinical_codes] 
mini_icd10 <- mini_icd10 %>% mutate(array_idx= stringr::str_sub(variable, 8)) %>%
                rename(coding= value) %>%
                select(-variable)

mini_icd9 <- codes_long_icd9[value %like% BC_updated_vector_clinical_codes] 
mini_icd9 <- mini_icd9 %>% mutate(array_idx= stringr::str_sub(variable, 8)) %>%
                rename(coding= value) %>%
                select(-variable)

mini_opcs4 <- codes_long_opcs4[value %like% BC_updated_vector_clinical_codes] 
mini_opcs4 <- mini_opcs4 %>% mutate(array_idx= stringr::str_sub(variable, 8)) %>%
                rename(coding= value) %>%
                select(-variable)

mini_icd10 <- left_join(mini_icd10, coding_icd10)
mini_icd9 <- left_join(mini_icd9, coding_icd9)
mini_opcs4 <- left_join(mini_opcs4, coding_opcs4)

# print(dim(mini_icd10)) 
# print(dim(mini_icd9))
# print(dim(mini_opcs4))


dates_long_icd10 <-  date_records_icd10 %>% 
                   melt(id.vars = "f.eid") %>%                  # Convert wide format to long format
                   .[!is.na(value)] %>%                         # filter out empty space
                   mutate(array_idx= stringr::str_sub(variable, 8),      # extract array index for joining
                               date= lubridate::ymd(value)) %>%            # convert text to date
                   select(-variable)


dates_long_icd9 <-  date_records_icd9 %>% 
                   melt(id.vars = "f.eid") %>%                  # Convert wide format to long format
                   .[!is.na(value)] %>%                         # filter out empty space
                   mutate(array_idx= stringr::str_sub(variable, 8),      # extract array index for joining
                               date= lubridate::ymd(value)) %>%            # convert text to date
                   select(-variable)


dates_long_opcs4 <-  date_records_opcs4 %>% 
                   melt(id.vars = "f.eid") %>%                  # Convert wide format to long format
                   .[!is.na(value)] %>%                         # filter out empty space
                   mutate(array_idx= stringr::str_sub(variable, 8),      # extract array index for joining
                               date= lubridate::ymd(value)) %>%            # convert text to date
                   select(-variable)

 
combined_icd10 <- inner_join(mini_icd10, dates_long_icd10); 
t0_icd10 <- combined_icd10 %>%                   ### table of all breast cancer patients, with columns of 
  group_by(f.eid) %>%               ### patient id, date of BC diagnosis and status (set to 1 since all are BC patients)
  summarise(minDate= min(date)) %>%
  mutate(status= 1)

out_icd10 <- full_join(all_f.eids_icd10, t0_icd10) %>%
      set_names(c("f.eid", "Breast_date", "Breast_status")) %>%
      mutate(Breast_status= ifelse(is.na(Breast_status), 0, Breast_status))

write.table(combined_icd10, file = "all_BC_patients_icd10.tsv", sep = "\t")
write.table(out_icd10, file = "BC_status_icd10.tsv", sep = "\t")

combined_icd9 <- inner_join(mini_icd9, dates_long_icd9); 
t0_icd9 <- combined_icd9 %>%                   ### table of all breast cancer patients, with columns of 
  group_by(f.eid) %>%               ### patient id, date of BC diagnosis and status (set to 1 since all are BC patients)
  summarise(minDate= min(date)) %>%
  mutate(status= 1)

out_icd9 <- full_join(all_f.eids_icd9, t0_icd9) %>%
      set_names(c("f.eid", "Breast_date", "Breast_status")) %>%
      mutate(Breast_status= ifelse(is.na(Breast_status), 0, Breast_status))

write.table(combined_icd9, file = "all_BC_patients_icd9.tsv", sep = "\t")
write.table(out_icd9, file = "BC_status_icd9.tsv", sep = "\t")


combined_opcs4 <- inner_join(mini_opcs4, dates_long_opcs4); 
t0_opcs4 <- combined_opcs4 %>%                   ### table of all breast cancer patients, with columns of 
  group_by(f.eid) %>%               ### patient id, date of BC diagnosis and status (set to 1 since all are BC patients)
  summarise(minDate= min(date)) %>%
  mutate(status= 1)

out_opcs4 <- full_join(all_f.eids_opcs4, t0_opcs4) %>%
      set_names(c("f.eid", "Breast_date", "Breast_status")) %>%
      mutate(Breast_status= ifelse(is.na(Breast_status), 0, Breast_status))

write.table(combined_opcs4, file = "all_BC_patients_opcs4.tsv", sep = "\t")
write.table(out_opcs4, file = "BC_status_opcs4.tsv", sep = "\t")

all_bcs_patients <- rbind(out_icd10, out_icd9, out_opcs4)
filt_all_bcs_patients <- all_bcs_patients %>% filter(Breast_status == 1)
dim(filt_all_bcs_patients)
length(unique(filt_all_bcs_patients$f.eid)) 

write.table(all_bcs_patients, file = "only_BC_patients_icd_and_opcs.tsv", sep = "\t")

### 22654 bzw laut python script 22896 including in situ
# now 25124
#
#

# ---------------------------------------------------------------------------------------
RT_vector_clinical_codes_icds <- icds %>% filter(icds$clinical_code %in% RT_icd_codes) ## remove ICD codes that do not code BC but RT 
RT_vector_clinical_codes_opcs4 <- opcs4s %>% filter(!opcs4s$clinical_code %in% bc_opcs4_codes)
# RT_vector_clinical_codes <- c(RT_vector_clinical_codes_icds$clinical_code, RT_vector_clinical_codes_opcs4$clinical_code)
RT_vector_clinical_codes <- c(RT_icd_codes, RT_vector_clinical_codes_opcs4$clinical_code)
RT_updated_vector_clinical_codes <- sprintf(paste0(RT_vector_clinical_codes, collapse = '|'))
print(RT_updated_vector_clinical_codes)


mini_icd10 <- codes_long_icd10[value %like% RT_updated_vector_clinical_codes] 
mini_icd10 <- mini_icd10 %>% mutate(array_idx= stringr::str_sub(variable, 8)) %>% # nolint
                rename(coding= value) %>%
                select(-variable)

mini_icd9 <- codes_long_icd9[value %like% RT_updated_vector_clinical_codes] 
mini_icd9 <- mini_icd9 %>% mutate(array_idx= stringr::str_sub(variable, 8)) %>%
                rename(coding= value) %>%
                select(-variable)

mini_opcs4 <- codes_long_opcs4[value %like% RT_updated_vector_clinical_codes] 
mini_opcs4 <- mini_opcs4 %>% mutate(array_idx= stringr::str_sub(variable, 8)) %>% # nolint
                rename(coding= value) %>%
                select(-variable)

mini_icd10 <- left_join(mini_icd10, coding_icd10)
mini_icd9 <- left_join(mini_icd9, coding_icd9)
mini_opcs4 <- left_join(mini_opcs4, coding_opcs4)

print(dim(mini_icd10)) 
print(dim(mini_icd9))
print(dim(mini_opcs4))

 
combined_icd10 <- inner_join(mini_icd10, dates_long_icd10) ## contains all RT sessions, including recurring for the same patient !!!

t0_icd10 <- combined_icd10 %>%                   ### table of all breast cancer patients, with columns of 
  group_by(f.eid) %>%               ### patient id, date of BC diagnosis and status (set to 1 since all are BC patients)
  summarise(minDate= min(date)) %>% ### find first date of a RT session
  mutate(status= 1)

out_icd10 <- full_join(all_f.eids_icd10, t0_icd10) %>%
      set_names(c("f.eid", "RT_date", "RT_status")) %>%
      mutate(RT_status= ifelse(is.na(RT_status), 0, RT_status))

write.table(combined_icd10, file = "all_RT_sessions_icd10.tsv", sep = "\t")
write.table(out_icd10, file = "RT_status_icd10.tsv", sep = "\t")

combined_opcs4 <- inner_join(mini_opcs4, dates_long_opcs4); 
t0_opcs4 <- combined_opcs4 %>%                   ### table of all breast cancer patients, with columns of 
  group_by(f.eid) %>%               ### patient id, date of BC diagnosis and status (set to 1 since all are BC patients)
  summarise(minDate= min(date)) %>%
  mutate(status= 1)

out_opcs4 <- full_join(all_f.eids_opcs4, t0_opcs4) %>%
      set_names(c("f.eid", "RT_date", "RT_status")) %>%
      mutate(RT_status= ifelse(is.na(RT_status), 0, RT_status))

write.table(combined_opcs4, file = "all_RT_sessions_opcs4.tsv", sep = "\t")
write.table(out_opcs4, file = "RT_status_opcs4.tsv", sep = "\t")

all_RT_patients <- rbind(out_icd10, out_opcs4)
filt_all_RT_patients <- all_RT_patients %>% filter(RT_status == 1)
dim(filt_all_RT_patients)
length(unique(filt_all_RT_patients$f.eid)) 

write.table(all_RT_patients, file = "only_RT_patients_icd_and_opcs.tsv", sep = "\t")

### 22654 bzw laut python script 22896 including in situ
# now 25124