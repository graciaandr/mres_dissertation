setwd("/Users/BHCadmin/Documents/mres_gracia/")
icd10_table <- read.csv("complete_icd10_table_as_long_Df.tsv", sep = "\t")
# icd9_table <- read.csv("complete_icd9_table_as_long_Df.tsv", sep = "\t")
# opcs4_table <- read.csv("complete_opcs4_table_as_long_Df.tsv", sep = "\t")

ids_pericardial <- c("I300", "I301", "I308", "I309", "I310", "I311", "I312", 
                     "I313", "I318", "I319", "I320", "I321", "I328")
ids_pericardial2 <- sprintf(paste0(ids_pericardial, collapse = '|'))
# View(head(icd10_table))

mini_icd10 <- icd10_table %>% dplyr::filter(icd10_table$coding %like% ids_pericardial2)
head(mini_icd10)

t0_icd10 <- mini_icd10 %>%         ### table of pericardial patients, with columns of 
  group_by(f.eid) %>%               ### patient id, date of pericardial diagnosis and status
  summarise(minDate= min(date)) %>%
  mutate(status= 1)

all_f.eids <- read.csv("all_f-eids_icd10.tsv", sep = "\t") %>% select(f.eid)

pericardial_tbl <- full_join(all_f.eids, t0_icd10) %>%
  set_names(c("f.eid", "pericard_date", "pericard_status")) %>%
  mutate(pericard_status= ifelse(is.na(pericard_status), 0, pericard_status))

write.table(pericardial_tbl, file = "pericardial_patients_in_UKB.tsv", sep = "\t")
