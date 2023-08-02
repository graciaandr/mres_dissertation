merge_date_columns <- function(sample_df, df_tbl, status_col){
  
  t0<- df_tbl %>% gather(key, value, -c(f.eid, status_col)) %>% 
    mutate(date= str_sub(value, 1, 10) %>% ymd())
  
  print(head(t0))
  
  t1<- t0 %>% group_by(f.eid, across(status_col)) %>% summarise(minDate= min(date, na.rm=TRUE))
  t1$minDate <- ymd(t1$minDate)

  temp_df <- sample_df %>% select(f.eid, recruitment_date) 
  temp_df$recruitment_date <- ymd(temp_df$recruitment_date)
  temp_df
  
  t2 <- full_join(temp_df, t1)
  t3 <- t2 %>% mutate(prevalent_status = minDate <= recruitment_date,
                incident_status = minDate > recruitment_date)
  
  return(t3)

}