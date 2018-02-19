##   ICPI COP FY18
##   A.Chafetz, USAID
##   Project: Echo
##   Purpose: generate dataset for HTS Male Helper Tool
##   Date: Feb 19, 2018
##   Updated: 


#dependencies
  pacman::p_load(tidyverse, knitr)

#load FV PSNU dataset v3.2
df_mer <- read_rds(Sys.glob("~/ICPI/Data/ICPI_FactView_PSNU_2*.Rds"))

#filter to just HTS and aggregate
  df_hts <- df_mer %>% 
    filter(indicator %in% c("HTS_TST"), 
           standardizeddisaggregate %in% c("Modality/AgeAboveTen/Sex/Result", "Modality/AgeLessThanTen/Result", "PMTCT ANC/Age/Result", "VMMC/Age/Result")) %>% 
    group_by(operatingunit, psnu, psnuuid, indicator, age, sex, resultstatus, modality) %>% 
    summarise_at(vars(fy2017apr), ~ sum(., na.rm = TRUE)) %>% 
    ungroup ()

#reshape wide & create a total variable (neg + pos)
  df_hts <- df_hts %>% 
    spread(resultstatus, fy2017apr) %>% 
    rename_all(tolower) %>% 
    mutate_at(vars(negative, positive), ~ ifelse(is.na(.), 0, .)) %>% 
    mutate(total = negative + positive) %>% 
    filter(total !=0) %>% 
    select(-negative)

#seperate modalities/age/sex not in DP,
  df_oth <- df_hts %>% 
    filter(age=="Unknown Age" | is.na(sex) | (modality %in% c("VMMC", "PMTCT ANC", "TBClinic")))

  df_hts <- df_hts %>% 
    filter(age!="Unknown Age",  !is.na(sex), (!modality %in% c("VMMC", "PMTCT ANC", "TBClinic")))

#create psnu/modality groupings' total and share 
  df_hts <- df_hts %>% 
    group_by(operatingunit, psnu, psnuuid, indicator, modality) %>% 
    mutate(postitive_group = sum(positive),
           total_group = sum(total)) %>% 
    ungroup() %>% 
    arrange(operatingunit, psnu, modality, sex) %>%
    mutate(pos_group_share = round(positive / postitive_group, 5),
           tot_group_share = round(total / total_group, 5))

#append extra other data
  df_hts <- bind_rows(df_hts, df_oth)
    rm(df_oth)  
#export  
  write_csv(df_hts, "~/tmp/echo_data.csv")
  
