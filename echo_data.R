##   ICPI COP FY18
##   A.Chafetz, USAID
##   Project: Echo
##   Purpose: generate dataset for HTS Male Helper Tool
##   Date: 2018.02.19
##   Updated: 2018.02.20


#dependencies
  pacman::p_load(tidyverse, knitr, RCurl)
  
## FUNCTIONS FROM DATA PACK --------------------------------------------------------------------------
#92_datapack_snu_adj.R - https://github.com/achafetz/DataPack/blob/master/Scripts/92_datapack_snu_adj.R
  
  # COMBINE/DELETE SNUS
  
  cleanup_snus <- function(df) {
    
    #table of dup PSNUs(psnuuid) & their replacments (psnuuid_adj)
    df_adj <- tribble(
      ~psnuuid,	    ~psnuuid_adj,
      "Z6b0Advh1f8",    "qPyHEwO7X6D",
      "EzsXkY9WARj",    "URj9zYi533e",
      "KN2TmcAVqzi",    "bDoKaxNx2Xb",
      "kxsmKGMZ5QF",    "mVuyipSx9aU",
      "FjiNyXde6Ae",    "xmRjV3Gx1H6",
      "J4yYjIqL7mG",    "oygNEfySnMl"
    )
    
    #replace duplicate UIDs so only one per PSNU
    df <- df %>%
      left_join(df_adj, by = "psnuuid") %>%
      mutate(psnuuid = ifelse(is.na(psnuuid_adj), psnuuid, psnuuid_adj)) %>%
      select(-psnuuid_adj) %>%
      
      #replace PNSU ek Ikere-Ekiti with ek Ikere
      mutate(psnu = ifelse(psnuuid=="KT3e5pmPdfB","ek Ikere", psnu)) %>%
      
      #remove all duplicates/blank PSNUs
      filter(!psnuuid %in% c("dOQ8r7iwZvS", "HHDEeZbVEaw", "IxeWi5YG9lE", "KT3e5pmPdfB", "h61xiVptz4A", 
                             "lC1wneS1GR5", "D47MUIzTapM", "RVzTHBO9fgR")) %>%
      
      #add country name to regional programs
      mutate(psnu = ifelse((operatingunit %in% 
                              c("Asia Regional Program", "Caribbean Region", "Central America Region", "Central Asia Region")), 
                           paste(snu1, psnu, sep = "/"), psnu)) %>%
      
      ## REMOVE SNUs ##
      #S.Ally (1/17/17) - no Sustained - Commodities districts 
      filter(!psnuuid %in% c("O1kvkveo6Kt", "hbnRmYRVabV", "N7L1LQMsQKd", "nlS6OMUb6s3")) %>%
      
      ## SNU NAMING ISSUES ##
      # M. Melchior (1/21/17) - txt import issue with French names 
      mutate( psnu = ifelse(psnuuid == "JVXPyu8T2fO", "Cap-Haïtien", psnu), 
              psnu = ifelse(psnuuid == "XXuTiMjae3r", "Anse à Veau", psnu),
              psnu = ifelse(psnuuid == "prA0IseYHWD", "Fort Liberté", psnu),
              psnu = ifelse(psnuuid == "xBsmGxPgQaw", "Gonaïves", psnu),
              psnu = ifelse(psnuuid == "fXIAya9MTsp", "Grande Rivière du Nord", psnu),
              psnu = ifelse(psnuuid == "lqOb8ytz3VU", "Jérémie", psnu),
              psnu = ifelse(psnuuid == "aIbf3wlRYB1", "La Gonave", psnu),
              psnu = ifelse(psnuuid == "nbvAsGLaXdk", "Léogâne", psnu),
              psnu = ifelse(psnuuid == "rrAWd6oORtj", "Limbé", psnu),
              psnu = ifelse(psnuuid == "nbvAsGLaXdk", "Léogâne", psnu),
              psnu = ifelse(psnuuid == "c0oeZEJ8qXk", "Môle Saint Nicolas", psnu),
              psnu = ifelse(psnuuid == "Y0udgSlBzfb", "Miragoâne", psnu),
              psnu = ifelse(psnuuid == "R2NsUDhdF8x", "Saint-Raphaël", psnu),
              psnu = ifelse(psnuuid == "mLFKTGjlEg1", "Chardonniàres", psnu),
              psnu = ifelse((psnuuid %in% c("ONUWhpgEbVk", "RVzTHBO9fgR")), "Vallières", psnu)
      ) %>% 
      
      #replace the 2 _unallocated cities in Mozambique with the PSNUs they fall within
      #C.Hill (2/15/2018)
      mutate(psnu = ifelse(psnuuid == "uMXJsbSbXBS", "Xai-Xai", psnu),
             psnuuid = ifelse(psnuuid == "uMXJsbSbXBS", "kKXWgaF11TT", psnuuid),
             currentsnuprioritization = ifelse(psnuuid == "uMXJsbSbXBS", "1 - Scale-Up: Saturation", currentsnuprioritization),
             psnu = ifelse(psnuuid == "nOE2NPIf8vq", "Nampula", psnu),
             psnuuid = ifelse(psnuuid == "nOE2NPIf8vq", "jwLNNIw1MjY", psnuuid),
             currentsnuprioritization = ifelse(psnuuid == "nOE2NPIf8vq", "2 - Scale-Up: Aggressive", currentsnuprioritization))
    
    #fix issue of snus having mutliple prioritizations over time --> use FY17Q4 value, otherwise take NA
       df_priority <- df %>% 
        #remove missing values (only keeping what is available in Q4)
        filter(!is.na(fy2017q4), fy2017q4!= 0, !is.na(psnuuid), !is.na(currentsnuprioritization)) %>%
        #keep unique values of psnus and their prioritization
        distinct(psnuuid, currentsnuprioritization)
       
    #remove problematic prioritization
    df <- df %>% 
      select(-currentsnuprioritization)
    
    #merge q4 priority in and reorder
    df <- left_join(df, df_priority) %>% 
      select(region:psnuuid, currentsnuprioritization, everything())
  }
    
  
  # Cluster SNUs 
  # clusters submitted by SI advisors - https://github.com/achafetz/ICPI/tree/master/DataPack/RawData
  # only for psnu and psnu x im datasets, not site (orgunituid should not exist in PSNU or PSNU IM dataset) 
  
  cluster_snus <- function(df){
    # import cluster dataset
    gh <- getURL("https://raw.githubusercontent.com/achafetz/DataPack/master/RawData/COP18Clusters.csv")
    df_cluster <- read.csv(text = gh)

    # remove duplicate data/headers
    df_cluster <- select(df_cluster, -operatingunit, -psnu, -currentsnuprioritization, -cluster_set:-cluster_date)
    
    # merge clusters onto factview
    df <- left_join(df, df_cluster, by = "psnuuid")
    
    # replace with cluster info
    df <- df %>%
      mutate(
        psnu = ifelse(is.na(cluster_psnu), psnu, cluster_psnu),
        snu1 = ifelse(is.na(cluster_snu1), snu1, cluster_snu1),
        psnuuid = ifelse(is.na(cluster_psnuuid), psnuuid, cluster_psnuuid),
        currentsnuprioritization = ifelse(is.na(cluster_currentsnuprioritization), currentsnuprioritization, cluster_currentsnuprioritization)
      ) %>%
      select(-cluster_psnu:-cluster_currentsnuprioritization)
    
  } 

## ECHO HTS DATASET -------------------------------------------------------------------------------------------------------------------- 
  
#load FV PSNU dataset v3.2
df_mer <- read_rds(Sys.glob("~/ICPI/Data/ICPI_FactView_PSNU_2*.Rds"))

#filter to just HTS and aggregate
  df_hts <- df_mer %>% 
    filter(indicator %in% c("HTS_TST"), 
           standardizeddisaggregate %in% c("Modality/AgeAboveTen/Sex/Result", "Modality/AgeLessThanTen/Result", "PMTCT ANC/Age/Result", "VMMC/Age/Result", "Malnutrition/Age/Sex/Result", "Pediatric/Age/Sex/Result")) 
#clean up
  df_hts <- cleanup_snus(df_hts)
  df_hts <- cluster_snus(df_hts)
    rm(cleanup_snus, cluster_snus)
  
  #aggregate
  df_hts  <- df_hts  %>% 
    group_by(operatingunit, psnu, psnuuid, indicator, age, sex, resultstatus, modality) %>% 
    summarise_at(vars(fy2017apr), ~ sum(., na.rm = TRUE)) %>% 
    ungroup()


#reshape wide & create a total variable (neg + pos)
  df_hts2 <- df_hts %>% 
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
  write_csv(df_hts, "~/tmp/echo_data.csv", na = "")
