# COP 2018 request for Botswana site-level TX trends

# loading packages
library(tidyverse)
library(readr)
library(eply)

# Creating basic functions to show top few rows of data
View50 <- function(x){View(x[1:50,])}
View100 <- function(x){View(x[1:100,])}



fileloc <- "C:/Mujawar/lrz5/AAA_Recent/COP2018/Botswana_TX_trend/"

# Loading PSNU-IM dataset 
site_im <- read_tsv(file=paste(fileloc, "ICPI_FactView_Site_IM_Botswana_20180215_v1_2.txt", sep=""), 
                    col_types = cols(MechanismID = "c",
                                     FY2015Q2 = "d",      
                                     FY2015Q3 = "d",      
                                     FY2015Q4 = "d",      
                                     FY2015APR = "d",     
                                     FY2016_TARGETS = "d",
                                     FY2016Q1 = "d",      
                                     FY2016Q2 = "d",      
                                     FY2016Q3 = "d",      
                                     FY2016Q4 = "d",      
                                     FY2016APR = "d",     
                                     FY2017_TARGETS = "d",
                                     FY2017Q1 = "d",      
                                     FY2017Q2 = "d",      
                                     FY2017Q3 = "d",      
                                     FY2017Q4 = "d",      
                                     FY2017APR = "d",
                                     FY2018_TARGETS = "d",
                                     FY2018Q1 = "d"))

# Checking class of the variables
sapply(site_im, class)



tx <- site_im %>% 
  # filter for HTS and HTS_TST_POS
  filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_RET")) 


tx2 <- tx %>% 
  # Create grouping variable for Fine, MCAD, and Total Numerator 
  mutate(F_C = case_when(
      standardizedDisaggregate %in% c("Age/Sex/HIVStatus",
                                      "Age/Sex")                     ~ "Fine",
      standardizedDisaggregate %in% c("MostCompleteAgeDisagg",
                                      "Age Aggregated/Sex")          ~ "MCAD/Coarse",
      standardizedDisaggregate %in% c("Total Numerator",        
                                      "Total Denominator")           ~ "NumDenom")  ) %>% 
    filter(!is.na(F_C)) %>% 
# Creating site_name, site_id and site_type variables  ========== 
  mutate(
  site_type = 
  case_when(
    orgUnitUID==FacilityUID   ~ "Facility",
    orgUnitUID==CommunityUID  ~"Community",
    typeMilitary=="Y"         ~"Military",
    TRUE                      ~ "") ) 


  
# Creating the site name variable
tx2$site_name = with(tx2, 
    ifelse(site_type=="Facility", Facility,
    ifelse(site_type=="Community", Community,
    ifelse(site_type=="Military", PSNU, ""))))


tx3 <- tx2 %>% 
  select(orgUnitUID                    ,
         OperatingUnit                 ,
         SNU1                          ,
         SNU1Uid                       ,
         PSNU                          ,
         PSNUuid                       ,
         CurrentSNUPrioritization      ,
         typeMilitary                  ,
         mechanismUID                  ,
         PrimePartner                  ,
         FundingAgency                 ,
         MechanismID                   ,
         ImplementingMechanismName     ,
         Community                     ,
         CurrentCommunityPrioritization,
         Facility                      ,
         CurrentFacilityPrioritization ,
         indicator                     ,
         numeratorDenom                ,
         indicatorType                 ,  
         F_C                           ,
         Age                           ,
         Sex                           ,
         resultStatus                  ,
         tieredSiteCounts              ,
         typeTieredSupport             ,
         site_name                     ,
         site_type                     ,
         FY2015Q2                      ,
         FY2015Q3                      ,
         FY2015Q4                      ,
         FY2015APR                     ,
         FY2016_TARGETS                ,
         FY2016Q1                      ,
         FY2016Q2                      ,
         FY2016Q3                      ,
         FY2016Q4                      ,
         FY2016APR                     ,
         FY2017_TARGETS                ,
         FY2017Q1                      ,
         FY2017Q2                      ,
         FY2017Q3                      ,
         FY2017Q4                      ,
         FY2017APR                     ,
         FY2018_TARGETS                ,
         FY2018Q1           
) %>%
  gather(period, vals, 29:46) %>% 
  mutate(colvar = case_when(
    indicator == "TX_RET"     ~ paste(numeratorDenom, period, sep="_"),
    TRUE                      ~ paste(period, sep="_") 
  )) %>% 
  group_by(orgUnitUID                    ,
         OperatingUnit                 ,
         SNU1                          ,
         SNU1Uid                       ,
         PSNU                          ,
         PSNUuid                       ,
         CurrentSNUPrioritization      ,
         typeMilitary                  ,
         mechanismUID                  ,
         PrimePartner                  ,
         FundingAgency                 ,
         MechanismID                   ,
         ImplementingMechanismName     ,
         Community                     ,
         CurrentCommunityPrioritization,
         Facility                      ,
         CurrentFacilityPrioritization ,
         indicatorType                 ,  
         F_C                           ,
         Age                           ,
         Sex                           ,
         resultStatus                  ,
         tieredSiteCounts              ,
         typeTieredSupport             ,
         site_name                     ,
         site_type                     ,
         indicator,
         colvar) %>% 
  summarize(values = sum(vals, na.rm=T)) %>% 
  spread(colvar, values)
  

  



# Output folder location
out_put <- "C:/Mujawar/lrz5/AAA_Recent/COP2018/Botswana_TX_trend/Output/"

# Output .txt file
write_tsv(tx3, path = paste(out_put, "Botswana_tx_trend_long.txt", sep=""), na="")



