# COP 2018 request for completeness of Fine disaggs at PSNU level, by Modality/SDP

# loading packages
library(tidyverse)
library(readr)
library(eply)

# Creating basic functions to show top few rows of data
View50 <- function(x){View(x[1:50,])}
View100 <- function(x){View(x[1:100,])}

# Loading RDS file

  setwd("C:/Mujawar/lrz5/AAA_Recent/COP2018/HTS_Males_req_2/New folder/FactView v3_2/")

  ou_im  <- readRDS("C:/Mujawar/lrz5/AAA_Recent/COP2018/HTS_Males_req_2/New folder/FactView v3_2/ICPI_FactView_OU_IM_20180209_v3_2.rds")

write_tsv(ou_im, path = paste(out_put, "OU_IM_v3_2.txt", sep=""), na="")


#  Reading in RDS files
psnu_imx  <- readRDS("C:/Mujawar/lrz5/AAA_Recent/COP2018/HTS_Males_req_2/New folder/FactView v3_2/ICPI_FactView_PSNU_IM_20180209_v3_2.rds")



fileloc <- "C:/Mujawar/lrz5/AAA_Recent/DataStore/Dec_22/"

# Loading PSNU-IM dataset 
psnu_im <- read_tsv(file=paste(fileloc, "ICPI_FactView_PSNU_IM_20171222_v2_2.txt", sep=""), 
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
                                     FY2018_TARGETS = "d"))

# Checking class of the variables
sapply(fact, class)


lowcase <- names(psnu_imx)
upcase <- names(psnu_im)

chex <- cbind(lowcase, upcase)

# Putting back original case-sensitive names from PSNU-IM dataset
names(psnu_imx) <- upcase


# Grouping for Fine and Numerator for HTS_TST and HTS_TST_POS for completeness check
hts_tst <- psnu_imx %>% 
  # filter for HTS and HTS_TST_POS
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>% 
  # Create grouping variable for Fine and Total Numerator to calculate completeness
  mutate(FN = case_when(
    standardizedDisaggregate %in% c("Modality/AgeAboveTen/Sex/Result",
                                    "Modality/AgeLessThanTen/Result", 
                                    "VMMC/Age/Result",
                                    "PMTCT ANC/Age/Result",
                                    "STI Clinic/Age/Sex/Result")              ~ "Fine",
    standardizedDisaggregate %in% c("Modality/Aggregated Age/Sex/Result") &
    disaggregate %in% c("Malnutrition/Result/<5",
                        "Pediatric/Result/<5")                                ~ "Fine",
    standardizedDisaggregate == "Total Numerator"                             ~ "Num"
  )) %>% 
  # Keeping only Fine and Total Numerator rows 
  filter(!is.na(FN)) %>% 
  # Create column variable names
  mutate(colvar = FN) %>% 
  # selecting variables needed
  select(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, indicator, colvar,
         FY2017Q1, FY2017Q2, FY2017Q3, FY2017Q4, FY2017APR) %>% 
  # Going long by period to be able to slice by period in completeness tool
  gather(period, vals, 8:12) %>% 
  # Aggregating data
  group_by(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, period, indicator, colvar) %>% 
  summarize(value = sum(vals, na.rm=T)) 


# Known completeness vs known + unknown
uknown <- psnu_imx %>% 
  # filter for HTS and HTS_TST_POS
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS") &
           # Only keeping fine data
        (
          standardizedDisaggregate %in% c("Modality/AgeAboveTen/Sex/Result",
                                          "Modality/AgeLessThanTen/Result", 
                                          "VMMC/Age/Result",
                                          "PMTCT ANC/Age/Result",
                                          "STI Clinic/Age/Sex/Result") |                
          (standardizedDisaggregate %in% c("Modality/Aggregated Age/Sex/Result") &
          disaggregate %in% c("Malnutrition/Result/<5",
                              "Pediatric/Result/<5"))                               
        )                                            &
         # The selected modalities
         modality %in% c( "IndexMod",
                          "MobileMod",
                          "VCTMod",
                          "OtherMod",
                          "Index",
                          "STI Clinic",
                          "Inpat",
                          "Emergency Ward",
                          "VCT",
                          "VMMC",
                          "TBClinic",
                          "PMTCT ANC",
                          "Pediatric",
                          "Malnutrition",
                          "OtherPITC")
  ) %>% 
  # Changing names of some modalities
  # Changing names of some modalities
mutate(modality = gsub(" ", "", modality, fixed = TRUE))
                         

# tabx <- psnu_imx %>% filter(modality %in% c("Pediatric", "Malnutrition")) 
# 
# View(table(tabx$modality, tabx$Age))

# View(table(uknown$Age, uknown$modality))
                         
                         
                         
                         
age_bands <- as.data.frame(table(uknown$Age))

age_k <- as.vector(age_bands$Var1[1:7])



uknown1 <- uknown %>% 
  # Create grouping variable for Fine and Total Numerator to calculate completeness
  mutate(uk = case_when(
    Age=="Unknown Age" ~ "U",
    Age %in% age_k     ~ "K"
  )) %>% 
  # Keeping only Fine and Total Numerator rows 
  filter(!is.na(uk)) %>% 
  # Create column variable names
  mutate(colvar = paste(modality, uk, sep="_")) %>% 
  filter(!is.na(colvar))

uknown2 <- uknown %>% 
  # Create grouping variable for Fine and Total Numerator to calculate completeness
  mutate(uk = case_when(
    Age=="Unknown Age" ~ "T",
    Age %in% age_k     ~ "T"
  )) %>% 
  # Keeping only Fine and Total Numerator rows 
  filter(!is.na(uk)) %>% 
  # Create column variable names
  mutate(colvar = paste(modality, uk, sep="_")) %>% 
  filter(!is.na(colvar))

# stacke 'em up
uknown_all <- bind_rows(uknown1, uknown2)


uknown_ag <- uknown_all %>%
  # selecting variables needed
  select(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, indicator, colvar,
         FY2017Q1, FY2017Q2, FY2017Q3, FY2017Q4, FY2017APR) %>% 
  # Going long by period to be able to slice by period in completeness tool
  gather(period, vals, 8:12) %>% 
  # Aggregating data
  group_by(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, period, indicator, colvar) %>% 
  summarize(value = sum(vals, na.rm=T)) 



# Male-female distribution of Unknown Age
gender <- psnu_imx %>% 
  # filter for HTS and HTS_TST_POS
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS") &
           # Only keeping fine data
        (
          standardizedDisaggregate %in% c("Modality/AgeAboveTen/Sex/Result",
                                          "Modality/AgeLessThanTen/Result", 
                                          "VMMC/Age/Result",
                                          "PMTCT ANC/Age/Result",
                                          "STI Clinic/Age/Sex/Result") |                
          (standardizedDisaggregate %in% c("Modality/Aggregated Age/Sex/Result") &
          disaggregate %in% c("Malnutrition/Result/<5",
                              "Pediatric/Result/<5"))                               
        )                                            &
         # The selected modalities
         modality %in% c( "IndexMod",
                          "MobileMod",
                          "VCTMod",
                          "OtherMod",
                          "Index",
                          "STI Clinic",
                          "Inpat",
                          "Emergency Ward",
                          "VCT",
                          "VMMC",
                          "TBClinic",
                          "PMTCT ANC",
                          "Pediatric",
                          "Malnutrition",
                          "OtherPITC")               &
        Age=="Unknown Age" & Sex %in% c("Male", "Female")  ) %>% 
  # Changing names of some modalities
mutate(modality = gsub(" ", "", modality, fixed = TRUE))



gender1 <- gender %>% 
  # Create column variable names
  mutate(colvar = paste(modality, Sex, sep="_")) %>% 
  # remove rows without data for columns needed
  filter(!is.na(colvar))

View(table(gender1$colvar))


gender2 <- gender %>% 
  # Create column variable names
  mutate(colvar = paste(modality, "Tot", sep="_")) %>% 
  # remove rows without data for columns needed
  filter(!is.na(colvar))

View(table(gender2$colvar))


gender_all <- bind_rows(gender1, gender2)


gender_ag <- gender_all %>%
  # selecting variables needed
  select(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, indicator, colvar,
         FY2017Q1, FY2017Q2, FY2017Q3, FY2017Q4, FY2017APR) %>% 
  # Going long by period to be able to slice by period in completeness tool
  gather(period, vals, 8:12) %>% 
  # Aggregating data
  group_by(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, period, indicator, colvar) %>% 
  summarize(value = sum(vals, na.rm=T)) 



# Creating overall long dataset
final <- bind_rows(hts_tst, uknown_ag, gender_ag) %>% 
  spread(colvar, value)

tester <- bind_rows(hts_tst, uknown_ag, gender_ag) 

vars <- as.data.frame(table(tester$colvar))

write.csv(vars, paste(out_put, "final_vars.csv", sep=""))


# Making sure all the variables are accounted for
dummy <- final[FALSE,]

dummy1 <- dummy %>% 
  mutate(
          Fine = NA,
          Num = NA,
          Index_K = NA,
          IndexMod_K = NA,
          Inpat_K = NA,
          MobileMod_K = NA,
          OtherMod_K = NA,
          OtherPITC_K = NA,
          VCT_K = NA,
          VCTMod_K = NA,
          Index_U = NA,
          IndexMod_U = NA,
          Inpat_U = NA,
          MobileMod_U = NA,
          OtherMod_U = NA,
          OtherPITC_U = NA,
          VCT_U = NA,
          VCTMod_U = NA,
          Index_T = NA,
          IndexMod_T = NA,
          Inpat_T = NA,
          MobileMod_T = NA,
          OtherMod_T = NA,
          OtherPITC_T = NA,
          VCT_T = NA,
          VCTMod_T = NA,
          Index_Male = NA,
          IndexMod_Male = NA,
          Inpat_Male = NA,
          MobileMod_Male = NA,
          OtherMod_Male = NA,
          OtherPITC_Male = NA,
          VCT_Male = NA,
          VCTMod_Male = NA,
          Index_Female = NA,
          IndexMod_Female = NA,
          Inpat_Female = NA,
          MobileMod_Female = NA,
          OtherMod_Female = NA,
          OtherPITC_Female = NA,
          VCT_Female = NA,
          VCTMod_Female = NA,
          Index_Tot = NA,
          IndexMod_Tot = NA,
          Inpat_Tot = NA,
          MobileMod_Tot = NA,
          OtherMod_Tot = NA,
          OtherPITC_Tot = NA,
          VCT_Tot = NA,
          VCTMod_Tot = NA,
          STIClinic_K = NA,
          STIClinic_U = NA,
          STIClinic_T = NA,
          STIClinic_Male = NA,
          STIClinic_Female = NA,
          STIClinic_Tot = NA,
          TBClinic_K = NA,
          TBClinic_U = NA,
          TBClinic_T = NA,
          TBClinic_Male = NA,
          TBClinic_Female = NA,
          TBClinic_Tot = NA,
          Malnutrition_U = NA,
          Malnutrition_K = NA,
          Malnutrition_T = NA,
          Pediatric_U = NA,
          Pediatric_K = NA,
          Pediatric_T = NA,
          PMTCTANC_K = NA,
          PMTCTANC_T = NA,
          PMTCTANC_U = NA, 
          VMMC_K = NA,
          VMMC_T = NA,
          VMMC_U = NA
)


final1 <- bind_rows(final, dummy1) %>% 
  select(OperatingUnit:indicator, Fine, Num,
          Index_K,
          Index_U,
          Index_T,
          Index_Male,
          Index_Female,
          Index_Tot,
          IndexMod_K,
          IndexMod_U,
          IndexMod_T,
          IndexMod_Male,
          IndexMod_Female,
          IndexMod_Tot,
          Inpat_K,
          Inpat_U,
          Inpat_T,
          Inpat_Male,
          Inpat_Female,
          Inpat_Tot,
          MobileMod_K,
          MobileMod_U,
          MobileMod_T,
          MobileMod_Male,
          MobileMod_Female,
          MobileMod_Tot,
          OtherMod_K,
          OtherMod_U,
          OtherMod_T,
          OtherMod_Male,
          OtherMod_Female,
          OtherMod_Tot,
          OtherPITC_K,
          OtherPITC_U,
          OtherPITC_T,
          OtherPITC_Male,
          OtherPITC_Female,
          OtherPITC_Tot,
          VCT_K,
          VCT_U,
          VCT_T,
          VCT_Male,
          VCT_Female,
          VCT_Tot,
          VCTMod_K,
          VCTMod_U,
          VCTMod_T,
          VCTMod_Male,
          VCTMod_Female,
          VCTMod_Tot, 
          STIClinic_K,
          STIClinic_U,
          STIClinic_T,
          STIClinic_Male,
          STIClinic_Female,
          STIClinic_Tot,
          TBClinic_K,
          TBClinic_U,
          TBClinic_T,
          TBClinic_Male,
          TBClinic_Female,
          TBClinic_Tot,
          Malnutrition_U,
          Malnutrition_K,
          Malnutrition_T,
          Pediatric_U,
          Pediatric_K,
          Pediatric_T,
          PMTCTANC_K,
          PMTCTANC_T,
          PMTCTANC_U,
          VMMC_K,
          VMMC_U,
          VMMC_T
)


# Output folder location
out_put <- "C:/Mujawar/lrz5/AAA_Recent/COP2018/HTS_Males_req_2/out_put/"

# Output .txt file
write_tsv(final1, path = paste(out_put, "hts_tst_compl_20180221_by_status_V3.txt", sep=""), na="")



