# COP 2018 request for completeness of Fine disaggs at PSNU level

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

rm(psnu_im)


# Grouping for Fine and Numerator for HTS_TST and HTS_TST_POS for completeness check
hts_tst <- psnu_imx %>% 
  # filter for HTS and HTS_TST_POS
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG")) %>% 
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
  group_by(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, period, indicator, colvar) %>%   summarize(value = sum(vals, na.rm=T)) %>% 
  mutate(modality="OVERALL")


# Known completeness vs known + unknown
uknown <- psnu_imx %>% 
  # filter for HTS and HTS_TST_POS
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG") &
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

age_k <- as.vector(age_bands$Var1[1:8])



uknown1 <- uknown %>% 
  # Create grouping variable for Fine and Total Numerator to calculate completeness
  mutate(uk = case_when(
    Age %in% age_k     ~ "Known_Age", 
    TRUE               ~ "Unknown_Age"
  )) %>% 
  # Keeping only Fine and Total Numerator rows 
  filter(!is.na(uk)) %>% 
  # Create column variable names
  mutate(colvar = uk) %>% 
  filter(!is.na(colvar))

uknown2 <- uknown %>% 
  # Create grouping variable for Fine and Total Numerator to calculate completeness
  mutate(uk = "Total_Age") %>% 
  # Keeping only Fine and Total Numerator rows 
  filter(!is.na(uk)) %>% 
  # Create column variable names
  mutate(colvar = uk) %>% 
  filter(!is.na(colvar))

# stacke 'em up
uknown_all <- bind_rows(uknown1, uknown2)


uknown_ag <- uknown_all %>%
  # selecting variables needed
  select(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, indicator, 
         modality,
         colvar, 
         FY2017Q1, FY2017Q2, FY2017Q3, FY2017Q4, FY2017APR) %>% 
  # Going long by period to be able to slice by period in completeness tool
  gather(period, vals, 9:13) %>% 
  # Aggregating data
  group_by(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, period, indicator, 
           modality,
           colvar) %>% 
  summarize(value = sum(vals, na.rm=T)) 


# Creating NOT IN function
`%ni%` <- Negate(`%in%`) 


# Male-female distribution of Unknown Age
gender <- psnu_imx %>% 
  # filter for HTS and HTS_TST_POS
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG") &
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
        Age %ni% age_k  ) %>% 
  # Changing names of some modalities
mutate(modality = gsub(" ", "", modality, fixed = TRUE))



gender1 <- gender %>% 
  # Create column variable names
  mutate(colvar = case_when(
    Sex == "Male"      ~ "Male_UnknownAge",
    Sex == "Female"    ~ "Female_UnknownAge",
    TRUE               ~ "Unknown_AgeSex"
  )) %>% 
  # remove rows without data for columns needed
  filter(!is.na(colvar))

View(table(gender1$colvar))


gender_ag <- gender1 %>%
  # selecting variables needed
  select(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, indicator, 
         modality,
         colvar,
         FY2017Q1, FY2017Q2, FY2017Q3, FY2017Q4, FY2017APR) %>% 
  # Going long by period to be able to slice by period in completeness tool
  gather(period, vals, 9:13) %>% 
  # Aggregating data
  group_by(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, period, indicator, 
           modality, colvar) %>% 
  summarize(value = sum(vals, na.rm=T)) 


#=======================================================================
#=======================================================================
# Analysis for the 25-49 Age band
#=======================================================================
#=======================================================================

# Known Sex completeness vs known + unknown Sex
adl_known <- psnu_imx %>% 
  # filter for HTS and HTS_TST_POS
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG") &
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
                          "OtherPITC")                &
          Age == "25-49"
  ) %>% 
  # Changing names of some modalities
  # Changing names of some modalities
mutate(modality = gsub(" ", "", modality, fixed = TRUE))
                         


adl_known1 <- adl_known %>% 
  # Create grouping variable for Fine and Total Numerator to calculate completeness
  mutate(ksex = case_when(
    Sex == "Male"     ~ "Male_25_49",
    Sex == "Female"   ~ "Female_25_49",
    TRUE              ~ "Unknown_sex_25_49"
  )) %>% 
  # Keeping only Fine and Total Numerator rows 
  filter(!is.na(ksex)) %>% 
  # Create column variable names
  mutate(colvar = ksex) %>% 
  filter(!is.na(colvar))




adl_known_ag <- adl_known1 %>%
  # selecting variables needed
  select(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, indicator, 
         modality, colvar,
         FY2017Q1, FY2017Q2, FY2017Q3, FY2017Q4, FY2017APR) %>% 
  # Going long by period to be able to slice by period in completeness tool
  gather(period, vals, 9:13) %>% 
  # Aggregating data
  group_by(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, period, indicator, 
           modality, colvar) %>% 
  summarize(value = sum(vals, na.rm=T)) 






#=======================================================================
#=======================================================================

tester <- bind_rows(hts_tst, uknown_ag, gender_ag, adl_known_ag) 

vars <- as.data.frame(table(tester$colvar))



# Creating overall long dataset
final <- bind_rows(hts_tst, uknown_ag, gender_ag, adl_known_ag) %>% 
  spread(colvar, value)



final1 <- final %>% 
  select(OperatingUnit:indicator, modality, 
         Fine, Num,
 Known_Age,
 Total_Age,
 Male_UnknownAge,
 Female_UnknownAge,
 Unknown_AgeSex,
 Male_25_49,
 Female_25_49,
 Unknown_sex_25_49
)


# Output folder location
out_put <- "C:/Mujawar/lrz5/AAA_Recent/COP2018/HTS_Males_req_2/out_put/"

# Output .txt file
write_tsv(final1, path = paste(out_put, "hts_tst_compl_20180224_v3.txt", sep=""), na="")



