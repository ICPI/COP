# COP 2018 request for completeness of Fine disaggs at PSNU level


fileloc <- "C:/Mujawar/lrz5/AAA_Recent/DataStore/Dec_22/"

# Loading PSNU-IM dataset 
psnu_im <- read_tsv(file=paste(fileloc, "ICPI_FactView_PSNU_IM_20171222_v2_2.txt", sep=""), 
                    col_types = cols(MechanismID = "c",
                                     FY2015Q2 = "d",      
                                     FY2015Q3 = "d",      
                                     FY2015Q4 = "d",      
               (                      FY2015APR = "d",     
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

# Grouping for Fine and Numerator for HTS_TST and HTS_TST_POS
hts_tst <- psnu_im %>% 
  # filter for HTS and HTS_TST_POS
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>% 
  # Create grouping variable for Fine and Total Numerator to calculate completeness
  mutate(FN = case_when(
    standardizedDisaggregate %in% c("Modality/AgeAboveTen/Sex/Result",
                                    "Modality/AgeLessThanTen/Result")         ~ "F",
    standardizedDisaggregate %in% c("Modality/Aggregated Age/Sex/Result") &
    disaggregate %in% c("Malnutrition/Result/<5",
                        "Pediatric/Result/<5")                                ~ "F",
    standardizedDisaggregate == "Total Numerator"                             ~ "N"
  )) %>% 
  # Keeping only Fine and Total Numerator rows 
  filter(!is.na(FN)) %>% 
  # Create column variable names
  mutate(colvar = paste(indicator, FN, sep="_")) %>% 
  # selecting variables needed
  select(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, colvar,
         FY2017Q1, FY2017Q2, FY2017Q3, FY2017Q4, FY2017APR) %>% 
  # Going long by period to be able to slice by period in completeness tool
  gather(period, vals, 7:11) %>% 
  # Aggregating data
  group_by(OperatingUnit, PSNU, PSNUuid, MechanismID, ImplementingMechanismName, period, colvar) %>% 
  summarize(value = sum(vals, na.rm=T)) %>% 
  # Going wide
  spread(colvar, value)


# Output folder location
out_put <- "C:/Mujawar/lrz5/AAA_Recent/COP2018/HTS_Males_req_2/out_put/"

# Output .txt file
write_tsv(hts_tst, path = paste(out_put, "hts_tst_completeness_20180220_v2.txt", sep=""), na="")



