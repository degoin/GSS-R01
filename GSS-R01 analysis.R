
library(tidyverse)
# this is the data that Dimitri has cleaned 
# the rows are the chemical features and the columns are the samples 
df_all <- read.csv("/Users/danagoin/Documents/R01 GSS New Methods/Data/R01_150_clean_dataset.csv")

# the values are unitless, but represents the abundance, which is the integration / the area under the curve for plots of intensity versus time 
#  the retention time is the time that the chemicals appear in the chromatograph 
# DF means the detection frequency of the samples 
# the Score has to do with the quality of the deconvoluion of the peaks--only peaks with scores above 60 or 70 are usually kept 




# create chemical id, which is the combination of the chemical formula and the retention time 
df_all$chem_id <- paste0(df_all$Formula,"_", df_all$Retention.Time)

# save variables related to chemical formulas if we need them 
df_chem <- df_all %>% select(chem_id, Formula, Retention.Time, Ionization.mode, Mass, Score, Compound)

# remove chemical variables for transposing data
df_all <- df_all %>% select(chem_id, everything(), -X, -Compound, -Formula, -Retention.Time, -Ionization.mode, -Mass, -Score)
rownames(df_all) <- df_all$chem_id
df_all <- df_all %>% select(-chem_id)

# transpose data and create ids for sample, ppt_id to merge with covariates, and indicator of whether it's a maternal or cord blood sample 
df_t <- data.frame(t(df_all))
df_t$sample_id <- rownames(df_t)
df_t$sample_type <- substr(df_t$sample_id,1,1)
df_t$ppt_id <- gsub("[^0-9]","",df_t$sample_id)

# sort by ppt id and sample type 
df_t <- df_t %>% arrange(ppt_id, sample_type)
df_t <- df_t %>% select(sample_id, ppt_id, sample_type, everything())
df_t$ppt_id <- as.numeric(df_t$ppt_id)

# next merge on demographics and test if distributions are different across them 
df <- read.csv("/Users/danagoin/Documents/CiOB-ECHO/CiOB2/questionnaire.csv") 

# recode key variables  

df$mat_edu <- ifelse(df$edu_m>=97, NA, df$edu_m)                   
df$mat_edu <- factor(df$mat_edu, levels=c(0,1,2,3,4,5), 
                     labels=c("Less than high school","High school grad","Some college","College grad","Master's degree","Doctoral degree"))


df$mat_race <- ifelse(df$race_m>=97,NA, df$race_m)
#df$mat_race <- factor(df$mat_race, levels=c(1,2,3,4,5,6), 
#                      labels=c("Asian","Pacific Islander","Black","White","Native American","Other"))

df$mat_eth <- ifelse(df$latina_m>=97,NA, df$latina_m)

df$mat_race_eth <- ifelse(df$mat_eth==1, 1, 
                          ifelse(df$mat_eth==0 & (df$mat_race==1 | df$mat_race==2), 2, 
                                 ifelse(df$mat_eth==0 & df$mat_race==3, 3, 
                                        ifelse(df$mat_eth==0 & df$mat_race==4, 4, 
                                               ifelse(df$mat_eth==0 & (df$mat_race==5 | df$mat_race==6), 5, NA)))))

df$mat_race_eth <- factor(df$mat_race_eth, levels=c(1,2,3,4,5), 
                          labels=c("Latina","Asian/PI", "Black","White","Other or multiple"))


df$marital <- ifelse(df$marital_stat>=97,NA, df$marital_stat)

df$marital <- ifelse(df$marital==4, 3, df$marital)
df$marital <- factor(df$marital, levels=c(1,3,5), labels=c("Married","Widowed, separated, or divorced","Never married"))

df$medi_cal <- ifelse(df$medi_cal_m>=97,NA, df$medi_cal_m)

# if Latina, what is her country of origin? 
# this corresponds to latino_coo in the codebook
df$latina_coo <- ifelse(df$country_m==1, 1, 
                      ifelse(df$country_m==2, 2, 
                             ifelse(df$country_m==3, 3, NA)))

df$latina_coo <- factor(df$latina_coo, levels=c(1,2,3), labels=c("Mexico","El Salvador","Other"))


# create income categories 

df$income_hh <- ifelse(df$income_hh %in% c(97,98,99), NA, df$income_hh)
df$income_20k <- ifelse(df$income_20k %in% c(97,98,99), NA, df$income_20k)
df$income_40k <- ifelse(df$income_40k %in% c(97,98,99), NA, df$income_40k)

df$hh_income_cat1 <- ifelse(df$income_hh==1,1, 
                           ifelse(df$income_hh==2,1,
                                  ifelse(df$income_hh==3, 1, 
                                         ifelse(df$income_hh==4,1,
                                                ifelse(df$income_hh==5,1, 
                                                       ifelse(df$income_hh==6,1, 
                                                              ifelse(df$income_hh==7,1,
                                                                     ifelse(df$income_hh==8,1, 0))))))))

df$hh_income_cat1 <-  ifelse(((df$income_20k==0 & !is.na(df$income_20k)) | (df$income_40k==0 & !is.na(df$income_40k))), 1, df$hh_income_cat1)



df$hh_income_cat2 <- ifelse(df$income_hh==9,2, 
                            ifelse(df$income_hh==10,2, 
                                   ifelse(df$income_hh==11,2, 
                                          ifelse(df$income_hh==12,2, 
                                                 ifelse(df$income_hh==13,2,
                                                        ifelse(df$income_hh==14,2,
                                                               ifelse(df$income_hh==15,2, 0)))))))

df$hh_income_cat2 <- ifelse((df$income_80k==0 & !is.na(df$income_80k)), 2, df$hh_income_cat2)


df$hh_income_cat3 <-  ifelse(df$income_hh==16,3, 
                              ifelse(df$income_hh==17,3, 
                                     ifelse(df$income_hh==18,3,
                                            ifelse(df$income_hh==19,3,
                                                   ifelse(df$income_hh==20,3,0)))))

 df$hh_income_cat <- ifelse(df$hh_income_cat1==1,1, 
                            ifelse(df$hh_income_cat2==2, 2, 
                                   ifelse(df$hh_income_cat3==3, 3, NA)))
 
# merge with chemical data 
df_m <- left_join(df_t, df)


# just keep chemicals and one demographic factor at the moment 





