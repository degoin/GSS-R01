
library(tidyverse)
# this is the data that Dimitri has cleaned 
# the rows are the chemical features and the columns are the samples 
df_all <- read.csv("/Users/danagoin/Documents/R01 GSS New Methods/Data/R01_150_clean_dataset.csv")

# create chemical id, which is the combination of the chemical formula and the retention time 
df_all$chem_id <- paste0(df_all$Formula,"_", df_all$Retention.Time)

# save variables related to chemical formulas if we need them 
df_chem <- df_all %>% select(chem_id, Formula, Retention.Time, Ionization.mode, Mass, Score, Compound)

# remove chemical variables for transposing data
df_all <- df_all %>% select(chem_id, everything(), -X, -Compound, -Formula, -Retention.Time, -Ionization.mode, -Mass, -Score)
rownames(df_all) <- df_all$chem_id
df_all <- df_all %>% select(-chem_id)

df_t <- data.frame(t(df_all))
df_t$sample_id <- rownames(df_t)
df_t$sample_type <- substr(df_t$sample_id,1,1)
df_t$ppt_id <- gsub("[^0-9]","",df_t$sample_id)

df_t <- df_t %>% arrange(ppt_id, sample_type)
df_t <- df_t %>% select(sample_id, ppt_id, sample_type, everything())

# the values are unitless, but represents the abundance, which is the integration / the area under the curve for plots of intensity versus time 
#  the retention time is the time that the chemicals appear in the chromatograph 
# DF means the detection frequency of the samples 
# the Score has to do with the quality of the deconvoluion of the peaks--only peaks with scores above 60 or 70 are usually kept 



