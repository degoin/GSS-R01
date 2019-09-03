rm(list=ls())


library(tidyverse)
# this is the data that Dimitri has cleaned 
# the rows are the chemical features and the columns are the samples 
df_all_is <- read.csv("/Users/danagoin/Documents/Research projects/CiOB-ECHO/R01 GSS New Methods/data/R01_150_isomeric_clean_dataset_2.0.csv")
df_all_un <- read.csv("/Users/danagoin/Documents/Research projects/CiOB-ECHO/R01 GSS New Methods/data/R01_150_clean_dataset.csv")

df_all <- data.frame(rbind(df_all_is, df_all_un))
# add unique chemicals too 
# add cleaning and dietary habits to demographics 
# compare white versus other race/ethnic groups


# the values are unitless, but represents the abundance, which is the integration / the area under the curve for plots of intensity versus time 
#  the retention time is the time that the chemicals appear in the chromatograph 
# DF means the detection frequency of the samples 
# the Score has to do with the quality of the deconvoluion of the peaks--only peaks with scores above 60 or 70 are usually kept 


# create chemical id, which is the combination of the chemical formula and the retention time 
df_all$chem_id <- paste0(df_all$Formula,"_", df_all$Retention.Time)
df_all$chem_id <- ifelse(df_all$chem_id=="TPP D15_4.40746262", "TPP_D15_4.40746262",
                         ifelse(df_all$chem_id=="DL-Cotinine D3_3.41909841", "DL_Cotinine_D3_3.41909841", df_all$chem_id))

# save variables related to chemical formulas if we need them 
df_chem <- df_all %>% select(chem_id, Formula, Retention.Time, Ionization.mode, Mass, Score, Compound)

# remove chemical variables for transposing data
df_all <- df_all %>% select(chem_id, everything(), -X, -Compound, -Formula, -Retention.Time, -Ionization.mode, -Mass, -Score)
chems <- rownames(df_all) <- df_all$chem_id
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
df <- read.csv("/Users/danagoin/Documents/Research projects/CiOB-ECHO/CiOB2 data/questionnaire.csv") 

# recode and/or create key variables  

# calculated maternal age 
#df$visit_date <- strptime(df$scndtri_dt, format="%Y-%m-%d")
#df$mb_date <- strptime(df$dob_m, format="%Y-%m-%d")

#df$mage <- as.numeric(difftime(df$visit_date, df$mb_date, units="days")/365)
# one person's visit date is their bday, not sure why but it's causing their age to be 0 so I'm assuming it's a mistake 
#df$mage <- ifelse(df$mage==0 & !is.na(df$mage), NA, df$mage)


# maternal education 
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


# vitamins 
df$vitamin_cat <- factor(df$vitamins, levels=c(0,1,2,3), labels=c("None","1-3 times a week","4-6 times a week","Every day of the week"))

# smoking 
df$ever_smk <- ifelse(df$smk_last_u>0 & df$smk_last_u<97,1,ifelse(df$smk_last_u==0,0,NA))

# alcohol consumption 
df$alcohol <- ifelse((df$alcohol_12m==0 | df$alcohol_last_u==0), 0, # never drinker
                     ifelse(((df$alcohol_last_u==3 & df$alcohol_last>3) | (df$alcohol_last_u==4 & df$alcohol_last>0) | df$alcohol_12m==1), 1, # had last drink more than 3 months ago, at least 1 year ago, or not in the last year
                            ifelse((df$alcohol_last_u %in% c(1,2) | (df$alcohol_last_u==5 & df$alcohol_last<90) | (df$alcohol_last_u==3 & df$alcohol_last<=3) | df$alcohol_today==1), 2, NA))) # had last drink within minutes or hours, or within 90 days, or  

df$alcohol <- ifelse(is.na(df$alcohol) & df$alcohol_12m %in% c(2,3,4,5,6,7,8,9,10), 1, df$alcohol) # if we don't have information from other questions but they said they had had some frequency of drinking in the past year, code them as previous drinker
df$alcohol <- factor(df$alcohol, levels=c(0,1,2), labels=c("Never drinker","Previous drinker","Current or recent drinker"))

# working status 
df$employment <- ifelse(df$work_stat %in% c(97,98,99), NA, df$work_stat)
df$employment <- factor(df$employment, levels=c(1,2,4,5,6), labels = c("Working","Looking for work","Raising children","Student","Other"))

# occupation 
health_care <- c("physician","nurs","acupunctur","accupunctur","doctor","fellow","resident", "medical","np", "therapist", "pediatrician", "pharmacist", "rn","chiropractor", "clinical","dentist","phlebotomist","psychologist","optometrist")
research <- c("research","post doc", "post-doc","statistician","scientist","biologist","epidemiologist","economist")
legal <- c("lawyer","attorney","prosecutor","legal","court","law")
caregiving <- c("babysit","baby sit","care","home","nanny")
cleaning <- c("janitor", "clean", "keep", "limpieza")
education <- c("teacher","professor","speech","librarian","principal", "school","student","tutor")
service <- c("cashier","cajera", "retail","clerk", "driver","rental car","UPS","street","train","gym","firefighter","boat")
food <- c("server","catering","cook","chef","busser","take out","food","restaurant","waitress","Mc Donalds","produce","deli","barista")
business <- c("receptionist","assistant","realtor","real estate","entrepreneur","entrepenuer","business","quality assurance","security","insurance","executive","account","administrat","analyst","human resource","program officer","communications","HR","recruit", "consultant","manage","finance","design","policy","tech","corporate","marketing","marketer","sales","engineer","data science","supply chain","public relations","workforce")
arts <- c("writer", "art","photo","producer","gaffer","film","yoga","life coach")

#"receptionist", "guard", "UPS", "security", "street inspector","training","trainer","gym","life coach", "real estate","yoga"
# program assistant 
df$occupation  <- ifelse(grepl(paste(health_care, collapse="|"),df$job1, ignore.case = T),1,
                         ifelse(grepl(paste(research, collapse="|"), df$job1, ignore.case=T), 2, 
                                ifelse(grepl(paste(legal, collapse="|"), df$job1, ignore.case=T), 3, 
                                       ifelse(grepl(paste(caregiving, collapse="|"), df$job1, ignore.case=T), 4, 
                                              ifelse(grepl(paste(cleaning, collapse="|"), df$job1, ignore.case=T), 5, 
                                                           ifelse(grepl(paste(education, collapse="|"), df$job1, ignore.case=T), 6, 
                                                                  ifelse(grepl(paste(service, collapse="|"), df$job1, ignore.case=T), 7, 
                                                                         ifelse(grepl(paste(food, collapse="|"), df$job1, ignore.case=T), 8, 
                                                                                ifelse(grepl(paste(business, collapse="|"), df$job1, ignore.case=T), 9, 
                                                                                       ifelse(grepl(paste(arts, collapse="|"), df$job1, ignore.case=T), 10, NA))))))))))

df$occupation <- factor(df$occupation, levels=c(1,2,3,4,5,6,7,8,9,10), labels=c("Health care","Research","Legal","Care giving","Sanitation","Education","Service","Food","Business","Arts"))


# consider adding information from second job -- how to create hierarchy?
table(df$occupation, exclude=NULL)
df$job1[is.na(df$occupation)]

# eating habits 
df$takeout_freq <- ifelse(df$fdx_takeout>95,NA,
                          ifelse(df$fdx_takeout>=4 & df$fdx_takeout<95, 4, df$fdx_takeout))

df$takeout_freq <- factor(df$takeout_freq, levels=c(1,2,3,4), labels=c("Less than once a month","1-3 times a month","Once a week","More than once a week"))


df$prepared_freq <- ifelse(df$fdx_prepare>95,NA, 
                           ifelse(df$fdx_prepare<=4, 4, df$fdx_prepare))

df$prepared_freq <- factor(df$prepared_freq, levels=c(4,5,6,7), labels=c("Every other day or less","4-6 times a week","Once a day","More than once a day"))


# personal product use 

products_daily <- c("prod_daily_shampoo", "prod_daily_makeup", "prod_daily_hairspray","prod_daily_lipbalm","prod_daily_lotion","prod_daily_sunscreen","prod_daily_deodorant","prod_daily_vagwash","prod_daily_perfume","prod_daily_nailpolish","prod_daily_colgate")


products_today <- c("prod_today_shampoo", "prod_today_makeup", "prod_today_hairspray","prod_today_lipbalm","prod_today_lotion","prod_today_sunscreen","prod_today_deodorant","prod_today_vagwash","prod_today_perfume","prod_today_nailpolish","prod_today_colgate")


# cleaning product use 

cleaning_daily <- c("cln_daily_bleach","cln_daily_airfresh","cln_daily_ammonia","cln_daily_candles","cln_daily_solvents","cln_daily_sprays","cln_daily_polish")

cleaning_today <- c("cln_today_bleach","cln_today_airfresh","cln_today_ammonia","cln_today_candles","cln_today_solvents","cln_today_sprays","cln_today_polish")


# specific food frequencies -- ask if these questions came from a specific source and if there are guidelines about how to analyze them

# cooking behaviors 
df$teflon_num <- ifelse(df$pots >95, NA, 
                        ifelse(df$pots>=8 & df$pots<95, 8, df$pots))

df$teflon_scratch <- ifelse(df$pots_scr>95, NA, 
                            ifelse(df$pots_scr>=6,6, df$pots_scr))

 

# read in medical record abstraction data 
df_mr <- read.csv("/Users/danagoin/Documents/Research projects/CiOB-ECHO/CiOB2 data/medicalrecordabstraction.csv")
# just keep age variable 
df_mr <- df_mr %>% select(ppt_id, age_dlvry_mr)

# merge with the rest of the data 
df <- left_join(df, df_mr)


# create nativity variable 
df$us_born <- ifelse(df$born_us_m==1,1,ifelse(df$born_us_m==0,0,NA))
table(df$us_born)
df$us_born <- ifelse(df$yrs_us_m<df$age_dlvry_mr & !is.na(df$yrs_us_m) & !is.na(df$age_dlvry_mr) & df$us_born!=1, 0, 
                     ifelse(df$yrs_us_m>=df$age_dlvry_mr & !is.na(df$yrs_us_m) & !is.na(df$age_dlvry_mr),1, df$us_born))
table(df$us_born)



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
 
 # for now just keep race, education, income, and nativity
 
df_c <- df %>% select(ppt_id, mat_race_eth, mat_edu, marital, hh_income_cat, us_born, age_dlvry_mr, 
                      occupation, takeout_freq, prepared_freq, products_daily, products_today, cleaning_daily, cleaning_today)
 
# merge with chemical data 
df_m <- left_join(df_t, df_c)

# descriptive statistics 
# calculated versus medical record age
#table(df_m$mage, exclude=NULL)
table(df_m$age_dlvry_mr, exclude=NULL)

df_m %>% summarise(mean=mean(age_dlvry_mr), sd=sqrt(var(age_dlvry_mr)))
df_m %>% group_by(mat_race_eth) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N)) 
df_m %>% group_by(marital) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N)) 
df_m %>% group_by(mat_edu) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N)) 
df_m %>% group_by(hh_income_cat) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N)) 
df_m %>% group_by(us_born) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N)) 


# test whether the distributions differ across categories 
# can't do one-way anova, at least on untransformed data, because it's very skewed and/or bimodal 
# could test difference in medians using Wilcoxon rank sum test 

# Aolin's paper used the following
# kruskal-wallis rank sum test was used to compare the rank order of peak area values 
# this does one-way ANOVA on ranks, and is a non-parametric test to tell whether samples came from the same distribution 
# is an extension of the Mann-Whitney U test which is only for 2 samples 
# the parametric version is just one-way ANOVA 
# for suspect features in fewer than 20% of participants, she used Fisher's exact test 
# adjust for multiple comparisons using B-H FDR 
# Aolin did a log-2 transform 

# I have paired samples though -- how to deal with that?
# for now just analyze them separately 

# bring all demographic vars to the front of the data set 
df_m <- df_m %>% select(sample_id, ppt_id, sample_type, mat_race_eth, marital, mat_edu, hh_income_cat, us_born, age_dlvry_mr, 
                        occupation, takeout_freq, prepared_freq, products_daily, products_today, cleaning_daily, cleaning_today, everything())

# replace with 0 if abundance was below 2*10^5  -- check if this is really 10^4

for(i in 1:length(chems)){
        print(i)
        df_m[[chems[i]]] <- ifelse(df_m[[chems[i]]]<2*10^5,0, df_m[[chems[i]]])
}

# identify chemicals who have abundance above threshold for >=80% of participants 
m <- length(df_m) - length(chems)

chems_detected <- apply(df_m[,m:length(df_m),], 2, function(x) sum(x>0)/dim(df_m)[1])

list_80pct <- chems_detected[chems_detected>=0.8]
length(list_80pct)
# there are 121 with more than 80% above abundance cutoff 

df_ms <- df_m %>% select(sample_id, ppt_id, sample_type, mat_race_eth, marital, mat_edu, hh_income_cat, us_born, age_dlvry_mr, 
                         occupation, takeout_freq, prepared_freq, products_daily, products_today, cleaning_daily, cleaning_today, 
                         names(list_80pct))


# maternal serum 
df_ms_serum <- df_ms %>% filter(sample_type=="M")



# cord blood 
df_ms_cb <- df_ms %>% filter(sample_type=="C")

# see how presence of chemicals differs by demographics 

bplot <- function(dem, i) {
ggplot(df_ms, aes(x=factor(get(dem)), y=get(names(list_80pct)[i]))) + 
        theme_bw()  + geom_boxplot() + labs(x="",y=names(list_80pct)[i]) 
        
}


bplot("occupation", 5)




# maternal serum descriptive stats 
df_ms %>% summarise(mean=mean(age_dlvry_mr), sd=sqrt(var(age_dlvry_mr)))
df_ms %>% group_by(mat_race_eth) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N)) 
df_ms %>% group_by(marital) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N)) 
df_ms %>% group_by(mat_edu) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N)) 
df_ms %>% group_by(hh_income_cat) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N)) 
df_ms %>% group_by(us_born) %>% summarise(N=n())  %>% mutate(proportion = N/sum(N)) 


# correlations betwen maternal serum and cord blood abundances 

ms_cb_corr <- function(x) {

df_t <- df_m %>% select(ppt_id, sample_type, names(df_m)[x])

df_ts <- spread(df_t, key=sample_type, value=names(df_t)[3])

cor_result <- cor(log(df_ts$M), log(df_ts$C), use="pairwise.complete.obs")
cor_test <- cor.test(log(df_ts$M), log(df_ts$C), method="spearman")

cord_ms_ratio <- log(df_ts$C)/log(df_ts$M)

results <- cbind(cor_result, cor_test$p.value, median(cord_ms_ratio, na.rm=T))

return(results)
}

cor_list <- lapply(10:dim(df_m)[2], function(x) ms_cb_corr(x))

cor_df <- data.frame(do.call(rbind, cor_list))
names(cor_df) <- c("correlation","p.value", "median_cb_ms_ratio")

cor_p1 <- ggplot(cor_df, aes(x=correlation)) + geom_histogram(fill="#225ea8", colour="black") + theme_bw()
ggsave(cor_p1, file="/Users/danagoin/Documents/R01 GSS New Methods/results/correlations_ms_cb.pdf")

cor_p2 <- ggplot(cor_df, aes(x=p.value)) + geom_histogram(fill="#225ea8", colour="black") + theme_bw()
ggsave(cor_p2, file="/Users/danagoin/Documents/R01 GSS New Methods/results/p_values_ms_cb.pdf")

# note: in Rachel's paper she said they only looked at the ratio among those where the chemical was detected 
cor_p3 <- ggplot(cor_df, aes(x=median_cb_ms_ratio)) + geom_histogram(fill="#225ea8", colour="black") + theme_bw()
ggsave(cor_p2, file="/Users/danagoin/Documents/R01 GSS New Methods/results/p_values_ms_cb.pdf")



kruskal.test(C24H49NO3_16.186764 ~ mat_race_eth, data=df_ms)
kruskal.test(C24H49NO3_16.186764 ~ hh_income_cat, data=df_ms)
kruskal.test(C24H49NO3_16.186764 ~ mat_edu, data=df_ms)
kruskal.test(C24H49NO3_16.186764 ~ latina_coo, data=df_ms)
kruskal.test(C24H49NO3_16.186764 ~ marital, data=df_ms)


