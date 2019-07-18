

# this is the data that Dimitri has cleaned 
# the rows are the chemical features and the columns are the samples 
df_all <- read.csv("/Users/danagoin/Documents/R01 GSS New Methods/Data/R01_150_maternal_and_cord_1.2.csv")

# see if there are any protocols or descriptions of what was done 
# otherwise double-check that the two replications of the samples should be averaged 
# then reshape and merge on demographics from the CiOB-Echo data 

