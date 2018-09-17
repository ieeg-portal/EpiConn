# Running the stats for struct-func ictal dynamics paper
# Preya Shah June 4 2018
# Revised Preya Shah Aug 9 2018

library(dplyr)
library(lme4)
library(pbkrtest)

regressed <- FALSE

if(regressed){
  outfile <- paste('StructFunc_LME_Analysis', '_regressed.txt',sep='')
  sink(outfile)
} else {
  outfile <- 'StructFunc_LME_Analysis.txt'
  sink(outfile)
}

for(band in c('broadband','alphatheta','beta','lowgamma','highgamma')){

  if(regressed){
    datfile <- paste('../data_for_lme_',  band, '_regressed.csv',sep='')
  } else {
    datfile <- paste('../data_for_lme_',  band, '.csv',sep='')
  }
  
  print(datfile)
  print('Reading data')
  dat <- read.csv(datfile)
  
   dat <- within(dat, {
    subject <- factor(subject)
  })
  
  cat('\n\n\n---Mixed Effects Models---\n')
  cat(band)
  
  #### Step 1: test if the difference between Ictal SC-FC correlation and Pre-ictal SC-FC correlation is significant across subjects
  #### (left-most part of Figure 1) - looks like it is
  print('Test: Ictal vs. Preictal')
  dat_ictal_v_pre = select(dat, ictal_v_pre, subject)
  #print(dat_ictal_v_pre)
  
  largemodel <- lmer('ictal_v_pre ~ 1 + (1 | subject)', dat_ictal_v_pre)
  #print(largemodel)
  
  smallmodel <- lmer('ictal_v_pre ~ 0 + (1 | subject)', dat_ictal_v_pre)
  #print(smallmodel)
  
  mymodel <-PBmodcomp(largemodel, smallmodel) 
  print(mymodel)
  
  #### Step 2: test if the difference between Ictal SC-FC correlation and Interictal SC-FC correlation is significant across subjects
  #### (middle part of Figure 1) - looks like it is
  
  print('Test: Ictal vs. Interictal')
  dat_ictal_v_inter = select(dat, ictal_v_inter, subject)
  #print(dat_ictal_v_inter)
  
  largemodel <- lmer('ictal_v_inter ~ 1 + (1 | subject)', dat_ictal_v_inter)
  #print(largemodel)
  
  smallmodel <- lmer('ictal_v_inter ~ -1 + (1 | subject)', dat_ictal_v_inter)
  #print(smallmodel)
  
  mymodel <-PBmodcomp(largemodel, smallmodel) 
  print(mymodel)
  
  #### Step 3: test if the difference between Preictal SC-FC correlation and Interictal SC-FC correlation is significant across subjects
  #### (right-most part of Figure 1) - looks like it isn't, and we wouldn't expect it to be.
  
  print('Test: Preictal vs. Interictal')
  dat_pre_v_inter = select(dat, pre_v_inter, subject)
  #print(dat_pre_v_inter)
  
  largemodel <- lmer('pre_v_inter ~ 1 + (1 | subject)', dat_pre_v_inter)
  #print(largemodel)
  
  smallmodel <- lmer('pre_v_inter~ -1 + (1 | subject)', dat_pre_v_inter)
  #print(smallmodel)
  
  mymodel <-PBmodcomp(largemodel, smallmodel) 
  print(mymodel)

}

sink()

