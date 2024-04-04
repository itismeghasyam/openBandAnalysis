#####################################
### OpenBand Prelim Study Summary
#####################################
library(synapser)
library(tidyverse)
library(blandr) #devtools::install_github("deepankardatta/blandr")
synapser::synLogin()

####
# Required functions
####
## Get HR from sitting signal
summarize_get_heartrate <- function(hr.df){
  
  hr.df.out <- tryCatch(  {red.df <- hr.df$red %>% 
    dplyr::arrange(confidence) %>% 
    dplyr::slice_tail(n=10) %>% 
    dplyr::summarise(mean_hr = mean(hr),
                     mean_conf = mean(confidence),
                     median_hr = median(hr),
                     median_conf = median(confidence)) %>% 
    dplyr::mutate(channel = 'g1')
  
  green.df <- hr.df$green %>% 
    dplyr::arrange(confidence) %>% 
    dplyr::slice_tail(n=10) %>% 
    dplyr::summarise(mean_hr = mean(hr),
                     mean_conf = mean(confidence),
                     median_hr = median(hr),
                     median_conf = median(confidence)) %>% 
    dplyr::mutate(channel = 'g2')
  
  hr.df.out <- red.df %>% 
    dplyr::full_join(green.df) %>% 
    dplyr::mutate(samplingRate = hr.df$sampling_rate)},
  
  error = function(e){
    return(data.frame(mean_hr = NA, mean_conf = NA, channel = 'NA'))})
  
  return(hr.df.out)
}

######################################
## Get Records and Analyze
######################################
# Find all records later than Oct 31, 2023 - this was the onboarding date
summary.tbl <- synapser::synTableQuery("SELECT * FROM syn22164707 where uploadDate > '2023-10-31'")$asDataFrame()

# Number of participants and records per participant per table
records.tbl <- summary.tbl %>% 
  dplyr::select(healthCode, originalTable, dayInStudy, phoneInfo, createdOn) %>% 
  unique() %>% 
  dplyr::group_by(healthCode) %>% 
  dplyr::mutate(correctedDayInStudy = (dayInStudy - min(dayInStudy) + 1)) %>% 
  dplyr::ungroup() 

# Records per each table
originalTable_records <- records.tbl %>% 
  dplyr::group_by(originalTable) %>% 
  dplyr::count()
knitr::kable(originalTable_records)

# Filter to tables that we need only - Sitting, Cycling, Walking, Sleeping
records.tbl.meta <- records.tbl %>% 
  dplyr::filter(originalTable %in% c('Sitting-v1','Cycling-v1','Walking-v1','Sleeping-v1'))

# number of participants! [Total 5 participants as of now]
print(unique(records.tbl.meta$healthCode))
print(unique(records.tbl.meta$healthCode) %>% length())

# How did participants contribute to acitivites: Records per participant categorized by activity
records.tbl.activity <- records.tbl.meta %>% 
  dplyr::select(healthCode, originalTable, correctedDayInStudy) %>% 
  tidyr::pivot_wider(names_from = originalTable,
                     values_from = correctedDayInStudy)
knitr::kable(records.tbl.activity)
View(records.tbl.activity)
# Looking at the above, we see that after the orientation date of Oct 31, 2023
# - Only 3 participants have records for all activities
# - One participant has records for all activities EXCEPT Walking
# - One participant has just a single record, for Sitting-v1 

sitting.syn <- synapser::synTableQuery("SELECT * FROM syn26336172 where uploadDate > '2023-10-01'")
sitting.tbl <- sitting.syn$asDataFrame()

## Get raw data
## Download required columns i,e the JSON files
columnsToDownload = c("rawData") 

# start_time = Sys.time()
sitting.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synapser::synDownloadTableColumns(sitting.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = paste0(col.name,'.fileLocation')))
})

## Merge file locations to main table to create a meta table
sitting.meta = data.table::rbindlist(
  list(sitting.tbl %>%
         dplyr::left_join(do.call(cbind, sitting.json.loc[1]))),
  use.names = T, fill = T) %>%
  as.data.frame

###
# unzip each file and check
###
bb <- apply(sitting.meta,1,function(x){
  print(x[['rawData.fileLocation']])
  file.list <- unzip(x[['rawData.fileLocation']],exdir = 'temp_test')
  
  # mean polarHR for the three minute duration
  polar.hr <- read.csv('temp_test/polarHr.csv') 
  mean_polar_hr <- mean(polar.hr$hr, na.rm = TRUE)
  median_polar_hr <- median(polar.hr$hr, na.rm = TRUE)
    
  # check for ppg data
  ppg1 <- read.csv('temp_test/openBandPpg1.csv') %>% 
    dplyr::mutate(sensor = 'ppg1')
  ppg2 <- read.csv('temp_test/openBandPpg2.csv') %>% 
    dplyr::mutate(sensor = 'ppg2')
  ppg <- ppg1 %>% dplyr::full_join(ppg2) %>% 
    dplyr::mutate(rawData.fileLocation = x[['rawData.fileLocation']]) %>% 
    dplyr::mutate(meanPolarHR = mean_polar_hr,
                  medianPolarHR = median_polar_hr)
  unlink('temp_test/', recursive = T)
  return(ppg)  
})

##### Correlation between ppg sensors [doing good]
ppg_signal_cor <- lapply(bb, function(aa){
  print(unique(aa$rawData.fileLocation))
  
  out.dat <- tryCatch({  aa_ppg1 <- aa %>% 
    dplyr::filter(sensor == 'ppg1') %>% 
    na.omit() %>% 
    dplyr::select(timestamp=relativeTimestamp, red = g1, green = g2) %>% 
    dplyr::mutate(blue = 0)
  
  aa_ppg2 <- aa %>% 
    dplyr::filter(sensor == 'ppg2') %>% 
    na.omit() %>% 
    dplyr::select(timestamp=relativeTimestamp, red = g1, green = g2) %>% 
    dplyr::mutate(blue = 0)
  
  ## PPG Correlation
  ppg1_cor <- cor(aa_ppg1$red, aa_ppg1$green)
  ppg2_cor <- cor(aa_ppg2$red, aa_ppg2$green)
  
  ## Get Resting HR from the signal
  ppg1_hr <- mhealthtools::get_heartrate(aa_ppg1)
  ppg2_hr <- mhealthtools::get_heartrate(aa_ppg2)  
  
  ## In the three minutes, let's pick 10 most confident windows and get mean HR from that
  ## compare that to polarHR
  
  ppg1_hr_summary <- summarize_get_heartrate(ppg1_hr) %>% 
    dplyr::mutate(sensor = 'ppg1')
  
  ppg2_hr_summary <- summarize_get_heartrate(ppg2_hr) %>% 
    dplyr::mutate(sensor = 'ppg2')
  
  ppg_hr_summary <- ppg1_hr_summary %>% 
    dplyr::full_join(ppg2_hr_summary) %>% 
    dplyr::mutate(`rawData.fileLocation` = unique(na.omit(aa$rawData.fileLocation)))
  
  out.dat <- aa %>% 
    dplyr::select(rawData.fileLocation, meanPolarHR, medianPolarHR) %>% 
    unique() %>% 
    dplyr::mutate(ppg1_cor = ppg1_cor,
                  ppg2_cor = ppg2_cor) %>% 
    dplyr::left_join(ppg_hr_summary)
  }, error = function(e){return(NULL)})
  
  return(out.dat)
  
}) %>% data.table::rbindlist(fill = T)

mean(abs(ppg_signal_cor$ppg1_cor), na.rm = T)
mean(abs(ppg_signal_cor$ppg2_cor), na.rm = T)

##### Plots
## Mean HR vs Polar HR
### Bland Altman plots
ppg_signal_plot <- ppg_signal_cor %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(diff_bland_mean = meanPolarHR-mean_hr,
                diff_bland_median = medianPolarHR - median_hr) %>% 
  dplyr::mutate(mean_bland = mean(c(meanPolarHR, mean_hr), na.rm = T),
                median_bland = median(c(medianPolarHR, median_hr),na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(sitting.meta)

# plot - MEAN HR
ggplot2::ggplot(ppg_signal_plot, aes(x= mean_bland, y= diff_bland_mean))+facet_wrap(~sensor)+
  geom_point()+theme_minimal()

# plot - MEDIAN HR
ggplot2::ggplot(ppg_signal_plot, aes(x= median_bland, y= diff_bland_median))+facet_wrap(~sensor)+
  geom_point()+theme_minimal()


## Is the error specific to a healthCode?
ggplot2::ggplot(ppg_signal_plot, aes(x= mean_bland, y= diff_bland_mean, color = healthCode))+facet_wrap(~sensor)+
  geom_point()+theme_minimal()

ggplot2::ggplot(ppg_signal_plot, aes(x= median_bland, y= diff_bland_median, color = healthCode))+facet_wrap(~sensor)+
  geom_point()+theme_minimal()


## Correlation plots
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

# polar HR vs mean HR
ggplotRegression(lm( meanPolarHR ~ mean_hr, data = ppg_signal_plot))
cor(ppg_signal_plot$meanPolarHR, ppg_signal_plot$mean_hr, use = 'pairwise.complete.obs')
cor(ppg_signal_plot$medianPolarHR, ppg_signal_plot$median_hr, use = 'pairwise.complete.obs')

# diff betwen HR vs conf in hr estimate
ggplotRegression(lm( diff_bland ~ mean_conf, data = ppg_signal_plot))
