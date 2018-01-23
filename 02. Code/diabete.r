rm(list=ls())

library(glmnet)
library(xlsx)

options(digits=7)

data_path<- 'D:\\Project_2014\\Jassen project\\02 raw data'
out_path<-'D:\\Project_2014\\Jassen project\\04 output hui'
setwd(out_path)

cohort<- 'mild_bil'
DescriptiveTable_diab <- function(cohort){
  raw_data <- read.table(paste(data_path, '\\', cohort, '_diab.csv', sep=''), head=T, sep=',')  #[1] 4332   56
  # change names of all variables into lower case
  names(raw_data)<- tolower(names(raw_data))
  sitelist <- c('A','P','S','V')
  pat_cnt1 <- numeric()
  pat_cnt2 <- numeric()
  for (site in sitelist){
    
    eval(parse(text = paste("pat_cnt_all <- length(levels(as.factor(as.vector(raw_data[raw_data$siteid=='",site,"','patientid']))))",sep='')))
    eval(parse(text = paste("pat_cnt_by_site <- length(levels(as.factor(as.vector(raw_data[raw_data$siteid=='",site,"' & raw_data$isdiabetic=='True','patientid']))))",sep='')))
  
    pat_cnt1 <- cbind(pat_cnt1, pat_cnt_by_site)
    pat_cnt2 <- cbind(pat_cnt2, pat_cnt_all)
  
  }
  pat_cnt <- rbind(pat_cnt1, pat_cnt2)
  colnames(pat_cnt)<-c('Site A', 'Site P', 'Site S', 'Site V')
  rownames(pat_cnt)<-c('pat_cnt_by_site_dia','pat_cnt_all_dia')
  

  write.xlsx(pat_cnt, file=paste(out_path, 'diabetes_table.xlsx', sep='\\'), sheetName=cohort, row.names=T, append=T, showNA=T)
  write.xlsx(test, file=paste(out_path, 'test_table.xlsx', sep='\\'), sheetName=cohort, row.names=T, append=T, showNA=T)
}
DescriptiveTable_diab('mild_bil')
DescriptiveTable_diab('mild_uni')
DescriptiveTable_diab('ga_all')


