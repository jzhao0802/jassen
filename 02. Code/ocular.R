rm(list=ls())

library(glmnet)
library(xlsx)

options(digits=7)

data_path<- 'D:\\Project_2014\\Jassen project\\02 raw data'
out_path<-'D:\\Project_2014\\Jassen project\\04 output hui'
setwd(out_path)

cohort<-'mild_uni'
DescriptiveTable_ocular <- function(cohort){
  raw_data <- read.table(paste(data_path, '\\', cohort, '_ocular.csv', sep=''), head=T, sep=',')  #[1] 4332   56
  # change names of all variables into lower case
  names(raw_data)<- tolower(names(raw_data))
  
  glaucoma <- raw_data[raw_data$type_diag=='Glaucoma',]
  cataract <- raw_data[raw_data$type_diag=='Cataract',]
  
  glaucoma_target <- glaucoma[grep('glaucoma',glaucoma$descr_diag),]
  cataract_target <- cataract[grep('cataract',cataract$descr_diag),]

  sitelist <- c('A','P','S','V')
  gla <- numeric()
  cat <- numeric()
  for (site in sitelist){
    

    gla_diag_by_site<-numeric()
    cat_diag_by_site<-numeric()
    gla_temp<-numeric()
    cat_temp<-numeric()
    
    
    # number of unique pat whose type = glaucoma or cataract
    eval(parse(text = paste("pat_cnt_gla_all <- length(levels(as.factor(as.vector(glaucoma[glaucoma$siteid=='",site,"','patientid']))))",sep='')))
    eval(parse(text = paste("pat_cnt_cat_all <- length(levels(as.factor(as.vector(cataract[cataract$siteid=='",site,"','patientid']))))",sep='')))
    # number of unique pat whose des contains glaucoma or cataract
    eval(parse(text = paste("pat_cnt_gla_all_by_site <- length(levels(as.factor(as.vector(glaucoma_target[glaucoma_target$siteid=='",site,"' ,'patientid']))))",sep='')))
    eval(parse(text = paste("pat_cnt_cat_all_by_site <- length(levels(as.factor(as.vector(cataract_target[cataract_target$siteid=='",site,"' ,'patientid']))))",sep='')))
    
    # first 5 descri_diag glaucoma
    for ( diag in names(head(sort(table(as.factor(as.vector(glaucoma_target$descr_diag))),T),3))){
      eval(parse(text = paste("glaucoma_target_diag <- glaucoma_target[glaucoma_target$descr_diag=='",diag,"',]",sep='')))
      
      eval(parse(text = paste("pat_cnt_gla_diag_by_site <- length(levels(as.factor(as.vector(glaucoma_target_diag[glaucoma_target_diag$siteid=='",site,"','patientid']))))",sep='')))
      
      gla_diag_by_site<-rbind(gla_diag_by_site,pat_cnt_gla_diag_by_site)
    }
    
    # cataract
    for ( diag in names(head(sort(table(as.factor(as.vector(cataract_target$descr_diag))),T),3))){
      eval(parse(text = paste("cataract_target_diag <- cataract_target[cataract_target$descr_diag=='",diag,"',]",sep='')))
      
      eval(parse(text = paste("pat_cnt_cat_diag_by_site <- length(levels(as.factor(as.vector(cataract_target_diag[cataract_target_diag$siteid=='",site,"','patientid']))))",sep='')))
      
      pat3<-levels(as.factor(as.vector(cataract_target_diag[cataract_target_diag$siteid=='P','patientid'])))
      
      cat_diag_by_site<-rbind(cat_diag_by_site,pat_cnt_cat_diag_by_site)
    }
    
    
    
    gla_temp <- rbind(gla_diag_by_site,pat_cnt_gla_all_by_site,pat_cnt_gla_all)
    cat_temp <- rbind(cat_diag_by_site,pat_cnt_cat_all_by_site,pat_cnt_cat_all)
    
    gla <- cbind(gla,gla_temp)
    cat <- cbind(cat,cat_temp)
    
  }
  
  colnames(gla)<-c('Site A', 'Site P', 'Site S', 'Site V')
  colnames(cat)<-c('Site A', 'Site P', 'Site S', 'Site V')
  rownames(gla)<-c(names(head(sort(table(as.factor(as.vector(glaucoma_target$descr_diag))),T),3)), 'pat_cnt_contain_gla', 'pat_cnt_gla')
  rownames(cat)<-c(names(head(sort(table(as.factor(as.vector(cataract_target$descr_diag))),T),3)), 'pat_cnt_contain_cat', 'pat_cnt_cat')
  
  combine <- rbind(gla,cat)
  write.xlsx(combine, file=paste(out_path, 'v2_ocular_table.xlsx', sep='\\'), sheetName=cohort, row.names=T, append=T, showNA=T)
}

DescriptiveTable_ocular('mild_bil')
DescriptiveTable_ocular('mild_uni')
DescriptiveTable_ocular('ga_all')


test<-raw_data[raw_data$patientid==pat2&raw_data$siteid=='P',c('patientid','eye','descr_diag')]



