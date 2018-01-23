#Descriptive table by outcome
#CV comordidities
#by Jie March09

library(glmnet)
library(xlsx)

options(digits=7)

rm(list=ls())

data_path<- 'D:\\working materials\\Jassen Project\\01. Raw_data\\fromSAS'
out_path<-'D:\\working materials\\Jassen Project\\01. Raw_data\\ForModel'
setwd(out_path)

raw_data <- read.table(paste(data_path, 'mild_bil_oct.csv', sep='\\'), head=T, sep=',')  #[1] 4332   56
# change names of all variables into lower case
names(raw_data)<- tolower(names(raw_data))
#attach(raw_data)

# qc raw data, data missing etc.
dim(raw_data)
na_check <- apply(apply(raw_data, 2, is.na), 2, sum)

var_list <- c('mi', 'cva', 'tia', 'othercvs', 'hypertension')
pt <- raw_data[, 'patientid']
pt_uni <- levels(pt) #2628
pt_uni_df <- data.frame(pt_uni=pt_uni)
for(i in var_list){
    i <- var_list[1]
    var <- raw_data[, i]
    pt_1 <- levels(as.factor(as.vector(pt[var=='True'])))  #14
    pt_1_df <- data.frame(pt_1=pt_1, flag_1=rep(1, length(pt_1)))
    cohort_data <- raw_data[, 1:46]
    merge(pt_uni_df, cohort_data, by.x='pt_uni', by.y='patientid', all.x=T)
}

#diabetic
data_path<- 'D:\\working materials\\Jassen Project\\01. Raw_data\\fromSAS'
out_path<-'D:\\working materials\\Jassen Project\\01. Raw_data\\ForModel'
setwd(out_path)
uniPt_cv <- function(cohort){
    raw_data <- read.table(paste(data_path, paste(cohort, 'cv.csv', sep='_'), sep='\\'), head=T, sep=',')  #[1] 4332   56
    # change names of all variables into lower case
    names(raw_data)<- tolower(names(raw_data))
    #attach(raw_data)
    pt <- raw_data[, 'patientid']
    pt_uni <- levels(pt) #2628
    pt_uni_df <- data.frame(pt_uni=pt_uni)
    pt_mi <-levels(as.factor(as.vector(pt[raw_data$mi=='True'])))
    pt_cva <-levels(as.factor(as.vector(pt[raw_data$cva=='True'])))
    pt_tia <-levels(as.factor(as.vector(pt[raw_data$tia=='True'])))
    pt_othercvs <-levels(as.factor(as.vector(pt[grepl('hypertension',raw_data$othercvs, ignore.case=T)==F & raw_data$othercvs !=''])))
    pt_hypertension <-levels(as.factor(as.vector(pt[raw_data$hypertension>0])))
    
    
    pt_mi_df <- data.frame(pt_mi=pt_mi)
    pt_cva_df <- data.frame(pt_cva=pt_cva)
    pt_tia_df <- data.frame(pt_tia=pt_tia)
    pt_othercvs_df <- data.frame(pt_othercvs=pt_othercvs)
    pt_hypertension_df <- data.frame(pt_hypertension=pt_hypertension)
    #write.xlsx(pt_uni_df, file=paste(out_path, paste(cohort, 'uniPt.xlsx', sep='_'), sep='\\'), sheetName='reference', row.names=F, append=T, showNA=T)
    write.xlsx(pt_mi_df, file=paste(out_path, paste(cohort, 'CV_uniPt.xlsx', sep='_'), sep='\\'), sheetName=paste(cohort, "mi", sep='_'), row.names=F, append=T, showNA=T)
    write.xlsx(pt_cva_df, file=paste(out_path, paste(cohort, 'CV_uniPt.xlsx', sep='_'), sep='\\'), sheetName=paste(cohort, "cva", sep='_'), row.names=F, append=T, showNA=T)
    write.xlsx(pt_tia_df, file=paste(out_path, paste(cohort, 'CV_uniPt.xlsx', sep='_'), sep='\\'), sheetName=paste(cohort, "tia", sep='_'), row.names=F, append=T, showNA=T)
    write.xlsx(pt_othercvs_df, file=paste(out_path, paste(cohort, 'CV_uniPt.xlsx', sep='_'), sep='\\'), sheetName=paste(cohort, "othercvs", sep='_'), row.names=F, append=T, showNA=T)
    write.xlsx(pt_hypertension_df, file=paste(out_path, paste(cohort, 'CV_uniPt.xlsx', sep='_'), sep='\\'), sheetName=paste(cohort, "hypertension", sep='_'), row.names=F, append=T, showNA=T)
    
}
uniPt_cv('mild_uni')
uniPt_cv('mild_bil')
uniPt_cv('ga_all')



uniPt_diab <- function(cohort){
    raw_data <- read.table(paste(data_path, paste(cohort, 'diab.csv', sep='_'), sep='\\'), head=T, sep=',')  #[1] 4332   56
    # change names of all variables into lower case
    names(raw_data)<- tolower(names(raw_data))
    #attach(raw_data)
    pt <- raw_data[, 'patientid']
    pt_uni <- levels(pt) #2628
    pt_uni_df <- data.frame(pt_uni=pt_uni)
    pt_1 <-levels(as.factor(as.vector(pt[raw_data$isdiabetic=='True'])))
    pt_1_df <- data.frame(pt_1=pt_1)
    #write.xlsx(pt_uni_df, file=paste(out_path, paste(cohort, 'uniPt.xlsx', sep='_'), sep='\\'), sheetName='reference', row.names=F, append=T, showNA=T)
    write.xlsx(pt_1_df, file=paste(out_path, paste('Diab_uniPt.xlsx', sep='_'), sep='\\'), sheetName=cohort, row.names=F, append=T, showNA=T)
    
}
uniPt_diab('mild_uni')
uniPt_diab('mild_bil')
uniPt_diab('ga_all')

uniPt_ocular <- function(cohort){
    raw_data <- read.table(paste(data_path, paste(cohort, 'ocular.csv', sep='_'), sep='\\'), head=T, sep=',')  #[1] 4332   56
    # change names of all variables into lower case
    names(raw_data)<- tolower(names(raw_data))
    #attach(raw_data)
    pt <- raw_data[, 'patientid']
    pt_uni <- levels(pt) #2628
    
    pt_uni_c <- levels(as.factor(as.vector(pt[grepl('cataract', raw_data$descr_diag, ignore.case = T)])))
    pt_uni_g <- levels(as.factor(as.vector(pt[grepl('glaucoma', raw_data$descr_diag, ignore.case = T)])))
    pt_uni_c_df <- data.frame(pt_uni_c=pt_uni_c)
    pt_uni_g_df <- data.frame(pt_uni_g=pt_uni_g)
    write.xlsx(pt_uni_c_df, file=paste(out_path, paste('Ocular_uniPt.xlsx', sep='_'), sep='\\'), sheetName=paste(cohort,"cataract", sep='_'), row.names=F, append=T, showNA=T)
    write.xlsx(pt_uni_g_df, file=paste(out_path, paste('Ocular_uniPt.xlsx', sep='_'), sep='\\'), sheetName=paste(cohort,"glaucoma", sep='_'), row.names=F, append=T, showNA=T)
    
}
uniPt_ocular('mild_bil')
uniPt_ocular('mild_uni')
uniPt_ocular('ga_all')

uniPt_oct <- function(cohort){
    raw_data <- read.table(paste(data_path, paste(cohort, 'oct.csv', sep='_'), sep='\\'), head=T, sep=',')  #[1] 4332   56
    # change names of all variables into lower case
    names(raw_data)<- tolower(names(raw_data))
    #attach(raw_data)
    pt <- raw_data[, 'patientid']
    pt_uni <- levels(pt) #2628
    
    pt_uni_c <- levels(as.factor(as.vector(pt[grepl('cataract', raw_data$descr_diag, ignore.case = T)])))
    pt_uni_g <- levels(as.factor(as.vector(pt[grepl('glaucoma', raw_data$descr_diag, ignore.case = T)])))
    pt_uni_c_df <- data.frame(pt_uni_c=pt_uni_c)
    pt_uni_g_df <- data.frame(pt_uni_g=pt_uni_g)
    write.xlsx(pt_uni_c_df, file=paste(out_path, paste('Oct_uniPt.xlsx', sep='_'), sep='\\'), sheetName=paste(cohort,"cataract", sep='_'), row.names=F, append=T, showNA=T)
    write.xlsx(pt_uni_g_df, file=paste(out_path, paste('Oct_uniPt.xlsx', sep='_'), sep='\\'), sheetName=paste(cohort,"glaucoma", sep='_'), row.names=F, append=T, showNA=T)
    
}
uniPt_oct('mild_bil')
uniPt_oct('mild_uni')
uniPt_oct('ga_all')

var<- 'central'
var <- 'macular'
var <- 'foveal'
cohort <- 'mild_bil'
stat_oct_uniPt <- function(var, cohort){
    raw_data <- read.table(paste(out_path, paste(var, '_nomiss_mean_', cohort, '.csv', sep=''), sep='\\'), head=T, sep=',')  #[1] 4332   56
    # change names of all variables into lower case
    names(raw_data)<- tolower(names(raw_data))
    #attach(raw_data)
    N <- table(raw_data$siteid)
    mean <- tapply(raw_data[, 3], raw_data[, 2], mean)
    sd <- tapply(raw_data[, 3], raw_data[, 2], sd)
    median <- tapply(raw_data[, 3], raw_data[, 2], median)
    iqr <- tapply(raw_data[, 3], raw_data[, 2], IQR)
    min <- tapply(raw_data[, 3], raw_data[, 2], min)
    max <- tapply(raw_data[, 3], raw_data[, 2], max)
    stat <- rbind(N=N, Mean=mean, SD=sd, Median=median, IQR=iqr, MIN=min, MAX=max)
    write.xlsx(stat, file=paste(out_path, paste('stat_uniPt_oct_', cohort, '.xlsx', sep=''), sep='\\'), sheetName=var, row.names=T, append=T, showNA=T)
    
}

stat_oct_uniPt('central', 'mild_bil')
stat_oct_uniPt('macular', 'mild_bil')
stat_oct_uniPt('foveal', 'mild_bil')
stat_oct_uniPt('central', 'mild_uni')
stat_oct_uniPt('macular', 'mild_uni')
stat_oct_uniPt('foveal', 'mild_uni')
stat_oct_uniPt('central', 'ga_all')
stat_oct_uniPt('macular', 'ga_all')
stat_oct_uniPt('foveal', 'ga_all')





    


