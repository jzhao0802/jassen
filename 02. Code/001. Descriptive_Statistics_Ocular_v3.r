#==========================================================================================
#  Project:  Janssen project

#    Part II: Discriptive table for Ocular, diabectic and oct

#    Develop time: 2/27/2015 - .

#    Developer: Jie Zhao
#==========================================================================================
rm(list=ls())

library(glmnet)
library(xlsx)

options(digits=7)

data_path<- 'D:\\working materials\\Jassen Project\\01. Raw_data\\fromSAS'
out_path<-'D:\\working materials\\Jassen Project\\03.Output\\Feb27'
setwd(out_path)

raw_data <- read.table(paste(data_path, 'mild_uni_ocular.csv', sep='\\'), head=T, sep=',')  #[1] 187  53

# change names of all variables into lower case
names(raw_data)<- tolower(names(raw_data))
#attach(raw_data)

# qc raw data, data missing etc.
dim(raw_data)
na_check <- apply(apply(raw_data, 2, is.na), 2, sum)



cohort <- 'mild_uni'
DescriptiveTable_oct_v3 <- function(cohort){
    
    raw_data <- read.table(paste(data_path, '\\', cohort, '_oct.csv', sep=''), head=T, sep=',')  #[1] 465   57
    
    # change names of all variables into lower case
    names(raw_data)<- tolower(names(raw_data))
    #attach(raw_data)
    
    # qc raw data, data missing etc.
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    diag_lst <- c('central1mmretinalthickness', 'fovealthickness', 'macularvolume', 'pedthickness')
    raw_data$macularvolume <- as.numeric(as.vector(raw_data$macularvolume))
    raw_data$nmiss <- ifelse(is.na(raw_data[, diag_lst[1]]) & is.na(raw_data[, diag_lst[2]]) & is.na(raw_data[, diag_lst[3]]) & is.na(raw_data[, diag_lst[4]]), 0, 1)
    #nmiss.num <- length(levels(as.factor(as.vecotr(raw_data[raw_data$nmiss==1, raw_data$patientid])))) #unique pt count
    #nmiss.prop <- nmiss.num/dim(raw_data)[1]
    nmiss.num.row <- numeric()
    miss.num.row <- numeric()
    total.num.row <- numeric()
    for(i in c('A', 'P', 'S', 'V')){
        nmiss.num.s <- length(levels(as.factor(as.vector(raw_data[raw_data$nmiss==1 & raw_data$siteid==i, 'patientid']))))
        miss.num.s  <- length(levels(as.factor(as.vector(raw_data[raw_data$nmiss==0 & raw_data$siteid==i, 'patientid']))))
        total.num.s <- length(levels(as.factor(as.vector(raw_data[raw_data$siteid==i, 'patientid']))))
        nmiss.num.row <- cbind(nmiss.num.row, nmiss.num.s)
        miss.num.row <- cbind(miss.num.row, miss.num.s)
        total.num.row <- cbind(total.num.row, total.num.s)
    }
    output <- rbind(nmiss.num.row, miss.num.row, total.num.row)
    output.df <- data.frame(IsMiss=c('False', 'True', 'Total'), output)
    colnames(output.df) <- c('IsMiss', 'A', 'P', 'S', 'V')
    
    write.xlsx(output.df, file=paste(out_path, 'Descriptive_Table_NMiss_oct_v2.xlsx', sep='\\'), sheetName=cohort, row.names=F, append=T, showNA=T)
    
    statMtx.df.rbind <- numeric()
    i <- diag_lst[4]
    #output_rbind <- numeric()
    for(i in diag_lst){
        cnt<-rep(0,4)
        siteList<-c('A', 'P', 'S', 'V')
        mean<-rep(0,4)
        sd<-rep(0,4)
        median<-rep(0,4)
        iqr<-rep(0,4)
        min<-rep(0,4)
        max<-rep(0,4)  
        
        var <- na.omit(raw_data[,i])
        site <- raw_data[!is.na(raw_data[, i]), 'siteid']
        if(length(var)>0){
            var1 <- rep(1, length(var))
            cnt <- tapply(var1, list(Site=site), sum)
            mean <- tapply(var, list(Site=site), mean)
            sd<- tapply(var, list(Site=site), sd)
            median <- tapply(var, list(Site=site), median)
            iqr <- tapply(var, list(Site=site), IQR)
            min <- tapply(var, list(Site=site), min)
            max <- tapply(var, list(Site=site), max)
            
        }
        statMtx <- rbind(N=cnt, Mean=mean, SD=sd, Median=median, IQR=iqr, Min=min, Max=max)
        statMtx.df <- data.frame(Variable=rep(i, 7), Stat=rownames(statMtx), statMtx)    #do not forget to add flag before rbind
        #colnames(statMtx.df) <- colnames(statMtx.rbind)
        statMtx.df.rbind <- rbind(statMtx.rbind, statMtx.df)
    }    
    write.xlsx(statMtx.df.rbind, file=paste(out_path, 'Descriptive_Table_oct_v4.xlsx', sep='\\'), sheetName=cohort, row.names=F, append=T, showNA=T)

}
DescriptiveTable_oct_v3('mild_bil')
DescriptiveTable_oct_v3('mild_uni')
DescriptiveTable_oct_v3('ga_all')
