#follow-up work of Jassen 
#boxplot for pol variable by biabetes
#4/26/2015-

rm(list=ls())

library(glmnet)
library(xlsx)
library(snowfall)
library(Hmisc)
options(digits=7)

data_path<- 'C:\\work\\jzhao\\working materials\\Jassen Project\\01. Raw_data\\ForModel'
out_path<-'C:\\work\\jzhao\\working materials\\Jassen Project\\03.Output\\sendout_Apr26'
setwd(out_path)

#names(raw_data)<- tolower(names(raw_data))
covar_definition <- read.table(paste(data_path, 'covar_definition_ning.csv', sep='\\'), sep=',', header=T)
covar_definition[, 2] <- tolower(covar_definition[, 2])
x <- gsub('^ +| +$', '', covar_definition[, 2], perl=T,)

#iop_bl, ct_iop_2y, age by diabetes
#boxplot
cohort <- 'mild_bil'
outcome <- 'ga_outcome'
boxplot_byDiab <- function(cohort){
  outcome <- 'ga_outcome'
  data_file<- paste('ModelData_', cohort, '_by_', outcome, '_v2.csv', sep='')
  raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
  names(raw_data)<- tolower(names(raw_data))
  
  dim(raw_data)
  na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
  raw_data <- na.omit(raw_data)
  na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
  
  pdf(file=paste('boxplot_by_diabetes_', cohort, '.pdf', sep=''), width=11, height=7)
  par(mfrow=c(1,2))
  par(pty='m')
  par(cex.main=1.3, cex.lab=1.2, cex.axis=1)
  
  i <- 'iop_bl'
  for(i in c('ct_iop_2y', 'age')){
    main.txt <- paste('Distribution by diabetes of Covariate =', i, sep='')
    diab <- raw_data$diabetes
    y <- raw_data[, i]
    boxplot(y~diab, xlab='Diabetes', ylab=i, main=main.txt)
    add <- TRUE
    plot(x=c(0, 1), y=tapply(y, as.factor(diab), mean), col=1, pch=19)
    add <- T
    text(c(0, 1), tapply(y, as.factor(diab), mean), col=2, pos=4, labels=as.character(round(tapply(y, as.factor(diab), mean)), 2))
  }
  dev.off()
  
}
boxplot_byDiab('mild_bil')
#discriptive table by diabetes of iop_bl, ct_iop_2y, age
cohort <- 'mild_bil'

DistributeTable_byDiab <- function(cohort){
    outcome <- 'ga_outcome'
    data_file<- paste('ModelData_', cohort, '_by_', outcome, '_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    raw_data <- na.omit(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    diab <- raw_data$diabetes
    ct_byDiab <- table(diab)
    for (i in c('iop_bl', 'ct_iop_2y', 'age')){
        var <- raw_data[, i]
        if(i=='iop_bl'){
            tb <- table(var, diab)
            result <- cbind(tb[, 1], tb[, 2])
            colnames(result)<- c('0', '1')
            rownames(result) <- c('iop_bl_0', 'iop_bl_1')
            write.xlsx(result , 'DistributeTable_byDiab.xlsx', sheetName=i, row.names=T, append=T, showNA=T)
            
        }else{
            mean <- tapply(var, as.factor(diab), mean)
            sd <- tapply(var, as.factor(diab), sd)
            IQR <- tapply(var, as.factor(diab), IQR)
            q <- tapply(var, as.factor(diab), quantile)
            quantile <- cbind(q[[1]][names(q[[1]]) %in% c('25%', '75%')], q[[2]][names(q[[2]]) %in% c('25%', '75%')])
            median <- tapply(var, as.factor(diab), median)
            min <- tapply(var, as.factor(diab), min)
            max <- tapply(var, as.factor(diab), max)
            #q_lv <- levels(cut2(var, g=4))
            if(i=='age'){
                q_lv <- tapply(var, as.factor(diab), function(x){levels(cut2(x, g=4))})
                iqr_box23 <- numeric()
                for(j in c(1, 2)){
                    q_lvi <- q_lv[[j]]
                    iqr_2 <- as.numeric(gsub("^\\W(\\d+)\\W(\\d+)\\W$", '\\2', q_lvi[2], perl=T))-as.numeric(gsub("^\\W(\\d+)\\W(\\d+)\\W$", '\\2', q_lvi[1], perl=T))
                    iqr_3 <- as.numeric(gsub("^\\W(\\d+)\\W(\\d+)\\W$", '\\2', q_lvi[3], perl=T))-as.numeric(gsub("^\\W(\\d+)\\W(\\d+)\\W$", '\\2', q_lvi[2], perl=T))
                    iqr_box23 <- cbind(iqr_box23, c(iqr_2, iqr_3)) 
                }
                
            }else{
                q_lv <- tapply(var, as.factor(diab), function(x){levels(cut2(x, g=4))})
                if(i=='ct_iop_2y'){
                    #freqency table for ct_iop_2y by diabetes
                    tb_1 <- table(var, diab)
                    freq.tb <- data.frame(rownames(tb_1), tb_1[, 1], tb[, 2])
                    colnames(freq.tb)<- c('Ct_iop_2y', 'diab_0', 'diab_1')
                    write.xlsx(freq.tb , 'FreqTable_byDiab.xlsx', sheetName=i, row.names=F, append=T, showNA=T)
                    
                }
                
                iqr_box23 <- numeric()
                for(j in c(1, 2)){
                    q_lvi <- q_lv[[j]]
                    if(j==2){
                        iqr_2 <- 0
                        iqr_3 <- 0
                        iqr_box23 <- cbind(iqr_box23, c(iqr_2, iqr_3))
                    }else{
                        iqr_2 <- as.numeric(gsub("^\\W(\\d+)\\W+(\\d+)\\W$", '\\2', q_lvi[2], perl=T))-as.numeric(gsub("^\\W(\\d+)\\W+(\\d+)\\W$", '\\1', q_lvi[2], perl=T))
                        iqr_3 <- as.numeric(gsub("^\\W(\\d+)\\W+(\\d+)\\W$", '\\2', q_lvi[3], perl=T))-as.numeric(gsub("^\\W(\\d+)\\W+(\\d+)\\W$", '\\1', q_lvi[3], perl=T))
                        iqr_box23 <- cbind(iqr_box23, c(iqr_2, iqr_3))
                    }
                }
                
            }
            
            
            result <- rbind(ct_byDiab, mean, median,sd, quantile, min, max, iqr_box23, IQR)
            rownames(result) <- c('N', 'Mean', 'Median', 'SD', '25th Pctl', '75th Pctl', 'Min', 'Max', '2nd Quarter Box', '3nd Quarter Box', 'IQR')
            write.xlsx(result , 'DistributeTable_byDiab.xlsx', sheetName=i, row.names=T, append=T, showNA=T)
        }
    }
    
}
DistributeTable_byDiab('mild_bil')
