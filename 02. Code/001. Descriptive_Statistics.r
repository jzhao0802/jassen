
#==========================================================================================
#  Project:  Janssen project

#    Part II: Discriptive table

#    Develop time: 2/27/2015 - .

#    Developer: Jie Zhao
#==========================================================================================
library(glmnet)
library(xlsx)

options(digits=7)

rm(list=ls())

data_path<- 'D:\\working materials\\Jassen Project\\01. Raw_data\\fromSAS'
out_path<-'D:\\working materials\\Jassen Project\\03.Output\\Feb27'
setwd(out_path)

raw_data <- read.table(paste(data_path, 'mild_bil_cv.csv', sep='\\'), head=T, sep=',')  #[1] 4332   56
# change names of all variables into lower case
names(raw_data)<- tolower(names(raw_data))
#attach(raw_data)

# qc raw data, data missing etc.
dim(raw_data)
na_check <- apply(apply(raw_data, 2, is.na), 2, sum)

cohort <- 'mild_bil'
DescriptiveTable<- function(cohort){
    comorList <- c('mi', 'cva', 'tia', 'othercvs', 'hypertension')
    
    raw_data <- read.table(paste(data_path, '\\', cohort, '_cv.csv', sep=''), head=T, sep=',')  #[1] 4332   56
    # change names of all variables into lower case
    names(raw_data)<- tolower(names(raw_data))
    siteid <- raw_data$siteid
    tb_rbind <- numeric()
    diag.v <- numeric()
    i <- comorList[1]
    for(i in comorList){
        vct <- raw_data[, i]
        if(i %in% comorList[1:3]){
            vct <- ifelse(regexpr('true', vct, ignore.case=T) >0, i, 0)
        }
        if(i == comorList[4]){
            vct1 <- as.vector(vct)
            vct2 <- ifelse(grepl('hypertension', vct1) ==0, vct1, "")
            vct <- vct2
        }
        if(i == comorList[5]){
            vct <- ifelse(!is.na(vct), vct, 0)
        }
        if (i %in% comorList[5]){
            tb <- table(vct, siteid)
            #tb.df <- as.data.frame(tb)
            #x <- as.numeric(as.vector(tb.df[, 1]))
            #tb.df$Freq <- ifelse(x==2, 2*tb.df$Freq, tb.df$Freq)
            tb[3, ] <- tb[3, ]*2
            sum <- apply(tb[2:3, ], 2, sum)
            tb1 <- tb[1:2, ]
            num <- tb1[2, ]
            num <- c(num, total.num = sum(num))
            prop <- prop.table(tb1, 2)[2, ]
            prop <- c(prop, total.prop = sum(tb1[2, ])/sum(tb1))
            tb_rbind <- rbind(tb_rbind, c(num, prop))
            diag <- i
            diag.v <- c(diag.v, diag)
            
        }else if(i %in% comorList[4]){
            tb <- table(vct, siteid)
            num <- tb[-1, ]
            total.num <- apply(num, 1, sum)
            num <- cbind(num, total.num)
            total2.num <- apply(num, 2, sum)
            num1 <- rbind(total2.num, num)
            prop <- prop.table(tb, 2)[-1, ]
            total.prop <- total.num/sum(tb)
            prop <- cbind(prop, total.prop)
            total2.prop <- apply(prop, 2, sum)
            prop1 <- rbind(total2.prop, prop)
            tb1 <- cbind(num1, prop1)
            tb1_order <- tb1[rev(order(tb1[,5])),]
            tb2 <- tb1_order[1:3,]       
            tb_rbind <- rbind(tb_rbind, tb2)
            diag <- c('OtherCVS', rownames(tb2)[-1])
            diag.v <- c(diag.v, diag)
        }else{
            tb <- table(vct, siteid)
            num <- tb[2, ]
            diag <- i
            num <- c(num, total.num = sum(num))
            prop <- prop.table(tb, 2)[2, ]
            prop <- c(prop, total.prop = sum(tb[2, ])/sum(tb))
            tb1 <- c(num, prop)
            tb_rbind <- rbind(tb_rbind, c( num, prop))
            diag.v <- c(diag.v, diag)
            
        }
        
    }
    rownames(tb_rbind) <- diag.v
    write.xlsx(tb_rbind, file=paste(out_path, 'Descriptive_Table_CV.xlsx', sep='\\'), sheetName=cohort, row.names=T, append=T, showNA=T)
    return(tb_rbind)
    
}
output1 <- DescriptiveTable('mild_bil')
output2 <- DescriptiveTable('mild_uni')

DescriptiveTable_ga <- function(cohort){
    comorList <- c('mi', 'othercvs', 'hypertension')
    raw_data <- read.table(paste(data_path, '\\', cohort, '_cv.csv', sep=''), head=T, sep=',')  #[1] 4332   56
    # change names of all variables into lower case
    names(raw_data)<- tolower(names(raw_data))
    siteid <- raw_data$siteid
    tb_rbind <- numeric()
    diag.v <- numeric()
    i <- comorList[3]
    for(i in comorList){
        vct <- raw_data[, i]
        if(i %in% comorList[1]){
            vct <- ifelse(regexpr('true', vct, ignore.case=T) >0, i, 0)
        }
        if(i == comorList[2]){
            vct1 <- as.vector(vct)
            vct2 <- ifelse(grepl('hypertension', vct1) ==0, vct1, "")
            vct <- vct2
        }
        if(i == comorList[3]){
            vct <- ifelse(!is.na(vct), vct, 0)
        }
        if (i %in% comorList[3]){
            tb <- table(vct, siteid)
            #tb.df <- as.data.frame(tb)
            #x <- as.numeric(as.vector(tb.df[, 1]))
            #tb.df$Freq <- ifelse(x==2, 2*tb.df$Freq, tb.df$Freq)
            tb[3, ] <- tb[3, ]*2
            sum <- apply(tb[2:3, ], 2, sum)
            tb1 <- tb[1:2, ]
            num <- tb1[2, ]
            num <- c(num, total.num = sum(num))
            prop <- prop.table(tb1, 2)[2, ]
            prop <- c(prop, total.prop = sum(tb1[2, ])/sum(tb1))
            tb_rbind <- rbind(tb_rbind, c(num, prop))
            diag <- i
            diag.v <- c(diag.v, diag)
            
        }else if (i %in%comorList[1]){
            tb <- table(vct, siteid)
            num <- tb[2, ]
            num <- c(num, total.num = sum(num))
            prop <- prop.table(tb, 2)[2, ]
            prop <- c(prop, total.prop = sum(tb[2, ])/sum(tb))
            tb_rbind <- rbind(tb_rbind, c(num, prop))
            diag <- i
            diag.v <- c(diag.v, diag)
        }else{
            tb <- table(vct, siteid)
            num <- tb[-1, ]
            total.num <- apply(num, 1, sum)
            num <- cbind(num, total.num)
            total2.num <- apply(num, 2, sum)
            num1 <- rbind(total2.num, num)
            prop <- prop.table(tb, 2)[-1, ]
            total.prop <- total.num/sum(tb)
            prop <- cbind(prop, total.prop)
            total2.prop <- apply(prop, 2, sum)
            prop1 <- rbind(total2.prop, prop)
            tb1 <- cbind(num1, prop1)
            tb1_order <- tb1[rev(order(tb1[,5])),]
            tb2 <- tb1_order[1:3,]
            #rownames(tb2) <- NULL
            tb_rbind <- rbind(tb_rbind, tb2)
            diag <- c('OtherCSV', rownames(tb2)[-1])
            diag.v <- c(diag.v, diag)
        }
        
    }
    rownames(tb_rbind) <- diag.v
    write.xlsx(tb_rbind, file=paste(out_path, 'Descriptive_Table_CV.xlsx', sep='\\'), sheetName=cohort, row.names=T, append=T, showNA=T)
    return(tb_rbind)
    
}
output3 <- DescriptiveTable_ga('ga_all')
write.xlsx(output3, file=paste(out_path, 'Descriptive_Table_CV.xlsx', sep='\\'), sheetName=cohort, row.names=T, append=T, showNA=T)


hypertension <- ifelse(othercvs=='hypertension', othercvs, 0)
tb1 <- table(hypertension, siteid)

othercvs <- ifelse(othercvs!='hypertension', 'othercvs', 0)
tb1 <- table(othercvs, siteid)

tb <- table(other)
nm <- names(which.max(tb[-1]))
other1 <- ifelse(other==nm, nm, 0)
tb1 <- table(other1, siteid)
