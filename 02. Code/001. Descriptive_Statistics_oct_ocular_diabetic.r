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

#freq <- aggregate(as.numeric(raw_data$descr_diag), by=list(Site=raw_data$siteid, Type=raw_data$type_diag), sum)
#freq <- tapply(as.numeric(raw_data$descr_diag), list(Site=raw_data$siteid, Type=raw_data$type_diag), count)

tb <- table(raw_data$siteid, raw_data$type_diag, raw_data$descr_diag)
tb.df <- as.data.frame(tb)
freq.diag <- table(raw_data$descr_diag)
diag.max <- names(which.max(freq.diag))

tb1 <- table(raw_data$type_diag, raw_data$siteid)

tb2 <- table(type_diag=raw_data$type_diag,  descr_diag=raw_data$descr_diag, site=raw_data$siteid)

str(tb2)

ftable(tb2, row.vars=1:2, col.vars=3)

cohort <- 'mild_uni'
DescriptiveTable_ocular <- function(cohort){
    raw_data <- read.table(paste(data_path, '\\', cohort, '_ocular.csv', sep=''), head=T, sep=',')  #[1] 4332   56
    # change names of all variables into lower case
    names(raw_data)<- tolower(names(raw_data))
    tb2 <- table(type_diag=raw_data$type_diag,  descr_diag=raw_data$descr_diag, site=raw_data$siteid)
    str(tb2)
    
    ftable(tb2, row.vars=1:2, col.vars=3)
    
    df1 <- as.data.frame(tb2[1,,])[rev(order(apply(as.data.frame(tb2[1,,]),1,sum))),]
    df2 <- as.data.frame(tb2[2,,])[rev(order(apply(as.data.frame(tb2[2,,]),1,sum))),]
    df <- rbind(df1, df2)
    
    write.xlsx(df, file=paste(out_path, 'Descriptive_Table_ocular.xlsx', sep='\\'), sheetName=cohort, row.names=T, append=T, showNA=T)
}

DescriptiveTable_ocular_v2 <- function(cohort){
    raw_data <- read.table(paste(data_path, '\\', cohort, '_ocular.csv', sep=''), head=T, sep=',')  #[1] 4332   56
    # change names of all variables into lower case
    names(raw_data)<- tolower(names(raw_data))
    type_lst <- levels(raw_data$type_diag)
    type <- type_lst[1]
    tb.1 <- table(raw_data[raw_data$type_diag==type, 'descr_diag'], raw_data[raw_data$type_diag==type, 'siteid'])
    flag.1 <- rep(type, dim(tb.1)[1])
    type <- type_lst[2]
    tb.2 <- table(raw_data[raw_data$type_diag==type, 'descr_diag'],  raw_data[raw_data$type_diag==type, 'siteid'])
    flag.2 <- rep(type, dim(tb.1)[1])
    flag <- c(flag.1, flag.2)
    tb <- rbind(tb.1, tb.2)
    p <- prop.table(tb, 2)
    n.p <- cbind(tb, p)
    num.total.1 <- apply(n.p[flag==type_lst[1],1:4], 2, sum)
    num.total.2 <- apply(n.p[flag==type_lst[2],1:4], 2, sum)
    prop.total.1 <- num.total.1/sum(n.p[, 1:4])
    prop.total.2 <- num.total.2/sum(n.p[, 1:4])
    row.1 <- c(num.total.1, prop.total.1)
    row.2 <- c(num.total.2, prop.total.2)
    add.1 <- rbind(row.1, n.p[flag==type_lst[1],])
    add.2 <- rbind(row.2, n.p[flag==type_lst[2],])
    out.1 <- data.frame(Type_Diag=c(type_lst[1],flag.1), Descr_Diag=c(type_lst[1], rownames(tb.1)), add.1)
    out.2 <- data.frame(Type_Diag=c(type_lst[2],flag.2), Descr_Diag=c(type_lst[2], rownames(tb.2)), add.2)
    out <- rbind(out.1, out.2)
    num.tot <- apply(out[3:6], 1, sum)
    prop.tot <- num.tot/sum(n.p[,1:4])
    out.f <- as.data.frame(cbind(out, num.total=num.tot, prop.total=prop.tot))
    #order
    c <- out.f[out.f$Type_Diag==type_lst[1], ]
    c.order <- c[rev(order(c$num.total)), ][1:7, ]
    g <- out.f[out.f$Type_Diag==type_lst[2], ]
    g.order <- g[rev(order(g$num.total)), ][1:6, ]
    output <- rbind(c.order, g.order)
    
    write.xlsx(output, file=paste(out_path, 'Descriptive_Table_ocular.xlsx', sep='\\'), sheetName=cohort, row.names=F, append=T, showNA=T)
}

DescriptiveTable_ocular_v2('mild_bil')
DescriptiveTable_ocular_v2('mild_uni')
DescriptiveTable_ocular_v2('ga_all')



cohort <- 'ga_all'
DescriptiveTable_oct <- function(cohort){
    
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
nmiss.num <- sum(raw_data$nmiss)
nmiss.prop <- nmiss.num/dim(raw_data)[1]
tb <- table(raw_data$nmiss, raw_data$siteid)
tb.total <- apply(tb, 1, sum)
tb1 <- cbind(tb, tb.total)
prop <- prop.table(tb)
prop.total <- tb.total/sum(tb)
prop1 <- cbind(prop, prop.total)
output_nmiss <- cbind(Nmiss=c('Miss', 'Nmiss'), tb1, prop1)

#write.xlsx(output_nmiss, file=paste(out_path, 'Descriptive_Table_NMiss_oct.xlsx', sep='\\'), sheetName=cohort, row.names=F, append=T, showNA=T)
i <- diag_lst[1]
output_rbind <- numeric()
    for(i in diag_lst){
        var <- na.omit(raw_data[, i])
        site <- raw_data[!is.na(raw_data[, i]), 'siteid']
        var1 <- ifelse(is.na(var), 0, 1)
        cnt <- aggregate(var1, by=list(Site=site), sum)
        mean <- aggregate(var1, by=list(Site=site), mean)
        sd <- aggregate(var1, by=list(Site=site), sd)
        median <- aggregate(var1, by=list(Site=site), median)
        iqr <- aggregate(var1, by=list(Site=site), IQR)
        min <- aggregate(var1, by=list(Site=site), min)
        max <- aggregate(var1, by=list(Site=site), max)
        output <- rbind(N=cnt[, 2], Mean=mean[, 2], SD=sd[, 2], Median=median[, 2], IQR=iqr[, 2], Min=min[, 2], Max=max[,2])
        output.df <- data.frame(Variable=rep(i, 7), output)
        colnames(output.df) <- cnt[, 2]
        output_rbind <- rbind(output_rbind, output.df)
        
        #write.xlsx(output, file=paste(out_path, 'Descriptive_Table_2_oct.xlsx', sep='\\'), sheetName=cohort, row.names=T, append=T, showNA=T)
        
    }
}
DescriptiveTable_oct('mild_bil')
DescriptiveTable_oct('mild_uni')
DescriptiveTable_oct('ga_all')


cohort='mild_uni'
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
    nmiss.num <- sum(raw_data$nmiss)
    nmiss.prop <- nmiss.num/dim(raw_data)[1]
    tb <- table(raw_data$nmiss, raw_data$siteid)
    tb.total <- apply(tb, 1, sum)
    tb1 <- cbind(tb, tb.total)
    prop <- prop.table(tb)
    prop.total <- tb.total/sum(tb)
    prop1 <- cbind(prop, prop.total)
    output_nmiss <- cbind(Nmiss=c('Miss', 'Nmiss'), tb1, prop1)
    
    #write.xlsx(output_nmiss, file=paste(out_path, 'Descriptive_Table_NMiss_oct.xlsx', sep='\\'), sheetName=cohort, row.names=F, append=T, showNA=T)
    i <- diag_lst[3]
    #output_rbind <- numeric()
    for(i in diag_lst){
        cnt<-numeric()
        mean<-numeric()
        sd<-numeric()
        median<-numeric()
        iqr<-numeric()
        min<-numeric()
        max<-numeric()
    
        var <- na.omit(raw_data[,i])
        site <- raw_data[!is.na(raw_data[, i]), 'siteid']
        if(length(var)==0){
            cnt<-rep(0,4)
            name<-levels(site)
            mean<-rep(0,4)
            sd<-rep(0,4)
            median<-rep(0,4)
            iqr<-rep(0,4)
            min<-rep(0,4)
            max<-rep(0,4)  
        }else{
        var1 <- ifelse(is.na(raw_data[,i]), 0, 1)
        cnt1 <- aggregate(var1, by=list(Site=raw_data$siteid), sum)
        mean1 <- aggregate(var, by=list(Site=site), mean)
        sd1<- aggregate(var, by=list(Site=site), sd)
        median1 <- aggregate(var, by=list(Site=site), median)
        iqr1 <- aggregate(var, by=list(Site=site), IQR)
        min1 <- aggregate(var, by=list(Site=site), min)
        max1 <- aggregate(var, by=list(Site=site), max)
        
        cnt<-cnt1[,2]
        name<-as.vector(cnt1[,1])
        mean<-mean1[,2]
        sd<-sd1[,2]
        median<-median1[,2]
        iqr<-iqr1[,2]
        min<-min1[,2]
        max<-max1[,2] 

        
        }
        output <- rbind(N=cnt, Mean=mean, SD=sd, Median=median, IQR=iqr, Min=min, Max=max)
        output.df <- data.frame(Variable=rep(i, 7), output)
        colnames(output.df) <- c('Variable', name)
        #output_rbind <- rbind(output_rbind, output.df)
        
        write.xlsx(output.df, file=paste(out_path, 'Descriptive_Table_2_oct_v3.xlsx', sep='\\'), sheetName=paste(cohort, i, sep='_'), row.names=T, append=T, showNA=T)
        
    }
}
DescriptiveTable_oct_v3('mild_bil')
DescriptiveTable_oct_v3('mild_uni')
DescriptiveTable_oct_v3('ga_all')


cnt<-cnt1[,2]
name<-as.vector(cnt1[,1])
mean<-c(0, mean1[,2])
sd<-c(0, sd1[,2])
median<-c(0, median1[,2])
iqr<-c(0, iqr1[,2])
min<-c(0, min1[,2])
max<-c(0, max1[,2] )

cnt<-cnt1[,2]
name<-as.vector(cnt1[,1])
mean<-c(rep(0, 3), mean1[,2])
sd<-c(rep(0, 3), sd1[,2])
median<-c(rep(0, 3), median1[,2])
iqr<-c(rep(0, 3), iqr1[,2])
min<-c(rep(0, 3), min1[,2])
max<-c(rep(0, 3), max1[,2] )

cnt<-cnt1[,2]
name<-as.vector(cnt1[,1])
mean<-c(rep(0, 2), mean1[,2])
sd<-c(rep(0, 2), sd1[,2])
median<-c(rep(0, 2), median1[,2])
iqr<-c(rep(0, 2), iqr1[,2])
min<-c(rep(0, 2), min1[,2])
max<-c(rep(0, 2), max1[,2] )




}

output1 <- DescriptiveTable_oct("mild_bil")
output2 <- DescriptiveTable_oct("mild_uni")

output <- rbind(output1, output2)
write.xlsx(output, file=paste(out_path, 'Descriptive_Table_oct.xlsx', sep='\\'), row.names=T, append=T, showNA=T)


cohort<- 'mild_uni'
DescriptiveTable_diab <- function(cohort){
    raw_data <- read.table(paste(data_path, '\\', cohort, '_diab.csv', sep=''), head=T, sep=',')  #[1] 4332   56
    # change names of all variables into lower case
    names(raw_data)<- tolower(names(raw_data))
    tb <- table(Isdiabetic=raw_data$isdiabetic, Site =raw_data$siteid)
    #num <- tb[2, ]
    num1 <- cbind(tb, total.num = apply(tb, 1, sum))
    prop <- prop.table(tb, 2)
    prop1 <- cbind(prop, total.prop = apply(tb, 1, sum)/sum(tb))
    tb.r<- cbind(num1, prop1)
    row.names(tb.r)[2] <- 'Diabetic'
    write.xlsx(tb.r, file=paste(out_path, 'Descriptive_Table_diab.xlsx', sep='\\'), sheetName=cohort, row.names=T, append=T, showNA=T)
    return(tb.r)
}
output1 <- DescriptiveTable_diab('mild_bil')
output2 <- DescriptiveTable_diab('mild_uni')













