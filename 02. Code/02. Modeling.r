
#==========================================================================================
#  Project:  Janssen project

#    Part II: Modelling (elestic-net regression with 10 folds cross validation)

#    Develop time: 2/2/2015 - .

#	Developer: Jie Zhao
#==========================================================================================
library(glmnet)
library(xlsx)

options(digits=7)

data_path<- 'D:\\working materials\\Jassen Project\\01. Raw_data'
out_path<-'D:\\working materials\\Jassen Project\\03.Output'
setwd(out_path)



#data QC on missing, constant for 4 cohort (ga_bil ga_all mild_bil mild_uni) without missing data
covar_definition <- read.table(paste(data_path, 'covar_definition.csv', sep='\\'), sep=',', head=T)
cohort <- 'mild_uni'
table_check_byBinaryCovar <- function(cohort){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data_omitMiss <- na.omit(raw_data)
    var_list <- names(raw_data_omitMiss)
    response_list <- c('wet_outcome', 'ga_outcome')
    covar_list <- var_list[(length(var_list)-12):length(var_list)]
    binary_flag <-  lapply(raw_data_omitMiss[, covar_list], function(x){length(levels(as.factor(x))) == 2})
    binary_var_list <- covar_list[unlist(binary_flag)]
    
    key <- numeric()
    pos_pos_lst <- numeric()
    pos_pos_numeric_lst <- numeric()
    resp <- 'wet_outcome'
    for(resp in response_list){
        response <- raw_data_omitMiss[, resp]
        num_response_1 <- sum(response)
        num_response_0 <- sum(1-response)
        for(i in binary_var_list){
            eval(parse(text=paste('var <- raw_data_omitMiss$', i, sep='')))
            
            tb <- table(var, response)
            pos <- sum(tb[2,])
            neg <- sum(tb[1,])
            pos_pos <- tb[2,2]
            covar1_response1 <- tb[2,2]
            covar0_response1 <- tb[1,2]
            covar1_response0 <- tb[2,1]
            covar0_response0 <- tb[1,1]
            odds_ratio_my <- ((covar1_response1/sum(tb[1, ]))/(covar1_response0/sum(tb[1, ])))/((covar0_response1/sum(tb[2, ]))/(covar0_response0/sum(tb[2, ])))    #odds_ratio=odds(x==1)/odds(x==0) = ((p(Y=1|x=1)/p(Y=0|x=1))/(p(Y=1|x=0)/p(Y=0|x=0))
            #contigency_table<- matrix(c(covar1_response1, covar0_response1, covar1_response0, covar0_response0), nc=2, byrow=T)
            #association_test<- fisher.test(contigency_table , alternative = "two.sided")
            #association_test <- fisher.test(x=as.factor(var), y=as.factor(response), alternative='two.sided')
            association_test <- fisher.test(x=as.factor(response), y=as.factor(var), alternative='two.sided')
            
            p_value<- association_test$p.value
            odds_ratio<- as.vector(association_test$estimate)
            
            
            key <- rbind(key, c(cohort=cohort, response_variable=resp, binary_variable=i))
            pos_pos_numeric_lst <- rbind(pos_pos_numeric_lst, c(num_response_0=num_response_0, num_response_1=num_response_1, 
                                                                num_of_BinaryCovar_0=neg, num_of_BinaryCovar_1=pos, num_of_BinaryCovar_1_response_1=pos_pos, odds_ratio=odds_ratio, p_value=p_value, odds_ratio_my=odds_ratio_my))
            #pos_pos_lst <- rbind(pos_pos_lst, c(cohort=cohort, response_variable=resp, binary_variable=i, num_response_0=num_response_0, num_response_1=num_response_1, 
            #                                   num_of_BinaryCovar_0=neg, num_of_BinaryCovar_1=pos, num_of_BinaryCovar_1_response_1=pos_pos))
        }
    }
    #tb_summary <- as.data.frame(pos_pos_lst)
    tb_summary <- as.data.frame(cbind(data.frame(key), data.frame(pos_pos_numeric_lst)))
    tb_summary$Definition <- covar_definition[match(tb_summary$binary_variable, covar_definition[, 1]), 2]
    return(tb_summary)
}

table_check_byBinaryCovar_1 <- table_check_byBinaryCovar('mild_bil')
table_check_byBinaryCovar_2 <- table_check_byBinaryCovar('mild_uni')
table_check_byBinaryCovar <- rbind(table_check_byBinaryCovar_1, table_check_byBinaryCovar_2)
#qc
sum(table_check_byBinaryCovar[, c(4, 5)]) == sum(table_check_byBinaryCovar[, c(6, 7)])
odds_ratio_vct <- table_check_byBinaryCovar[, dim(table_check_byBinaryCovar)[2]-2]
focus <- table_check_byBinaryCovar[odds_ratio_vct == max(odds_ratio_vct) | odds_ratio_vct == min(odds_ratio_vct) , ]
write.csv(table_check_byBinaryCovar, 'StatDiscription_byBinaryCovar_mild.csv')
write.table(table_check_byBinaryCovar, 'StatDiscription_byBinaryCovar_mild.txt', sep='\t\t', quote=F, row.names=F)
#extract the lines including 0
var_without_pos_pos <- table_check_byVar_mild[table_check_byVar_mild[,4] ==0 | table_check_byVar_mild[,5]== 0,]
var_without_pos_pos_list <- unique(as.vector(var_without_pos_pos$variable)) # convert factor vector to charactor vector
write.csv(var_without_pos_pos, 'variable_without_pos_pos.csv')
#convert factor vector into numeric vector 
for_calculate <- as.numeric(as.vector(table_check_byVar_mild[, 4]))

#check the continuous variable distribution on 2 response variables in 2 cohort datasets
cohort <- 'mild_uni'
table_check_byContiVar <- function(cohort){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data_omitMiss <- na.omit(raw_data)
    var_list <- names(raw_data_omitMiss)
    response_list <- c('wet_outcome', 'ga_outcome')
    covar_list <- var_list[(length(var_list)-12):length(var_list)]
    conti_flag <-  lapply(raw_data_omitMiss[, covar_list], function(x){length(levels(as.factor(x))) > 2})
    conti_var_list <- covar_list[unlist(conti_flag)]
    
    
    resp <- 'ga_outcome'
    for(resp in response_list){
        response <- raw_data_omitMiss[, resp]
        #i <- conti_var_list[5]
        tb.df.rbind <- numeric()
        i <- conti_var_list[6]
        for(i in conti_var_list){
            eval(parse(text=paste('var <- raw_data_omitMiss$', i, sep='')))
            
            tb <- table(var, response)
            tb.df <- as.data.frame(tb)
            
            tb.df$label <- rep(i, dim(tb.df)[1])
            tb.df1 <- data.frame(variable=tb.df$label, varaible_value=tb.df$var, response=tb.df$response, freq=tb.df$Freq)
            tb.df.rbind <- rbind(tb.df.rbind, tb.df1)
        }
        write.csv(tb.df.rbind, paste('table_check_byContiVar_', cohort, '_', resp, '.csv', sep=''))
        write.table(tb.df.rbind, paste('table_check_byContivar_', cohort, '-', resp, '.txt', sep=''), sep='\t\t', quote=F, row.names=F)
    }
    
}
table_check_byContiVar('mild_bil')
table_check_byContiVar('mild_uni')


cohort <- 'mild_bil'
table_check_byContiVar_writeLine <- function(cohort, by_response){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data_omitMiss <- na.omit(raw_data)
    var_list <- names(raw_data_omitMiss)
    response_list <- c('wet_outcome', 'ga_outcome')
    covar_list <- var_list[(length(var_list)-12):length(var_list)]
    conti_flag <-  lapply(raw_data_omitMiss[, covar_list], function(x){length(levels(as.factor(x))) > 2})
    conti_var_list <- covar_list[unlist(conti_flag)]
    if(by_response==FALSE){
        OUT<- file(paste('frequency_table_', cohort, sep='' ) , 'w')
        for(i in conti_var_list){
            eval(parse(text = paste('x<- raw_data_omitMiss$' , i , sep='')))
            re<- table(x)
            nm<- rownames(re)
            writeLines(paste(i, 'Frequency' , sep='\t\t') , OUT)
            writeLines(paste(rep('-' , 100) , collapse='') , OUT)
            for(j in 1:dim(re)[1]){
                writeLines(paste(nm[j], re[j], collapse='\t\t') , OUT)
            }
            writeLines('' , OUT)
        }
        close(OUT)
        
    }else{
        resp <- 'wet_outcome'
        for(resp in response_list){
            response <- raw_data_omitMiss[, resp]
            #i <- conti_var_list[5]
            OUT<- file(paste('frequency_table_', cohort, '_by_', resp, sep='' ) , 'w')
            for(i in conti_var_list){
                eval(parse(text = paste('x<- raw_data_omitMiss$' , i , sep='')))
                re<- table(x, response)
                nm<- rownames(re)
                writeLines(paste(i, 'Frequency_by_Response=0', 'Frequency_by_Response=1' , sep='\t\t') , OUT)
                writeLines(paste(rep('-' , 130) , collapse='') , OUT)
                for(j in 1:dim(re)[1]){
                    writeLines(paste(nm[j] , re[j, 1] , re[j, 2] , sep='\t\t') , OUT)
                }
                writeLines('' , OUT)
            }
            close(OUT)
        }
        
    }
}
table_check_byContiVar_writeLine('mild_bil', by_response=FALSE)
table_check_byContiVar_writeLine('mild_uni', by_response=FALSE)
table_check_byContiVar_writeLine('mild_bil', by_response=TRUE)
table_check_byContiVar_writeLine('mild_uni', by_response=TRUE)



table_check_byContiVar_2 <- function(cohort){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data_omitMiss <- na.omit(raw_data)
    var_list <- names(raw_data_omitMiss)
    response_list <- c('wet_outcome', 'ga_outcome')
    covar_list <- var_list[(length(var_list)-12):length(var_list)]
    conti_flag <-  lapply(raw_data_omitMiss[, covar_list], function(x){length(levels(as.factor(x))) > 2})
    conti_var_list <- covar_list[unlist(conti_flag)]
    
    
    #i <- conti_var_list[5]
    tb.df.rbind <- numeric()
    i <- conti_var_list[6]
    for(i in conti_var_list){
        eval(parse(text=paste('var <- raw_data_omitMiss$', i, sep='')))
        
        tb <- table(var)
        tb.df <- as.data.frame(tb)
        
        tb.df$label <- rep(i, dim(tb.df)[1])
        tb.df1 <- data.frame(variable=tb.df$label, varaible_value=tb.df$var, freq=tb.df$Freq)
        tb.df.rbind <- rbind(tb.df.rbind, tb.df1)
        write.csv(tb.df.rbind, paste('table_check_byContiVar_', cohort, '.csv', sep=''))
        write.table(tb.df.rbind, paste('table_check_byContivar_', cohort, '.txt', sep=''), sep='\t\t', quote=F, row.names=F)
    }
    
}
table_check_byContiVar_1('mild_bil')
table_check_byContiVar_1('mild_uni')


cohort <- 'mild_bil'
completeSeparation_check_byContiVar <- function(cohort){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data_omitMiss <- na.omit(raw_data)
    var_list <- names(raw_data_omitMiss)
    response_list <- c('wet_outcome', 'ga_outcome')
    covar_list <- var_list[(length(var_list)-12):length(var_list)]
    conti_flag <-  lapply(raw_data_omitMiss[, covar_list], function(x){length(levels(as.factor(x))) > 2})
    conti_var_list <- covar_list[unlist(conti_flag)]
    
    
    resp <- 'wet_outcome'
    table_numeric <- numeric()
    table_key <- numeric()
    for(resp in response_list){
        response <- raw_data_omitMiss[, resp]
        #        i <- conti_var_list[5]
        for(i in conti_var_list){
            complete_separate <- 0
            eval(parse(text=paste('var <- raw_data_omitMiss$', i, sep='')))
            min_resp0 <- min(var[response == 0])
            min_resp1 <- min(var[response == 1])
            max_resp0 <- max(var[response == 0])
            max_resp1 <- max(var[response == 1])
            if(max_resp0 <= min_resp1 | max_resp1 <= min_resp0){
                complete_separate <- 1
            }else{
                complete_separate <- 0
            }
            table_numeric <- rbind(table_numeric, c(min_response_0=min_resp0, min_response_1=min_resp1, max_response_0=max_resp0, max_response_1=max_resp1, complete_separate=complete_separate))
            table_key <- rbind(table_key, c(cohort=cohort, response=resp, conti_vairable=i))
        }
    }
    table_f <- cbind(data.frame(table_key), data.frame(table_numeric))
    return(table_f)
}
completeSeparation_check_byContiVar_1 <- completeSeparation_check_byContiVar('mild_bil')
completeSeparation_check_byContiVar_2 <- completeSeparation_check_byContiVar('mild_uni')
completeSeparation_check_byContiVar <- rbind(completeSeparation_check_byContiVar_1, completeSeparation_check_byContiVar_2)
write.csv(completeSeparation_check_byContiVar, 'completeSeparation_check_byContiVar.csv')
write.table(completeSeparation_check_byContiVar, 'completeSeparation_check_byContiVar.txt', quote=F, sep='\t\t')

cohort <- 'mild_uni'
table_check_byContiVar2 <- function(cohort){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data_omitMiss <- na.omit(raw_data)
    var_list <- names(raw_data_omitMiss)
    response_list <- c('wet_outcome', 'ga_outcome')
    covar_list <- var_list[(length(var_list)-12):length(var_list)]
    conti_flag <-  lapply(raw_data_omitMiss[, covar_list], function(x){length(levels(as.factor(x))) > 2})
    conti_var_list <- covar_list[unlist(conti_flag)]
    key <- numeric()
    tb_numeric <- numeric()
    #resp <- 'wet_outcome'
    for(resp in response_list){
        response <- raw_data_omitMiss[, resp]
        #i <- conti_var_list[5]
        for(i in conti_var_list){
            eval(parse(text=paste('var <- raw_data_omitMiss$', i, sep='')))
            mean <- round(mean(var), 2)
            sd <- round(sd(var), 2)
            tb <- table(var, response)
            mean_onResponse <- round(as.vector(tapply(var, response, mean))[2], 2)
            sd_onResponse <- round(as.vector(tapply(var, response, sd))[2], 2)
            mean_onResponse0 <- round(as.vector(tapply(var, response, mean))[1], 2)
            sd_onResponse0 <- round(as.vector(tapply(var, response, sd))[1], 2)
            
            key <- rbind(key, c(cohort=cohort, response_variable=resp, conti_variable=i))
            tb_numeric <- rbind(tb_numeric, c(mean_ContiCovar_1=mean, sd_ContiCovar_1=sd, mean_ContiCovar_1_response_1=mean_onResponse, sd_ContiCovar_1_response_1=sd_onResponse,
                                              mean_ContiCovar_1_response_0=mean_onResponse0, sd_ContiCovar_1_response_0=sd_onResponse0  #add for version2(v2)
            ))
        }
    }
    tb_summary <- data.frame(data.frame(key), data.frame(tb_numeric))
    tb_summary$Definition <- covar_definition[match(tb_summary$binary_variable, covar_definition[, 1]), 2]
    return(tb_summary)
}
table_check_byContiVar_1 <- table_check_byContiVar2('mild_bil')
table_check_byContiVar_2 <- table_check_byContiVar2('mild_uni')
table_check_byContiVar <- rbind(table_check_byContiVar_1, table_check_byContiVar_2)
write.csv(table_check_byContiVar, 'StatDiscription_byContiCovar_mild.csv')
write.table(table_check_byContiVar, 'StatDiscription_byContiCovar_mild.txt', quote=F, row.names=F, sep='\t\t')
write.csv(table_check_byContiVar, 'StatDiscription_byContiCovar_mild_v2.csv')
write.table(table_check_byContiVar, 'StatDiscription_byContiCovar_mild_v2.txt', quote=F, row.names=F, sep='\t\t')

#histogram for continous variables (6 continous variables)
cohort <- 'mild_uni'
table_check_byContiVar_hist <- function(cohort, by_response){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data_omitMiss <- na.omit(raw_data)
    var_list <- names(raw_data_omitMiss)
    response_list <- c('wet_outcome', 'ga_outcome')
    covar_list <- var_list[(length(var_list)-12):length(var_list)]
    conti_flag <-  lapply(raw_data_omitMiss[, covar_list], function(x){length(levels(as.factor(x))) > 2})
    conti_var_list <- covar_list[unlist(conti_flag)]
    if(by_response==FALSE){
        pdf(paste('histogram_', cohort, '.pdf', sep=''))
        par(mfrow=c(3,2))
        par(pty='m')
        par(cex.lab=1.2, cex.axis=0.9)
        i <- conti_var_list[1]
        for(i in conti_var_list){
            eval(parse(text=paste('var <- raw_data_omitMiss$', i, sep='')))
            
            main.txt <- paste('Distribution of Covariate =', i, sep='')
            xlab.txt <- paste('variable = ', i, sep='')
            bin.num <- 10
            breaks.txt <- seq(from=min(var), length.out=bin.num, to=max(var))
            breaks.label <- round(seq(from=min(var), length.out=bin.num, to=max(var)), 1)
            
            hist(var, breaks=breaks.txt,freq=T, main=main.txt, xlab=xlab.txt, xlim=range(breaks.txt), xaxt="n")
            hist(var, main=main.txt, xlab=xlab.txt, xlim=range(breaks.txt))
            axis(side=1, at=breaks.label, labels=breaks.label)
            
            
        }
        dev.off()
        
    }else{
        #resp <- 'wet_outcome'
        for(resp in response_list){
            response <- raw_data_omitMiss[, resp]
            pdf(paste('histogram_', cohort, '_by_', resp, '.pdf', sep=''))
            par(mfrow=c(3,2))
            par(mfrow=c(3,2))
            par(pty='m')
            par(cex.lab=1.2, cex.axis=0.9)
            
            #i <- conti_var_list[1]
            for(i in conti_var_list){
                eval(parse(text=paste('var <- raw_data_omitMiss$', i, sep='')))
                var_response1 <- var[response==1]
                
                main.txt <- paste('Distribution by ', resp, ' = 1', sep='')
                xlab.txt <- paste('variable = ', i, sep='')
                bin.num <- 10
                breaks.txt <- seq(from=min(var_response1), length.out=bin.num, to=max(var_response1))
                breaks.label <- round(seq(from=min(var_response1), length.out=bin.num, to=max(var_response1)), 1)
                #breaks.txt <- seq(from=min(var_response1), by=round((max(var_response1)-min(var_response1))/bin.num), to=max(var_response1))
                
                hist(var_response1, breaks=breaks.txt, freq=T, main=main.txt, xlab=xlab.txt, xlim=range(breaks.txt), xaxt="n")
                #hist(var_response1, breaks=breaks.txt, main=main.txt, xlab=xlab.txt, xlim=range(breaks.txt))
                #axis(side=1, at=breaks.txt, labels=seq(from=min(var_response1), length.out=10, to=max(var_response1), 2))
                axis(side=1, at=breaks.label, labels=breaks.label)
                
            }
            dev.off()
        }
        
    }
    
}
table_check_byContiVar_hist('mild_uni', by_response=FALSE)
table_check_byContiVar_hist('mild_bil', by_response=FALSE)
table_check_byContiVar_hist('mild_uni', by_response=TRUE)
table_check_byContiVar_hist('mild_bil', by_response=TRUE)

#check constant variable in the 4 cohort without missing data
constant <- function(cohort){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data_omitMiss <- na.omit(raw_data)
    var_list <- names(raw_data_omitMiss)
    
    constant_flag <- lapply(raw_data_omitMiss, function(x){length(levels(as.factor(x))) == 1})
    constant_var_list <- var_list[unlist(constant_flag)]
    return(constant_var_list)
}
output1 <- constant('ga_bil') #[1] "other_tx_2y"
output2 <- constant('ga_all')
output3 <- constant('mild_uni')
output4 <- constant('mild_bil')

var_miss <- var_list[apply(raw_data, 2, function(x) NA %in% x )]
length(raw_data$bl_va[raw_data$bl_va=='NA'])
var_miss_num <- sapply(raw_data[, var_miss], as.numeric(is.na))



#test for lapply and sapply
cohort <- 'mild_uni'
data_file<- paste(cohort, '_2y.csv', sep='')
raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)

lp <- lapply(raw_data, is.numeric)
lp.vct <- unlist(lp)
is.vector(lp.vct)

sp <- sapply(raw_data, is.numeric)
is.vector(sp)

#convert to '123' to 123 in data.frame
sp <- sapply(table_check_byVar_2, as.vector)
sp.numeric <- apply(sp, 2, as.numeric)         #sapply should not used here
sp.numeric.idx <- sapply(sp.numeric, is.numeric)


num_mtx <- matrix(seq(1, 12, 2), nrow=3) 
dim(num_mtx)  #[1] 3 2
sapply(num_mtx, mean)  #vector of lenght 6
sapply(data.frame(num_mtx), mean) #vector of length 2
#-----------------01. Descriptive Statistics------------------------#


#t <- 'mild'
#cohort <- 'mild_uni'
discriptive_statistics <- function(cohort, t){
    output <- numeric()
    
    if (t == 'ga'){
        data_file<- paste(cohort, '_2y.csv', sep='')
        raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
        raw_data_omitMiss <- na.omit(raw_data)
        miss_num <- dim(raw_data)[1] - dim(raw_data_omitMiss)[1]
        miss_rate <- round(miss_num/dim(raw_data)[1], 2)
        obs_num <- dim(raw_data_omitMiss)[1]
        attr_num <- dim(raw_data_omitMiss)[2]
        response_var <- response_list[1]
        response <- raw_data_omitMiss[, response_var]
        num_pos_resp <- sum(response)
        output <- rbind(output, c(cohort=cohort, response=response_var, obs_num=obs_num, missing_num=miss_num, missing_rate=miss_rate, attribute_num=attr_num, num_positive_response=num_pos_resp))
    }
    if (t == 'mild'){
        data_file<- paste(cohort, '_2y.csv', sep='')
        raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
        raw_data_omitMiss <- na.omit(raw_data)
        miss_num <- dim(raw_data)[1] - dim(raw_data_omitMiss)[1]
        miss_rate <- round(miss_num/dim(raw_data)[1], 2)
        obs_num <- dim(raw_data_omitMiss)[1]
        attr_num <- dim(raw_data_omitMiss)[2]
        
        for(response_var in response_list){
            response <- raw_data_omitMiss[, response_var]
            num_pos_resp <- sum(response)
            output <- rbind(output, c(cohort=cohort, response=response_var, obs_num=obs_num, missing_num=miss_num, missing_rate=miss_rate, attribute_num=attr_num, num_positive_response=num_pos_resp))
            
        }
        
    }
    output <- data.frame(output)
    return(output)
    
    
}

line_1 <- discriptive_statistics('mild_uni', 'mild')
line_2 <- discriptive_statistics('mild_bil', 'mild')
line_3 <- discriptive_statistics('ga_bil', 'ga')
line_4 <- discriptive_statistics('ga_all', 'ga')

#output_f <- as.data.frame(rbind(line_1, line_2, line_3, line_4))
output_f <- as.data.frame(rbind(line_1, line_2))
write.csv(output_f, 'DS_table1.csv')
write.table(output_f, 'DS_table1.txt', sep='\t\t', quote=F)

#-----------------02. stepwise regression Modeling for all the Outcome/Cohort type (mild_uni mild_bil outcome_wet outcome_ga)----------------------#
#test for histogram of log(age) and log(bl_va(nonzero))

table_check_byContiVar_hist_sp <- function(cohort, by_response){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data_omitMiss <- na.omit(raw_data)
    var_list <- names(raw_data_omitMiss)
    response_list <- c('wet_outcome', 'ga_outcome')
    covar_list <- var_list[(length(var_list)-12):length(var_list)]
    conti_flag <-  lapply(raw_data_omitMiss[, covar_list], function(x){length(levels(as.factor(x))) > 2})
    conti_var_list <- covar_list[unlist(conti_flag)]
    conti_var_list <- c('age', 'bl_va')
    if(by_response==FALSE){
        pdf(paste('histogram_', cohort, '_after_logtrans.pdf', sep=''))
        par(mfrow=c(2,1))
        par(pty='m')
        par(cex.lab=1.2, cex.axis=0.9)
        #i <- conti_var_list[1]
        for(i in conti_var_list){
            eval(parse(text=paste('var <- raw_data_omitMiss$', i, sep='')))
            var <- var[var != 0]
            var <- log(var)
            main.txt <- paste('Distribution of Covariate = log( ', i, ' )', sep='')
            xlab.txt <- paste('variable = log( ', i, ' )', sep='')
            bin.num <- 10
            breaks.txt <- seq(from=min(var), length.out=bin.num, to=max(var))
            breaks.label <- round(seq(from=min(var), length.out=bin.num, to=max(var)), 1)
            
            hist(var, breaks=breaks.txt,freq=T, main=main.txt, xlab=xlab.txt, xlim=range(breaks.txt), xaxt="n")
            #hist(var, main=main.txt, xlab=xlab.txt, xlim=range(breaks.txt))
            axis(side=1, at=breaks.label, labels=breaks.label)
            
            
        }
        dev.off()
        
    }else{
        #resp <- 'wet_outcome'
        for(resp in response_list){
            response <- raw_data_omitMiss[, resp]
            pdf(paste('histogram_', cohort, '_by_', resp, '.pdf', sep=''))
            par(mfrow=c(3,2))
            par(mfrow=c(3,2))
            par(pty='m')
            par(cex.lab=1.2, cex.axis=0.9)
            
            #i <- conti_var_list[1]
            for(i in conti_var_list){
                eval(parse(text=paste('var <- raw_data_omitMiss$', i, sep='')))
                var_response1 <- var[response==1]
                
                main.txt <- paste('Distribution by ', resp, ' = 1', sep='')
                xlab.txt <- paste('variable = ', i, sep='')
                bin.num <- 10
                breaks.txt <- seq(from=min(var_response1), length.out=bin.num, to=max(var_response1))
                breaks.label <- round(seq(from=min(var_response1), length.out=bin.num, to=max(var_response1)), 1)
                #breaks.txt <- seq(from=min(var_response1), by=round((max(var_response1)-min(var_response1))/bin.num), to=max(var_response1))
                
                hist(var_response1, breaks=breaks.txt, freq=T, main=main.txt, xlab=xlab.txt, xlim=range(breaks.txt), xaxt="n")
                #hist(var_response1, breaks=breaks.txt, main=main.txt, xlab=xlab.txt, xlim=range(breaks.txt))
                #axis(side=1, at=breaks.txt, labels=seq(from=min(var_response1), length.out=10, to=max(var_response1), 2))
                axis(side=1, at=breaks.label, labels=breaks.label)
                
            }
            dev.off()
        }
        
    }
    
}

table_check_byContiVar_hist_sp(cohort='mild_bil', by_response=F)
table_check_byContiVar_hist_sp(cohort='mild_uni', by_response=F)

#test for logistic regression
covar_definition <- read.table(paste(data_path, 'covar_definition.csv', sep='\\'), sep=',', head=T)
cohort <- 'mild_uni'
DistributionCheck <- function(cohort, resp){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data <- raw_data[, setdiff(names(raw_data), var_unused_list)]
    raw_data_omitMiss <- na.omit(raw_data)
    var_list <- names(raw_data_omitMiss)
    response_list <- c('ga_outcome', 'wet_outcome')
    covar_list <- var_list[(length(var_list)-12):length(var_list)]
    log_conti_list <- c('age', 'bl_va')
    dich_conti_list <- grep('^ct', covar_list, value=TRUE)
    binary_list <- setdiff(covar_list, c(log_conti_list, dich_conti_list))
    for(i in dich_conti_list){
        var <-  raw_data_omitMiss[, i]
        raw_data_omitMiss[, paste(i, '_dich', sep='')] <- as.numeric(as.vector(cut(var, breaks=c(0, 0.01, max(var)), right=TRUE, include.lowest=TRUE, labels=c('0', '1'))))
        raw_data_omitMiss[, i] <- NULL
    }
    var <- raw_data_omitMiss[ , 'ct_ivi_2y_dich']
    response <- raw_data_omitMiss[, resp]
    tb <- table(var, response)
    response_0 <- sum(tb[, 1])
    response_1 <- sum(tb[, 2])
    posVar_0 <- sum(tb[1, ])
    posVar_1 <- sum(tb[2, ])
    pos_pos <- tb[2, 2]
    outcome <- cbind(Cohort=cohort, Response=resp, Variable='ct_ivi_2y_dich', response_0, response_1, posVar_0, posVar_1, pos_pos)
    return(outcome)
    
}
output1 <- DistributionCheck('mild_bil', resp='wet_outcome')
output2 <- DistributionCheck('mild_uni', resp='ga_outcome')
output <- as.data.frame(rbind(output1, output2))
Definition <- covar_definition[match(output[, 3], covar_definition[, 1]), 'Definition']
output.df <- data.frame(output[, seq(1,3,1)], Definition=Definition, output[, -seq(1, 3, 1)])
write.table(output.df, 'Distribution_check_table.txt', row.names=F, quote=F)


#run stepwise after dropping the var_without_pos_pos variables
#mild_bill-ga_outcome: other_tx_2y
#mild_uni-ga_outcome: other_tx_2y & diab_bl
#define the unused variables, mild cohort should be used here to get the full unused variables
cohort <- 'mild_uni'
data_file<- paste(cohort, '_2y.csv', sep='')
raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
var_unused_list <- names(raw_data)[1:5]

stepwise_drop <- function(cohort, response_input, log){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data <- raw_data[, setdiff(names(raw_data), var_unused_list)]
    raw_data_omitMiss <- na.omit(raw_data)
    raw_data_omitMiss <- raw_data
    
    var_list <- names(raw_data_omitMiss)
    response_list <- c('ga_outcome', 'wet_outcome')
    covar_list <- var_list[(length(var_list)-12):length(var_list)]
    log_conti_list <- c('age', 'bl_va')
    dich_conti_list <- grep('^ct', covar_list, value=TRUE)
    binary_list <- setdiff(covar_list, c(log_conti_list, dich_conti_list))
    for(i in dich_conti_list){
        var <-  raw_data_omitMiss[, i]
        raw_data_omitMiss[, paste(i, '_dich', sep='')] <- as.numeric(as.vector(cut(var, breaks=c(0, 0.01, max(var)), right=TRUE, include.lowest=TRUE, labels=c('0', '1'))))
        raw_data_omitMiss[, i] <- NULL
    }
    covar_list <- names(raw_data_omitMiss)[(length(var_list)-12):length(var_list)]
    if(log==TRUE){
        for(i in log_conti_list){
            raw_data_omitMiss[, paste(i, '_log', sep='')] <- ifelse(raw_data_omitMiss[, i]==0, 0, log(raw_data_omitMiss[, i]))    
            raw_data_omitMiss[, i] <- NULL
        }
        covar_list <- names(raw_data_omitMiss)[(length(var_list)-12):length(var_list)]
    }
    
    resp <- response_input
    response <- raw_data_omitMiss[, resp]
    if(resp == 'wet_outcome'){
        covar_list_model <- setdiff(covar_list, c('other_tx_2y'))
    }else if(resp=='ga_outcome'){
        covar_list_model <- setdiff(covar_list,  binary_list)
    }
    model_data <- cbind(raw_data_omitMiss[, covar_list_model], response)
    training_data <- model_data
    fit<- glm(response~., data=training_data, family=binomial)
    step_wise<- step(fit , direction="both")
    
    coef<- data.frame(Coefficient=round(coef(step_wise), 7) , Odds_ratio=round(exp(coef(step_wise)), 7))
    #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
    
    p_value<- round(summary(step_wise)$coef[, "Pr(>|z|)"], 7)
    covar_definition_matched <- covar_definition[match(rownames(coef), covar_definition[, 1]), 'Definition'] 
    stepwise_output <- cbind(Definition=covar_definition_matched, coef, P_value=p_value)
    write.xlsx(stepwise_output, file='stepwise_output.xlsx', sheetName=paste(cohort, '_by_', resp, sep=''), row.names=T, append=T, showNA=T)
    #add flag for later rbind txt file
    covariate <- rownames(stepwise_output)
    stepwise_output_flag <- data.frame(cbind(Cohort=cohort, Response=resp, covariate, stepwise_output))
    return(stepwise_output_flag)
    #write.table(stepwise_output, paste(cohort, '_', resp, '_stepwise_output.txt', sep=''), quote=F, row.names=F, sep='\t\t')
    
}



output1 <- stepwise_drop('mild_uni', 'ga_outcome', F)
output2 <- stepwise_drop('mild_uni', 'wet_outcome', F)
output3 <- stepwise_drop('mild_bil', 'ga_outcome', F)
output4 <- stepwise_drop('mild_bil', 'wet_outcome', F)
#rbind all the 4 model results and save as txt file
stepwise_output_bind <- rbind(output1, output2, output3, output4)
write.table(stepwise_output_bind, 'stepwise_output.txt', quote=F, row.names=F, sep='\t\t')

#-----------------------------------------------single logistc regression test on several problem-----------------------------------------#
#-----------------------------------------------age bi_va ^ct..-------------------------------------------------#
target_var <- 'age'
cohort <- 'mild_uni'
test_sigLogistic <- function(target_var){
        sigLogistic_output_rbind <- numeric()
        breaks2_rbind <- numeric()
        breaks4_rbind <- numeric()
        for(cohort in c('mild_uni', 'mild_bil')){
        data_file<- paste(cohort, '_2y.csv', sep='')
        raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
        raw_data_omitMiss <- na.omit(raw_data)
        var_list <- names(raw_data_omitMiss)
        conti <- raw_data_omitMiss[, target_var]
        log <- ifelse(conti==0, 0, log(conti))
        for(resp in c('ga_outcome', 'wet_outcome')){
            bucket2 <- cut(conti, breaks=quantile(conti, c(0, 0.5, 1)), labels=c(0, 1), include.lowest=T)
            breaks2 <- c(cohort, resp, quantile(conti, c(0, 0.5, 1)))
            breaks2_rbind <- rbind(breaks2_rbind, breaks2)
            if(target_var == 'age' & cohort == 'mild_bil' & resp == 'ga_outcome'){
                bucket4 <- cut(conti, breaks=c(min(conti), 77, 82, 84, max(conti)), labels=c(0, 1, 2, 3), include.lowest=T)
                breaks4 <- c(cohort, resp, c(min(conti), 77, 82, 84, max(conti)))
            }else if(target_var == 'age' & cohort=='mild_uni' & resp=='ga_outcome'){
                bucket4 <- cut(conti, breaks=c(min(conti), 59, 83, 85, max(conti)), labels=c(0, 1, 2, 3), include.lowest=T)
                breaks4 <- c(cohort, resp, c(min(conti), 59, 83, 85, max(conti)))
                
            }else{
                bucket4 <- cut(conti, breaks=quantile(conti, c(0, 0.25, 0.5, 0.75, 1)), labels=c(0, 1, 2, 3), include.lowest=T)
                breaks4 <- c(cohort, resp, quantile(conti, c(0, 0.25, 0.5, 0.75, 1)))
                
            }
            breaks4_rbind <- rbind(breaks4_rbind, breaks4)
            model_data <- data.frame(conti, log, bucket2, bucket4)
            names(model_data) <- c(paste(target_var, '_conti', sep=''), paste(target_var, '_log', sep=''), paste(target_var, '_bucket2', sep=''), paste(target_var, '_bucket4', sep=''))
            form_list <- names(model_data)
            bucket_list <- grep('bucket', form_list, value=T)
            response <- raw_data_omitMiss[, resp]
            model_data$response <- response
            i <- names(model_data)[3]
            for(i in form_list){
                if(i %in% bucket_list){
                    for_modelMatrix <- data.frame(model_data[, i])
                    names(for_modelMatrix ) <- i
                    for_modelMatrix1 <- model.matrix(~., data=for_modelMatrix)
                    data <- data.frame(response=response, data.frame(for_modelMatrix1)[-1])
                    #fit.sig <- glm.fit(x=for_modelMatrix, y=as.factor(response), family=binomial)?????????????????
                    fit.sig <- glm(response~., data=data, family=binomial)
                }else{
                    data <- model_data
                    eval(parse(text=paste('fit.sig <- glm(response ~ ', i, ', data=data, family=binomial)', sep='')))
                }
                coef<- data.frame(Coefficient=coef(fit.sig) , Odds_ratio=exp(coef(fit.sig)))
                p_value<- summary(fit.sig)$coef[, "Pr(>|z|)"]
                covar_definition_matched <- covar_definition[match(rownames(coef), covar_definition[, 1]), 'Definition'] 
                covariate <- rownames(coef)
                pred_vct <- predict(fit.sig, data, type='response')
                auc <- auc(response, pred_vct)
                sigLogistic_output <- cbind(Cohort=cohort, Response=resp, Covariate=covariate[-1], Definition=covar_definition_matched[-1], coef[-1, ], P_value=p_value[-1], Auc=auc)
                sigLogistic_output_rbind <- rbind(sigLogistic_output_rbind, sigLogistic_output)
            }
        }
    }
    #write.xlsx(sigLogistic_output_rbind, 'test_sigLogistic_output_reference_recode.xlsx', sheetName=target_var, append=T, row.names=F, showNA=T)
    #return(sigLogistic_output_rbind)
    return(breaks2_rbind)
    
}
output1 <- test_sigLogistic('age')
output2 <- test_sigLogistic('bl_va')
output <- rbind(output1, output2)
#write.table(output, 'test_sigLogistic_output_reference_recode.txt', quote=F, row.names=F, sep='\t\t')


#drow the transformed cross table with response
cohort <- 'mild_uni'
distrTransformed <- function(target_var){
    for(cohort in c('mild_uni', 'mild_bil')){
        data_file<- paste(cohort, '_2y.csv', sep='')
        raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
        raw_data_omitMiss <- na.omit(raw_data)
        var_list <- names(raw_data_omitMiss)
        conti <- raw_data_omitMiss[, target_var]
        log <- ifelse(conti==0, 0, log(conti))
        bucket2 <- cut(conti, breaks=quantile(conti, c(0, 0.5, 1)), labels=c(0, 1), include.lowest=T)
        bucket4 <- cut(conti, breaks=quantile(conti, c(0, 0.25, 0.5, 0.75, 1)), labels=c(0, 1, 2, 3), include.lowest=T)
        model_data <- data.frame(conti, log, bucket2, bucket4)
        names(model_data) <- c(paste(target_var, '_conti', sep=''), paste(target_var, '_log', sep=''), paste(target_var, '_bucket2', sep=''), paste(target_var, '_bucket4', sep=''))
        form_list <- names(model_data)
        bucket_list <- grep('bucket', form_list, value=T)
        resp <- 'ga_outcome'
        tb_rbind <- numeric()
        for(resp in c('ga_outcome', 'wet_outcome')){
            response <- raw_data_omitMiss[, resp]
            response0 <- sum(1-response)
            response1 <- sum(response)
            model_data$response <- response
            for(i in bucket_list){
                    var <- model_data[, i]
                    lv <- length(levels(as.factor(var)))
                    tb <- table(var, response )
                    row_nm<- paste(i, 0:(lv-1), sep='')
                    #col.names(tb) <- c('Quantile_by_Response0', 'Quantile_by_Response1')
                    tb_cbind <- cbind(Cohort=cohort, Response=resp, Quantile=row_nm, Response0=response0, Response1=response1, tb)
                    tb_rbind <- rbind(tb_rbind, tb_cbind)
                }
            }
        }
        rownames(tb_rbind) <- NULL
        tb_rbind_df <- as.data.frame(tb_rbind)
        names(tb_rbind_df)[(dim(tb_rbind_df)[2]-1): dim(tb_rbind_df)[2]] <- c('Quantile_by_Response0', 'Quantile_by_Response1')
        return(tb_rbind_df)
    }
    output1 <- distrTransformed('age')
    output2 <- distrTransformed('bl_va')
    output <- data.frame(rbind(output1, output2))
write.xlsx(output, 'Distribute_BucketVar_byResponse.xlsx', row.names=F)


quantile_byResponse <- function(target_var){
    for(cohort in c('mild_uni', 'mild_bil')){
        data_file<- paste(cohort, '_2y.csv', sep='')
        raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
        raw_data_omitMiss <- na.omit(raw_data)
        var_list <- names(raw_data_omitMiss)
        conti <- raw_data_omitMiss[, target_var]
        
        resp <- 'ga_outcome'
        tb_rbind <- numeric()
        for(resp in c('ga_outcome', 'wet_outcome')){
            response <- raw_data_omitMiss[, resp]
            response0 <- sum(1-response)
            response1 <- sum(response)
            model_data$response <- response
            for(i in bucket_list){
                var <- model_data[, i]
                lv <- length(levels(as.factor(var)))
                tb <- table(var, response )
                row_nm<- paste(i, 0:(lv-1), sep='')
                #col.names(tb) <- c('Quantile_by_Response0', 'Quantile_by_Response1')
                tb_cbind <- cbind(Cohort=cohort, Response=resp, Quantile=row_nm, Response0=response0, Response1=response1, tb)
                tb_rbind <- rbind(tb_rbind, tb_cbind)
            }
        }
    }
    rownames(tb_rbind) <- NULL
    tb_rbind_df <- as.data.frame(tb_rbind)
    names(tb_rbind_df)[(dim(tb_rbind_df)[2]-1): dim(tb_rbind_df)[2]] <- c('Quantile_by_Response0', 'Quantile_by_Response1')
    return(tb_rbind_df)
}
output1 <- distrTransformed('age')
output2 <- distrTransformed('bl_va')
output <- data.frame(rbind(output1, output2))
write.xlsx(output, 'Distribute_BucketVar_byResponse.xlsx', row.names=F)



#-----------------03. elastic-net regression Modeling for all the Outcome/Cohort type----------------------#
cohort <- 'mild_uni'
elastic <- function(cohort){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data_omitMiss <- na.omit(raw_data)
    var_list <- names(raw_data_omitMiss)
    response_list <- c('wet_outcome', 'ga_outcome')
    covar_list <- var_list[(length(var_list)-12):length(var_list)]
    resp <- response_list[2]
    for(resp in response_list){
        response <- raw_data_omitMiss[, resp]
        model_data <- cbind(raw_data_omitMiss[, covar_list], response)
        #model_data <- cbind(raw_data_omitMiss[, covar_list], response)
        training_data <- model_data
        training_data_lasso<-training_data
        #test_data_lasso1<-datasets_lasso[[2]]
        #training_data_lasso<-training_data_lasso1[,variable_list_v2]
        
        k.folds<- 10                                                           # tenfold cross-validation
        foldid<- nrow(training_data_lasso)
        foldid[training_data_lasso$response==1]<-sample(rep(1:k.folds, length=length(which(training_data_lasso$response==1))))
        foldid[training_data_lasso$response==0]<- sample(rep(1:k.folds, length=length(which(training_data_lasso$response==0))))
        table(training_data_lasso$response , foldid) # QC
        #list_lasso_foldid<- rbind(list_lasso_foldid , foldid)
        
        # Calculating initial lambda and the lambda sequence
        training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
        initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=1, standardize=F)$lambda  # calculating the initial lambda
        lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500)) # get a length=500 descending sequence from initial lambda to 0
        cv_auc <- matrix(nr=k.folds , nc=length(lambda_seq))
        
        # Tenfold cross-validation
        cv_auc <- numeric()
        alpha_seq <- seq(0, 1, 0.1) #change by test result
        for(j in alpha_list){
            for(i in 1:k.folds){
                cv_training_data_lasso<- training_data_lasso[foldid!=i,]                                            # select 9 folds as training data
                cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
                cv_test_data_lasso<- training_data_lasso[foldid==i,]                                                # select 1 fold as test data
                cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1]
                
                fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                                   lambda=lambda_seq, family="binomial", alpha=j, standardize=F)
                test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
                test_pred_avg<- apply(test_pred, 2, function(x){auc(cv_test_data_lasso$response , x)})
                test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
                
                cv_auc<- rbind(cv_auc, c(j, i, test_pred_avg))                                                              # calculate the AUC based on left-out fold
            }#end for i
        }#end for j
        
        #cv_auc_group <- aggregate(cv_auc[,-c(1, 2)],by=list(alpha=cv_auc[, 1],fold=cv_auc[, 2]),mean)          #[1] 110 590
        cv_auc_byAlpha <- aggregate(cv_auc[,-c(1, 2)],by=list(alpha=cv_auc[, 1]),mean)[, -1]                            #[1]  11 587
        alphaIdx_forAuc<- apply(cv_auc_byAlpha, 1, which.max)                          # allocate the maximum lambda for each alpha 
        maxAuc_byAlpha <- apply(cv_auc_byAlpha, 1, function(x) x[which.max(x)])
        optimum_alpha <- which.max(maxAuc_byAlpha)                                          # allocate which alpha the final maximum lambda fall into
        optimum_lambdaIdx <-  alphaIdx_forAuc[optimum_alpha]
        auc_test_esti <- max_auc_byAlpha[optimum_alpha]
        # calculate coeff, odds ratio and p-value
        optim_model<- glmnet(x=training_matrix, y=training_data_lasso$response, lambda=lambda_seq[optimum_lambdaIdx], family="binomial", alpha=alpha_seq[optimum_alpha], standardize=F)
        model_coef<- optim_model$beta
        odds_ratio<- exp(model_coef)[model_coef != 0]
        non_zero_var<- rownames(model_coef)[as.vector(model_coef != 0)]
        re_model_var<- c('response', non_zero_var)
        re_fit<- glm(response ~., data=training_data_lasso[,match(re_model_var , names(training_data_lasso))], family=binomial)
        #p_value<- summary(re_fit)$coef[match(rownames(summary(re_fit)$coef), non_zero_var), "Pr(>|z|)"]
        p_value<- summary(re_fit)$coef[rownames(summary(re_fit)$coef) %in% non_zero_var, "Pr(>|z|)"]
        #model<- data.frame(coefficient=model_coef[model_coef!=0], odds_ratio, p_value=p_value[match(non_zero_var, names(p_value))])
        model_output <- data.frame(variable=non_zero_var, coefficient=as.vector(model_coef)[as.vector(model_coef) != 0], odds_ratio=odds_ratio, p_value=p_value)
        
    }#end for resp
    
    
}#end for function
#-----------------04. Model evaluation for all the 14 models using overfitting------------#

#detect the distinguish of variables distribution used modeling when different seeds are used on training data sampling

#1. set up working folder and read in model data


res_var <- 'post_relapse_persist'

lrcv<-function(res_var){
    
    training_auc_rep_list <- numeric()
    cv_auc_training_list <- numeric()
    cv_auc_mean1_list <- numeric()
    cv_auc_sd_list <- numeric()
    
    cv_auc_rep_list <- numeric()
    list_model_lasso<- list()
    list_predict_prob_training_lasso<- numeric()                                       
    list_lasso_lambda<- numeric()
    list_lasso_foldid<- list()
    
    raw_data$response<- raw_data[,names(raw_data)==res_var]                      # define response: persistent or relapse
    
    res_var1<-ifelse(res_var=='persistent','persistent','relapse')                # just for naming the output file
    model_variable<- model_variable_run1_3                                                    # select different variable list for different runs
    
    training_data<- raw_data[ , match(model_variable, names(raw_data))]
    
    
    # store results for lasso
    for(k in 1:100){                                                              # sample training and test data set, proportionally split response outcome into training and test
        
        # the lasso is based on a different 75/25 training/test random split of the data
        #datasets_lasso<-sampling_ims(model_data,0.75,'response',setseed=TRUE)
        
        training_data_lasso<-training_data
        #test_data_lasso1<-datasets_lasso[[2]]
        #training_data_lasso<-training_data_lasso1[,variable_list_v2]
        
        k.folds<- 10                                                           # tenfold cross-validation
        foldid<- nrow(training_data_lasso)
        foldid[training_data_lasso$response==1]<-sample(rep(1:k.folds, length=length(which(training_data_lasso$response==1))))
        foldid[training_data_lasso$response==0]<- sample(rep(1:k.folds, length=length(which(training_data_lasso$response==0))))
        table(training_data_lasso$response , foldid) # QC
        list_lasso_foldid<- rbind(list_lasso_foldid , foldid)
        
        # Calculating initial lambda and the lambda sequence
        training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
        initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=1, standardize=F)$lambda  # calculating the initial lambda
        lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500)) # get a length=500 descending sequence from initial lambda to 0
        cv_auc <- matrix(nr=k.folds , nc=length(lambda_seq))
        
        # Tenfold cross-validation
        for(i in 1:k.folds){
            cv_training_data_lasso<- training_data_lasso[foldid!=i,]                                            # select 9 folds as training data
            cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
            cv_test_data_lasso<- training_data_lasso[foldid==i,]                                                # select 1 fold as test data
            cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1]
            
            fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                               lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
            test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
            test_pred_avg<- apply(test_pred, 2, function(x){auc(cv_test_data_lasso$response , x)})
            test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
            
            cv_auc[i,]<- test_pred_avg                                                              # calculate the AUC based on left-out fold
        }
        
        total_model<- glmnet(x=training_matrix, y=training_data_lasso$response, lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
        cv_auc_mean<- apply(cv_auc , 2 , function(x) mean(x[!is.na(x)]))                          # mean of the AUC by each lambda 
        optimum_model<- which.max(cv_auc_mean)  # select the lambda with largest AUC as the optimum one
        cv_auc_mean_optimum <- mean(cv_auc_mean[optimum_model]) #please be carefull here 
        list_lasso_lambda<- rbind(list_lasso_lambda , c(i , lambda_seq[optimum_model]))# store optimum lambda sequence 
        cv_auc_rep_list <- rbind(cv_auc_rep_list, cv_auc_mean_optimum)
        training_obs<- predict(total_model, training_matrix, type="response")[,optimum_model]
        training_auc<- auc(training_data$response , training_obs)
        training_auc_rep_list <- rbind(training_auc_rep_list, training_auc)
        
    }
    
    cv_auc_rep_vct <- as.vector(cv_auc_rep_list)
    cv_auc_sd <- sd(cv_auc_rep_vct)
    cv_auc_mean1 <- mean(cv_auc_rep_vct)
    cv_auc_sd_list <- rbind(cv_auc_sd_list, cv_auc_sd)
    cv_auc_mean1_list <- rbind(cv_auc_mean1_list, cv_auc_mean1)
    cv_auc_training <- mean(training_auc_rep_list)
    cv_auc_training_list <- rbind(cv_auc_training_list, cv_auc_training)
    
}

#========================test for test auc estimate for lasso=========================
#test for 100 cv lasso
training_data_lasso <- raw_data
rep.num <- 100
test_auc_est_lst <- numeric()
test_auc_est_lst2 <- numeric()
for(r in 1:rep.num){
    k.folds<- 10                                                           # tenfold cross-validation
    foldid<- nrow(training_data_lasso)
    foldid[training_data_lasso$response==1]<-sample(rep(1:k.folds, length=length(which(training_data_lasso$response==1))))
    foldid[training_data_lasso$response==0]<- sample(rep(1:k.folds, length=length(which(training_data_lasso$response==0))))
    table(training_data_lasso$response , foldid) # QC
    
    # Calculating initial lambda and the lambda sequence
    training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term [5483, 100]
    initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=1, standardize=F)$lambda  # calculating the initial lambda [63]
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500)) # get a length=500 descending sequence from initial lambda to 0
    cv_auc <- matrix(nr=k.folds , nc=length(lambda_seq)) #[1]  10 562
    
    
    # Tenfold cross-validation
    for(i in 1:k.folds){
        cv_training_data_lasso<- training_data_lasso[foldid!=i,]                                            # select 9 folds as training data
        cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
        cv_test_data_lasso<- training_data_lasso[foldid==i,]                                                # select 1 fold as test data
        cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1] #[1] 547 100
        
        fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                           lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
        test_pred<- predict(fit_lasso, cv_test_matrix, type="response")                          #[1] 547 562
        test_pred_avg<- apply(test_pred, 2, function(x){auc(cv_test_data_lasso$response , x)})       #[1] 562
        test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
        
        cv_auc[i,]<- test_pred_avg                                                              ##[1]  10 562  calculate the AUC based on left-out fold
    }
    
    total_model<- glmnet(x=training_matrix, y=training_data_lasso$response, lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
    cv_auc_mean<- apply(cv_auc , 2 , function(x) mean(x[!is.na(x)]))                          # mean of the AUC by each lambda 
    optimum_model<- which.max(cv_auc_mean)  # select the lambda with largest AUC as the optimum one
    cv_auc_mean_optimum <- mean(cv_auc_mean[optimum_model]) #please be carefull here 
    test_auc_est_lst <- rbind(test_auc_est_lst, cv_auc_mean_optimum)
    cv_auc_lst <- numeric()
    for(i in 1:k.folds){
        cv_training_data_lasso<- training_data_lasso[foldid!=i,]                                            # select 9 folds as training data
        cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
        cv_test_data_lasso<- training_data_lasso[foldid==i,]                                                # select 1 fold as test data
        cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1] #[1] 547 100
        
        fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                           lambda=lambda_seq[optimum_model], family="binomial", alpha=1, standardize=F)
        test_pred<- predict(fit_lasso, cv_test_matrix, type="response")                          #[1] 547 
        test_pred_avg<- apply(test_pred, 2, function(x){auc(cv_test_data_lasso$response , x)})       #[1] 1
        cv_auc_lst <- rbind(cv_auc_lst, test_pred_avg)
    }
    test_auc_est <- mean(cv_auc_lst)
    test_auc_est_lst2 <- rbind(test_auc_est_lst2, test_auc_est)
    
}
test_auc_est_lst1_avg <- mean(test_auc_est_lst)
test_auc_est_lst2_avg <- mean(test_auc_est_lst2)
test_auc_est_compare <- data.frame(test_auc_1cv=test_auc_est_lst1_avg, test_auc_10cv=test_auc_est_lst2_avg, diff=test_auc_est_lst1_avg-test_auc_est_lst2_avg, diff_ratio=(test_auc_est_lst1_avg-test_auc_est_lst2_avg)/test_auc_est_lst1_avg) #do not forget the bracket!!!






# test for only one time cv lasso
k.folds<- 10                                                           # tenfold cross-validation
foldid<- nrow(training_data_lasso)
foldid[training_data_lasso$response==1]<-sample(rep(1:k.folds, length=length(which(training_data_lasso$response==1))))
foldid[training_data_lasso$response==0]<- sample(rep(1:k.folds, length=length(which(training_data_lasso$response==0))))
table(training_data_lasso$response , foldid) # QC

# Calculating initial lambda and the lambda sequence
training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term [5483, 100]
initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=1, standardize=F)$lambda  # calculating the initial lambda [63]
lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500)) # get a length=500 descending sequence from initial lambda to 0
cv_auc <- matrix(nr=k.folds , nc=length(lambda_seq)) #[1]  10 562


# Tenfold cross-validation
for(i in 1:k.folds){
    cv_training_data_lasso<- training_data_lasso[foldid!=i,]                                            # select 9 folds as training data
    cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
    cv_test_data_lasso<- training_data_lasso[foldid==i,]                                                # select 1 fold as test data
    cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1] #[1] 547 100
    
    fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                       lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
    test_pred<- predict(fit_lasso, cv_test_matrix, type="response")                          #[1] 547 562
    test_pred_avg<- apply(test_pred, 2, function(x){auc(cv_test_data_lasso$response , x)})       #[1] 562
    test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
    
    cv_auc[i,]<- test_pred_avg                                                              ##[1]  10 562  calculate the AUC based on left-out fold
}

total_model<- glmnet(x=training_matrix, y=training_data_lasso$response, lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
cv_auc_mean<- apply(cv_auc , 2 , function(x) mean(x[!is.na(x)]))                          # mean of the AUC by each lambda 
optimum_model<- which.max(cv_auc_mean)  # select the lambda with largest AUC as the optimum one
cv_auc_mean_optimum <- mean(cv_auc_mean[optimum_model]) #please be carefull here 
cv_auc_lst <- numeric()
for(i in 1:k.folds){
    cv_training_data_lasso<- training_data_lasso[foldid!=i,]                                            # select 9 folds as training data
    cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
    cv_test_data_lasso<- training_data_lasso[foldid==i,]                                                # select 1 fold as test data
    cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1] #[1] 547 100
    
    fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                       lambda=lambda_seq[optimum_model], family="binomial", alpha=1, standardize=F)
    test_pred<- predict(fit_lasso, cv_test_matrix, type="response")                          #[1] 547 
    test_pred_avg<- apply(test_pred, 2, function(x){auc(cv_test_data_lasso$response , x)})       #[1] 1
    cv_auc_lst <- rbind(cv_auc_lst, test_pred_avg)
}
test_auc_estimate <- mean(cv_auc_lst)
