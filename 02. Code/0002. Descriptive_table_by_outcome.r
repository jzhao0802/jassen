

#==========================================================================================
#  Project:  Jassen project

#    Part II: frequency table by outcome

#    Develop time: 9/3/2015 - .

#    Developer: Jie Zhao
#==========================================================================================
rm(list=ls())

library(glmnet)
library(xlsx)

options(digits=7)

data_path<- 'D:\\working materials\\Jassen Project\\01. Raw_data\\ForModel'
out_path<-'D:\\working materials\\Jassen Project\\03.Output\\Feb27\\sendout_Mar19'
setwd(out_path)
#names(raw_data)<- tolower(names(raw_data))
covar_definition <- read.table(paste(data_path, 'covar_definition.csv', sep='\\'), sep=',', head=T)
covar_definition[, 1]<- gsub(' +$', '', covar_definition[, 1])


#var_list_df <- read.table(paste(data_path, 'old_var_list.csv', sep='\\'), sep=',', header=F)
#var_list_old <- var_list_df[,1]
var_list_old <- covar_definition[covar_definition[, 3]==0, 1]
length(var_list_old) # 22
#var_list_old <- gsub(' +$', '', var_list_old, perl=T)

cohort <- 'mild_bil'
binary_list <- c('mi', 'cva', 'tia', 'othercvs', 'hypertension', 'cataract', 'glaucoma', 'diabetes')
conti_list <- c('central1mmretinalthickness', 'macularvolume', 'fovealthickness')
var_list <- c(binary_list, conti_list)
var_list_all <- tolower(c(var_list_old, var_list)) #33=22(old)+11(new)
conti_list_all <- c(conti_list, 'age', 'bl_va', grep('^ct', var_list_all, value=T)) # 13
binary_list_all <- setdiff(var_list_all, conti_list_all) #20
#write.csv(var_list_all, paste(data_path, 'covar_list_all.csv', sep='\\'))



cohort <- 'ga_all'
outcome<- 'wet_outcome'
table_check_byAllBinaryCovar <- function(cohort, outcome){
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    raw_data <- raw_data[, c(var_list_all, outcome)]
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    
    #raw_data_omitMiss <- na.omit(raw_data)
    #var_list <- names(raw_data_omitMiss)
    
    key <- numeric()
    pos_pos_lst <- numeric()
    pos_pos_numeric_lst <- numeric()
    resp <- outcome
    response <- raw_data[, resp]
    num_response_1 <- sum(response)
    num_response_0 <- sum(1-response)
    i <- 'tx_any'
    for(i in binary_list_all){
        eval(parse(text=paste('var <- raw_data$', i, sep='')))
        tb <- table(var, response)
        if(dim(tb)[1] ==1 & '0' %in% rownames(tb)){
            pos <- 0
            neg <- sum(tb)
            pos_pos <- 0
            covar1_response1 <- 0
            covar0_response1 <- tb[1,2]
            covar1_response0 <- 0
            covar0_response0 <- tb[1,1]
            odds_ratio_my <- ((covar1_response1/sum(tb))/(covar1_response0/sum(tb)))/((covar0_response1/pos)/(covar0_response0/pos))    #odds_ratio=odds(x==1)/odds(x==0) = ((p(Y=1|x=1)/p(Y=0|x=1))/(p(Y=1|x=0)/p(Y=0|x=0))
            
        }else{
            pos <- sum(tb[2,])
            neg <- sum(tb[1,])
            pos_pos <- tb[2,2]
            covar1_response1 <- tb[2,2]
            covar0_response1 <- tb[1,2]
            covar1_response0 <- tb[2,1]
            covar0_response0 <- tb[1,1]
            odds_ratio_my <- ((covar1_response1/sum(tb[1, ]))/(covar1_response0/sum(tb[1, ])))/((covar0_response1/sum(tb[2, ]))/(covar0_response0/sum(tb[2, ])))    #odds_ratio=odds(x==1)/odds(x==0) = ((p(Y=1|x=1)/p(Y=0|x=1))/(p(Y=1|x=0)/p(Y=0|x=0))
        }
        #odds_ratio_my <- ((covar1_response1/sum(tb[1, ]))/(covar1_response0/sum(tb[1, ])))/((covar0_response1/sum(tb[2, ]))/(covar0_response0/sum(tb[2, ])))    #odds_ratio=odds(x==1)/odds(x==0) = ((p(Y=1|x=1)/p(Y=0|x=1))/(p(Y=1|x=0)/p(Y=0|x=0))
        contigency_table<- matrix(c(covar1_response1, covar0_response1, covar1_response0, covar0_response0), nc=2, byrow=T)
        association_test<- fisher.test(contigency_table , alternative = "two.sided")
        #association_test <- fisher.test(x=as.factor(var), y=as.factor(response), alternative='two.sided')
        #association_test <- fisher.test(x=as.factor(response), y=as.factor(var), alternative='two.sided')
        
        p_value<- association_test$p.value
        odds_ratio<- as.vector(association_test$estimate)
        
        
        key <- rbind(key, c(cohort=cohort, response_variable=resp, binary_variable=i))
        pos_pos_numeric_lst <- rbind(pos_pos_numeric_lst, c(num_response_0=num_response_0, num_response_1=num_response_1, 
                                                            num_of_BinaryCovar_0=neg, num_of_BinaryCovar_1=pos, num_of_BinaryCovar_1_response_1=pos_pos, Odds_ratio=odds_ratio, P_value=p_value))
        #pos_pos_lst <- rbind(pos_pos_lst, c(cohort=cohort, response_variable=resp, binary_variable=i, num_response_0=num_response_0, num_response_1=num_response_1, 
        #                                   num_of_BinaryCovar_0=neg, num_of_BinaryCovar_1=pos, num_of_BinaryCovar_1_response_1=pos_pos))
        
        
    }
    
    #tb_summary <- as.data.frame(pos_pos_lst)
    tb_summary <- as.data.frame(cbind(data.frame(key), data.frame(pos_pos_numeric_lst)))
    tb_summary$Definition <- covar_definition[match(tb_summary$binary_variable, covar_definition[, 1]), 2]
    write.xlsx(tb_summary, file=paste(out_path, paste('Descriptive_table_allBinary_', cohort, '.xlsx', sep=''), sep='\\'), sheetName=outcome, row.names=F, append=T, showNA=T)
    
    return(tb_summary)
}

table_check_byBinaryCovar_1 <- table_check_byAllBinaryCovar('mild_bil', 'ga_outcome')
table_check_byBinaryCovar_2 <- table_check_byAllBinaryCovar('mild_bil', 'adv_outcome')
table_check_byBinaryCovar_3 <- table_check_byAllBinaryCovar('mild_uni', 'ga_outcome')
table_check_byBinaryCovar_4 <- table_check_byAllBinaryCovar('mild_uni', 'adv_outcome')
table_check_byBinaryCovar_5 <- table_check_byAllBinaryCovar('ga_all', 'wet_outcome')
table_check_byBinaryCovar <- rbind(table_check_byBinaryCovar_1, table_check_byBinaryCovar_2)

cohort <- 'mild_bil'
outcome <- 'ga_outcome'
Des_stat<-function(cohort, outcome){
    
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    response <- raw_data[, outcome]
    for(i in conti_list){
        raw_data[, i] <- ifelse(raw_data[, i]==0, NA, raw_data[, i])
    }
    
    OUT<- file(paste('Descriptive_table_',cohort, '_by_', outcome,'.txt',sep='') , 'w')
    i <- binary_list[1]
    for(i in binary_list_all){
        eval(parse(text = paste('x<- raw_data$' , i , sep='')))
        re<- table(x,response)
        nm<- rownames(re)
        mean0<-tapply(response,x,mean)
        writeLines(paste(i, 'response=0', 'response=1' ,'rate', sep='\t\t') , OUT)
        writeLines(paste(rep('-' , 150) , collapse='') , OUT)
        
        for(j in 1:nrow(re)){
            writeLines(paste(nm[j] , re[j,1]  ,re[j,2], round(mean0[j],4),sep='\t\t') , OUT)
        }
        writeLines('' , OUT)
    }
    i<- conti_list[1]
    for(i in conti_list_all){
        eval(parse(text = paste('x<- raw_data$' , i , sep='')))
        mean <- mean(x, na.rm=T)
        sd <- sd(x, na.rm=T)
        mean_by <- tapply(x, response, function(i)mean(i, na.rm=T))
        sd_by <- tapply(x, response, function(i)sd(i, na.rm=T))
        tb <- round(rbind(mean_by, sd_by), 2)
        nm<- names(mean_by)
        writeLines(paste(i, 'mean_by_response', 'sd_by_response' , sep='\t\t') , OUT)
        writeLines(paste(rep('-' , 150) , collapse='') , OUT)
        
        for(j in 1:length(sd_by)){
            writeLines(paste(nm[j] , tb[1, j], tb[2, j], sep='\t\t') , OUT)
        }
        writeLines('' , OUT)
    }
    
    
    close(OUT) 
}
Des_stat('mild_bil', 'ga_outcome')
Des_stat('mild_bil', 'adv_outcome')
Des_stat('mild_uni', 'ga_outcome')
Des_stat('mild_uni', 'adv_outcome')
Des_stat('ga_all', 'wet_outcome')

Des_stat_v2<-function(cohort, outcome){
    
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    response <- raw_data[, outcome]
    for(i in conti_list){
        raw_data[, i] <- ifelse(raw_data[, i]==0, NA, raw_data[, i])
    }
    OUT<- file(paste('Descriptive_table_',cohort, '_by_', outcome,'_v2.txt',sep='') , 'w')
    i <- binary_list[1]
    for(i in binary_list_all){
        eval(parse(text = paste('x<- raw_data$' , i , sep='')))
        re<- table(x,response)
        nm<- rownames(re)
        mean0<-tapply(response,x,mean)
        writeLines(paste(i, 'response=0', 'response=1' ,'rate', sep='\t\t') , OUT)
        writeLines(paste(rep('-' , 150) , collapse='') , OUT)
        
        for(j in 1:nrow(re)){
            writeLines(paste(nm[j] , re[j,1]  ,re[j,2], round(mean0[j],2),sep='\t\t') , OUT)
        }
        writeLines('' , OUT)
    }
    i<- conti_list[1]
    for(i in conti_list_all){
        eval(parse(text = paste('x<- raw_data$' , i , sep='')))
        N <- length(x)
        N_miss <- length(x[is.na(x)])
        mean <- round(mean(x, na.rm=T), 2)
        sd <- round(sd(x, na.rm=T), 2)
        writeLines(paste(i, 'N', 'N_Missing', 'Mean_nonMissing', 'SD_nonMissing', sep='\t\t') , OUT)
        writeLines(paste(rep('-' , 150) , collapse='') , OUT)
        writeLines(paste(N , N_miss, mean, sd, sep='\t\t') , OUT)
        writeLines('' , OUT)
    }
    
    
    close(OUT) 
}
Des_stat_v2('mild_bil', 'ga_outcome')
Des_stat_v2('mild_bil', 'adv_outcome')
Des_stat_v2('mild_uni', 'ga_outcome')
Des_stat_v2('mild_uni', 'adv_outcome')
Des_stat_v2('ga_all', 'wet_outcome')


cohort <- 'mild_uni'
outcome <- 'ga_outcome'

table_check_byContiVar2 <- function(cohort, outcome){
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    for(i in conti_list){
        raw_data[, i] <- ifelse(raw_data[, i]==0, NA, raw_data[, i])
    }
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    
    key <- numeric()
    tb_numeric <- numeric()
    resp <- outcome
    #resp <- 'wet_outcome'
    response <- raw_data[, resp]
    #i <- conti_var_list[5]
    for(i in conti_list_all){
        eval(parse(text=paste('var <- raw_data$', i, sep='')))
        mean <- round(mean(var, na.rm=T), 2)
        sd <- round(sd(var, na.rm=T), 2)
        tb <- table(var, response)
        mean_onResponse <- round(as.vector(tapply(var, response, function(x)mean(x, na.rm=T)))[2], 2)
        sd_onResponse <- round(as.vector(tapply(var, response, function(x)sd(x, na.rm=T)))[2], 2)
        mean_onResponse0 <- round(as.vector(tapply(var, response, function(x)mean(x, na.rm=T)))[1], 2)
        sd_onResponse0 <- round(as.vector(tapply(var, response, function(x)sd(x, na.rm=T)))[1], 2)
        
        key <- rbind(key, c(cohort=cohort, response_variable=resp, conti_variable=i))
        tb_numeric <- rbind(tb_numeric, c(mean_ContiCovar_1=mean, sd_ContiCovar_1=sd, mean_ContiCovar_1_response_1=mean_onResponse, sd_ContiCovar_1_response_1=sd_onResponse,
                                          mean_ContiCovar_1_response_0=mean_onResponse0, sd_ContiCovar_1_response_0=sd_onResponse0  #add for version2(v2)
        ))
    }
    
    tb_summary <- data.frame(data.frame(key), data.frame(tb_numeric))
    tb_summary$Definition <- covar_definition[match(tb_summary$conti_variable, covar_definition[, 1]), 2]
    write.xlsx(tb_summary, file=paste(out_path, paste('Descriptive_table_conti_', cohort, '.xlsx', sep=''), sep='\\'), sheetName=resp, row.names=F, append=T, showNA=T)
    
    return(tb_summary)
}
table_check_byContiVar_1 <- table_check_byContiVar2('mild_bil', 'ga_outcome')
table_check_byContiVar_2 <- table_check_byContiVar2('mild_bil', 'adv_outcome')
table_check_byContiVar_3 <- table_check_byContiVar2('mild_uni', 'ga_outcome')
table_check_byContiVar_4 <- table_check_byContiVar2('mild_uni', 'adv_outcome')
table_check_byContiVar_5 <- table_check_byContiVar2('ga_all', 'wet_outcome')

table_check_byContiVar <- rbind(table_check_byContiVar_1, table_check_byContiVar_2)


table_check_byContiVar3 <- function(cohort, outcome){
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    for(i in conti_list){
        raw_data[, i] <- ifelse(raw_data[, i]==0, NA, raw_data[, i])
    }
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    
    key <- numeric()
    tb_numeric <- numeric()
    resp <- outcome
    #resp <- 'wet_outcome'
    response <- raw_data[, resp]
    i <- conti_list[5]
    for(i in conti_list_all){
        eval(parse(text=paste('var <- raw_data$', i, sep='')))
        N <- dim(raw_data)[1]
        N_miss <- length(var[is.na(var)])
        mean <- round(mean(var, na.rm=T), 2)
        sd <- round(sd(var, na.rm=T), 2)
        #mean_onResponse <- round(as.vector(tapply(var, response, mean))[2], 2)
        #sd_onResponse <- round(as.vector(tapply(var, response, sd))[2], 2)
        ##mean_onResponse0 <- round(as.vector(tapply(var, response, mean))[1], 2)
        #sd_onResponse0 <- round(as.vector(tapply(var, response, sd))[1], 2)
        
        key <- rbind(key, c(cohort=cohort, response_variable=resp, conti_variable=i))
        tb_numeric <- rbind(tb_numeric, c(N=N, N_missing=N_miss, mean_of_nonMissing=mean, sd_of_nonMissing=sd))  #add for version2(v2)
    }
    
    tb_summary <- data.frame(data.frame(key), data.frame(tb_numeric))
    tb_summary$Definition <- covar_definition[match(tb_summary$conti_variable, covar_definition[, 1]), 2]
    write.xlsx(tb_summary, file=paste(out_path, paste('Descriptive_table_conti_', cohort, '_v2.xlsx', sep=''), sep='\\'), sheetName=resp, row.names=F, append=T, showNA=T)
    
    return(tb_summary)
}
table_check_byContiVar3('mild_bil', 'ga_outcome')
table_check_byContiVar3('mild_bil', 'adv_outcome')
table_check_byContiVar3('mild_uni', 'ga_outcome')
table_check_byContiVar3('mild_uni', 'adv_outcome')
table_check_byContiVar3('ga_all', 'wet_outcome')


cohort <- 'mild_bil'
outcome <- 'ga_outcome'
completeSeparation_check_byContiVar <- function(cohort, outcome){
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    
    resp <- outcome
    table_numeric <- numeric()
    table_key <- numeric()
    
    response <- raw_data[, resp]
    #        i <- conti_var_list[5]
    for(i in conti_list){
        complete_separate <- 0
        eval(parse(text=paste('var <- raw_data$', i, sep='')))
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
    
    table_f <- cbind(data.frame(table_key), data.frame(table_numeric))
    return(table_f)
}
output1<- completeSeparation_check_byContiVar('mild_bil', 'ga_outcome')
output2<- completeSeparation_check_byContiVar('mild_bil', 'adv_outcome')
output3<- completeSeparation_check_byContiVar('mild_uni', 'ga_outcome')
output4<- completeSeparation_check_byContiVar('mild_uni', 'adv_outcome')
output5<- completeSeparation_check_byContiVar('ga_all', 'wet_outcome')
output <- rbind(output1, output2, output3, output4, output5)
completeSeparation_check_byContiVar_2 <- completeSeparation_check_byContiVar('mild_uni')



#step-wise logistic regression
cohort <- 'mild_uni'
outcome <- 'ga_outcome'
covar_model_df <- read.table(paste(data_path, 'covar_list_all.csv', sep='\\'), sep=',', head=T)
stepwise_v1 <- function(cohort, outcome){
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    resp <- outcome
    response <- raw_data[, resp]
    if(cohort=='mild_uni' & outcome=='ga_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 2])]
    }else if(cohort=='mild_uni' & outcome=='adv_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 3])]
    }else if(cohort=='mild_bil' & outcome=='ga_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 4])]
    }else if(cohort=='mild_bil' & outcome=='adv_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 5])]
    }else{
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 6])]
    }
    covar_list_model <- as.vector(covar_list_model)
    model_data <- cbind(raw_data[, covar_list_model], response)
    if(cohort=='ga_all'){
        model_data <- na.omit(model_data)
    }
    na_check <- apply(apply(model_data, 2, is.na), 2, sum)
    
    training_data <- model_data
    fit<- glm(response~., data=training_data, family=binomial)
    step_wise<- step(fit , direction="both")
    
    coef<- data.frame(Coefficient=round(coef(step_wise), 2) , Odds_ratio=round(exp(coef(step_wise)), 2))
    #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
    
    p_value<- round(summary(step_wise)$coef[, "Pr(>|z|)"], 2)
    covariate <- rownames(coef)
    covar_definition_matched <- covar_definition[match(rownames(coef), covar_definition[, 1]), 'Definition'] 
    stepwise_output <- data.frame(Covariable=covariate, Definition=covar_definition_matched, coef, P_value=p_value, AIC=round(step_wise$aic, 2))
    write.xlsx(stepwise_output, file=paste('stepwise_output_', cohort, '_v1.xlsx', sep=''), sheetName=paste(cohort, '_by_', resp, sep=''), row.names=F, append=T, showNA=T)
    #add flag for later rbind txt file
    stepwise_output_flag <- data.frame(cbind(Cohort=cohort, Response=resp, covariate, stepwise_output))
    return(stepwise_output_flag)
    #write.table(stepwise_output, paste(cohort, '_', resp, '_stepwise_output.txt', sep=''), quote=F, row.names=F, sep='\t\t')
    
}
output1 <- stepwise_v1('mild_uni', 'ga_outcome')
output2 <- stepwise_v1('mild_uni', 'adv_outcome')
output3 <- stepwise_v1('mild_bil', 'ga_outcome')
output4 <- stepwise_v1('mild_bil', 'adv_outcome')
output5 <- stepwise_v1('ga_all', 'wet_outcome')

cohort <- 'mild_uni'
outcome <- 'ga_outcome'
covar_model_df <- read.table(paste(data_path, 'covar_list_all.csv', sep='\\'), sep=',', head=T)
stepwise_v2 <- function(cohort, outcome){
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    resp <- outcome
    response <- raw_data[, resp]
    if(cohort=='mild_uni' & outcome=='ga_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 2])]
        covar_list_model <- setdiff(covar_list_model, c('ivi_post', 'oct_bl', 'tx_any'))
    }else if(cohort=='mild_uni' & outcome=='adv_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 3])]
    }else if(cohort=='mild_bil' & outcome=='ga_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 4])]
    }else if(cohort=='mild_bil' & outcome=='adv_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 5])]
    }else{
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 6])]
    }
    covar_list_model <- as.vector(covar_list_model)
    model_data <- cbind(raw_data[, covar_list_model], response)
    if(cohort=='ga_all'){
        model_data <- na.omit(model_data)
    }
    na_check <- apply(apply(model_data, 2, is.na), 2, sum)
    
    training_data <- model_data
    fit<- glm(response~., data=training_data, family=binomial)
    step_wise<- step(fit , direction="both")
    
    coef<- data.frame(Coefficient=round(coef(step_wise), 2) , Odds_ratio=round(exp(coef(step_wise)), 2))
    #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
    
    p_value<- round(summary(step_wise)$coef[, "Pr(>|z|)"], 2)
    covariate <- rownames(coef)
    covar_definition_matched <- covar_definition[match(rownames(coef), covar_definition[, 1]), 'Definition'] 
    stepwise_output <- data.frame(Covariable=covariate, Definition=covar_definition_matched, coef, P_value=p_value, AIC=round(step_wise$aic, 2))
    write.xlsx(stepwise_output, file=paste('stepwise_output_', cohort, '_v2.xlsx', sep=''), sheetName=paste(cohort, '_by_', resp, sep=''), row.names=F, append=T, showNA=T)
    #add flag for later rbind txt file
    stepwise_output_flag <- data.frame(cbind(Cohort=cohort, Response=resp, covariate, stepwise_output))
    return(stepwise_output_flag)
    #write.table(stepwise_output, paste(cohort, '_', resp, '_stepwise_output.txt', sep=''), quote=F, row.names=F, sep='\t\t')
    
}
output1 <- stepwise_v2('mild_uni', 'ga_outcome')
output2 <- stepwise_v2('mild_uni', 'adv_outcome')
output3 <- stepwise_v2('mild_bil', 'ga_outcome')
output4 <- stepwise_v2('mild_bil', 'adv_outcome')
output5 <- stepwise_v2('ga_all', 'wet_outcome')




