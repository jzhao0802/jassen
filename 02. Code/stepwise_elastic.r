
#==========================================================================================
#  Project:  Janssen project

#    Part II: frequency table by outcome

#    Develop time: 9/3/2015 - .

#    Developer: Jie Zhao
#==========================================================================================
rm(list=ls())

library(glmnet)
library(xlsx)

options(digits=7)

data_path<- 'D:\\working materials\\Jassen Project\\01. Raw_data\\ForModel'
out_path<-'D:\\working materials\\Jassen Project\\03.Output\\Feb27\\sendout_Mar9'
setwd(out_path)
#names(raw_data)<- tolower(names(raw_data))
covar_definition <- read.table(paste(data_path, 'covar_definition.csv', sep='\\'), sep=',', head=T)
covar_definition[, 1]<- gsub(' $', '', covar_definition[, 1])
covar_model_df <- read.table(paste(data_path, 'covar_list_all.csv', sep='\\'), sep=',', head=T)
covar_model_df[, 1] <- gsub(' $', '', covar_model_df[, 1])


var_list_df <- read.table(paste(data_path, 'old_var_list.csv', sep='\\'), sep=',', header=F)
var_list_old <- var_list_df[,1]
var_list_old <- gsub(' +$', '', var_list_old, perl=T)
binary_list <- c('mi', 'cva', 'tia', 'othercvs', 'hypertension', 'cataract', 'glaucoma', 'diabetes')
conti_list <- c('central1mmretinalthickness', 'macularvolume', 'fovealthickness')
var_list <- c(binary_list, conti_list)
var_list_all <- tolower(c(var_list_old, var_list))
conti_list_all <- c(conti_list, 'age', 'bl_va', grep('^ct', var_list_all, value=T))
binary_list_all <- setdiff(var_list_all, conti_list_all)
#write.csv(var_list_all, paste(data_path, 'covar_list_all.csv', sep='\\'))

cohort <- 'mild_uni'
outcome <- 'ga_outcome'
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
        covar_list_model <- setdiff(covar_list_model, c('ivi_post', 'oct_bl', 'tx_any','ivi_any','other_tx_2y'))
    }else if(cohort=='mild_uni' & outcome=='adv_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 3])]
    }else if(cohort=='mild_bil' & outcome=='ga_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 4])]
    }else if(cohort=='mild_bil' & outcome=='adv_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 5])]
        covar_list_model <- setdiff(covar_list_model, c('ct_ivi_2y', 'other_tx_2y'))
    }else{
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 6])]
        covar_list_model <- setdiff(covar_list_model, c('ivi_any','tx_any','ivi_post'))
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
    
    coef<- data.frame(Coefficient=round(coef(step_wise), 7) , Odds_ratio=round(exp(coef(step_wise)), 7))
    #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
    
    p_value<- round(summary(step_wise)$coef[, "Pr(>|z|)"], 7)
    covariate <- rownames(coef)
    covar_definition_matched <- covar_definition[match(rownames(coef), covar_definition[, 1]), 'Definition'] 
    stepwise_output <- data.frame(Covariable=covariate, Definition=covar_definition_matched, coef, P_value=p_value, AIC=round(step_wise$aic, 2))
    write.xlsx(stepwise_output, file=paste('stepwise_output_', cohort, '_v3.xlsx', sep=''), sheetName=paste(cohort, '_by_', resp, sep=''), row.names=F, append=T, showNA=T)
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

missVar_list <- list()
pt_num <- function(cohort, outcome, n.model){
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check1 <- apply(apply(raw_data, 2, is.na), 2, sum)
    resp <- outcome
    response <- raw_data[, resp]
    if(cohort=='mild_uni' & outcome=='ga_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 2])]
        covar_list_model <- setdiff(covar_list_model, c('ivi_post', 'oct_bl', 'tx_any','ivi_any','other_tx_2y'))
    }else if(cohort=='mild_uni' & outcome=='adv_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 3])]
    }else if(cohort=='mild_bil' & outcome=='ga_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 4])]
    }else if(cohort=='mild_bil' & outcome=='adv_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 5])]
        covar_list_model <- setdiff(covar_list_model, c('ct_ivi_2y', 'other_tx_2y'))
    }else{
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 6])]
        covar_list_model <- setdiff(covar_list_model, c('ivi_any','tx_any','ivi_post'))
    }
    covar_list_model <- as.vector(covar_list_model)
    model_data <- cbind(raw_data[, covar_list_model], response)
    if(cohort=='ga_all'){
        model_data <- na.omit(model_data)
    }
    na_check <- apply(apply(model_data, 2, is.na), 2, sum)
    miss_list <- na_check[na_check>0]
    missVar_list[[n.model]] <<- miss_list 
    n.pt <- dim(model_data)[1]
    n.pt.1 <- length(response[response==1])
    result <- c(cohort, outcome, n.pt, n.pt.1)
    return(result)
}
output1 <- pt_num('mild_uni', 'ga_outcome', 1)
output2 <- pt_num('mild_uni', 'adv_outcome', 2)
output3 <- pt_num('mild_bil', 'ga_outcome', 3)
output4 <- pt_num('mild_bil', 'adv_outcome', 4)
output5 <- pt_num('ga_all', 'wet_outcome', 5)
output <- rbind(output1, output2, output3, output4, output5)



cohort <- 'mild_uni'
outcome <- 'adv_outcome'
seed <- 1
covar_model_df <- read.table(paste(data_path, 'covar_list_all.csv', sep='\\'), sep=',', head=T)
covar_model_df[, 1] <- gsub(' $', '', covar_model_df[, 1])

Model_Evaluate <- function(cohort, outcome){
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check1 <- apply(apply(raw_data, 2, is.na), 2, sum)
    resp <- outcome
    response <- raw_data[, resp]
    if(cohort=='mild_uni' & outcome=='ga_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 2])]
        covar_list_model <- setdiff(covar_list_model, c('ivi_post', 'oct_bl', 'tx_any','ivi_any','other_tx_2y'))
    }else if(cohort=='mild_uni' & outcome=='adv_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 3])]
    }else if(cohort=='mild_bil' & outcome=='ga_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 4])]
    }else if(cohort=='mild_bil' & outcome=='adv_outcome'){
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 5])]
        covar_list_model <- setdiff(covar_list_model, c('ct_ivi_2y', 'other_tx_2y'))
    }else{
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 6])]
        covar_list_model <- setdiff(covar_list_model, c('ivi_any','tx_any','ivi_post'))
    }
    covar_list_model <- as.vector(covar_list_model)
    model_data <- cbind(raw_data[, covar_list_model], response)
    na_check <- apply(apply(model_data, 2, is.na), 2, sum)
    
    training_data <- model_data
    fit<- glm(response~., data=training_data, family=binomial)
    step_wise<- step(fit , direction="backward")
    training_obs<- predict(step_wise, training_data, type="response")
    training_auc<- auc(training_data$response , training_obs)
    
    #  for estimated test auc on stepwise using 10-folds CV
    fold.num<-10
    if(cohort =='mild_uni' & outcome=='ga_outcome'){
        fold.num <- 5
    }else{
        fold.num <- 10
    }
    k.folds<- fold.num
    foldid<- nrow(training_data)
    set.seed(seed)
    foldid[training_data$response==1]<- sample(rep(1:k.folds, length=length(which(training_data$response==1))))
    set.seed(seed)
    foldid[training_data$response==0]<- sample(rep(1:k.folds, length=length(which(training_data$response==0))))
    table(training_data$response , foldid) # QC
    list_auc_stepwise_cv_test <- numeric()
    list_coef <- list()
    cv_auc<-matrix(nr=k.folds, nc=1)
    for(i in 1:k.folds){
        cv_training_data<- training_data[foldid!=i,]
        cv_test_data<- training_data[foldid==i,]
        
        #stepwise
        fit_std<- glm(response~., data=cv_training_data, family=binomial)
        step_wise<- step(fit_std , direction="both")
        coef<- data.frame(Coefficient=round(coef(step_wise), 7) , Odds_ratio=round(exp(coef(step_wise)), 7))
        training_pred<- predict(step_wise, cv_training_data, type="response")
        test_pred<- predict(step_wise, cv_test_data, type="response")
        
        #training_auc<- auc(cv_training_data$response , training_pred)
        test_auc<- auc(cv_test_data$response , test_pred)
        cv_auc[i,]<-test_auc
        #list_auc_stepwise_cv_test<- rbind(list_auc_stepwise_cv_test , test_auc)
        list_coef[[i]]<- coef
    }
    #obtain estimated auc of test data based on 10 folders CV
    #auc_foldAvg_stepwise <- apply(list_auc_stepwise_cv_test, 2, mean)
    auc_esti <- mean(cv_auc)
    #auc_esti <- auc_foldAvg_stepwise
    overfitting_diff <- training_auc-auc_esti
    overfitting_ratio <- overfitting_diff/training_auc
#    flag <- rbind(flag , c(Cohort=cohort, Outcome=outcome))
    auc_output <- data.frame(Cohort=cohort, Outcome=outcome, AUC_training=training_auc, AUC_esti=auc_esti, Overfitting_diff=overfitting_diff, Overfitting_ratio=overfitting_ratio)
    return(auc_output)
}

output1 <- Model_Evaluate('mild_uni','ga_outcome')
output2 <- Model_Evaluate('mild_uni','adv_outcome')
output3 <- Model_Evaluate('mild_bil','ga_outcome')
output4 <- Model_Evaluate('mild_bil','adv_outcome')
output5 <- Model_Evaluate('ga_all','wet_outcome')

cohort <- 'mild_bil'
outcome <- 'adv_outcome'
coef_allModel_list <- list()
Model_Evaluate_v2 <- function(cohort, outcome,kfold){
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check1 <- apply(apply(raw_data, 2, is.na), 2, sum)
    resp <- outcome
    response <- raw_data[, resp]
    if(cohort=='mild_uni' & outcome=='ga_outcome'){
        j<-1
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 2])]
        covar_list_model <- setdiff(covar_list_model, c('ivi_post', 'oct_bl', 'tx_any','ivi_any','other_tx_2y'))
    }else if(cohort=='mild_uni' & outcome=='adv_outcome'){
        j<-2
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 3])]
    }else if(cohort=='mild_bil' & outcome=='ga_outcome'){
        j<-3
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 4])]
    }else if(cohort=='mild_bil' & outcome=='adv_outcome'){
        j<-4
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 5])]
        covar_list_model <- setdiff(covar_list_model, c('ct_ivi_2y', 'other_tx_2y'))
    }else{
        j<-5
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 6])]
        covar_list_model <- setdiff(covar_list_model, c('ivi_any','tx_any','ivi_post'))
    }
    covar_list_model <- as.vector(covar_list_model)
    model_data <- cbind(raw_data[, covar_list_model], response)
    na_check <- apply(apply(model_data, 2, is.na), 2, sum)
    training_data<-model_data
    
    fit_stepwise<- glm(response~., data=training_data, family=binomial)
    step_wise<- step(fit_stepwise , direction="both")                                      # select backword method
    coef<- data.frame(Coefficient=round(coef(step_wise), 7) , Odds_ratio=round(exp(coef(step_wise)), 7))
    #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
    
    p_value<- round(summary(step_wise)$coef[, "Pr(>|z|)"], 7)
    covariate <- rownames(coef)
    covar_definition_matched <- covar_definition[match(rownames(coef), covar_definition[, 1]), 'Definition'] 
    stepwise_output <- data.frame(Covariable=covariate, Definition=covar_definition_matched, coef, P_value=p_value, AIC=round(step_wise$aic, 2))
    write.xlsx(stepwise_output, file=paste('stepwise_output_', cohort, '_v5.xlsx', sep=''), sheetName=paste(cohort, '_by_', resp, sep=''), row.names=F, append=T, showNA=T)
    
    training_obs<- predict(step_wise, training_data, type="response")
    
    training_auc<- auc(training_data$response , training_obs)
    
    
    #  for estimated test auc on stepwise using 10-folds CV
    #if (outcome=='ga_outcome' | ){
       # kfold <- 5
    #}else{
    #    kfold<- 10
    #}
    k.folds<- kfold
    foldid<- nrow(training_data)
    foldid[training_data$response==1]<- sample(rep(1:k.folds, length=length(which(training_data$response==1))))
    foldid[training_data$response==0]<- sample(rep(1:k.folds, length=length(which(training_data$response==0))))
    table(training_data$response , foldid) # QC
    cv_auc <- matrix(nr=k.folds , nc=1)                                         # store AUC for each fold
    coef_list <- list()
    for(i in 1:k.folds){
        cv_training_data<- training_data[foldid!=i,]
        cv_test_data<- training_data[foldid==i,]
        
        # Standard logistic regression
        cv_fit_stepwise<- glm(response~., data=cv_training_data, family=binomial) 
        cv_step_wise<-step(cv_fit_stepwise,direction="both")
        coef_stepwise<- data.frame(coefficient=coef(cv_step_wise) , odds_ratio=exp(coef(cv_step_wise)))
        coef_list[[i]] <- coef_stepwise
        test_pred<- predict(cv_step_wise, cv_test_data, type="response")
        test_pred_avg<- auc(cv_test_data$response , test_pred)
        
        cv_auc[i,]<- test_pred_avg   
    }
    coef_allModel_list[[j]] <- coef_list
    cv_auc_mean<- apply(cv_auc , 2 , function(x) mean(x[!is.na(x)])) 
    auc_esti <- cv_auc_mean
    #auc_esti <- auc_foldAvg_stepwise
    overfitting_diff <- training_auc-auc_esti
    overfitting_ratio <- overfitting_diff/training_auc
    #    flag <- rbind(flag , c(Cohort=cohort, Outcome=outcome))
    auc_output <- data.frame(Cohort=cohort, Outcome=outcome, AUC_training=training_auc, AUC_esti=auc_esti, Overfitting_diff=overfitting_diff, Overfitting_ratio=overfitting_ratio)
    return(auc_output)
    
    
}

output1 <- Model_Evaluate_v2('mild_uni','ga_outcome')
output2 <- Model_Evaluate_v2('mild_uni','adv_outcome')
output3 <- Model_Evaluate_v2('mild_bil','ga_outcome',5)
output4 <- Model_Evaluate_v2('mild_bil','adv_outcome')
output5 <- Model_Evaluate_v2('ga_all','wet_outcome',5)
output <- rbind(output1, output2, output3, output4, output5)
output <- rbind(output3,  output5)
write.xlsx(output, file=paste('auc_output_stepwise_v2.xlsx', sep=''), sheetName='overfitting1', row.names=F, append=T, showNA=T)


#10-folds elastic regression
cohort <- 'mild_uni'
outcome <- 'ga_outcome'
Model_Evaluate_elastic_v1 <- function(cohort, outcome){
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check1 <- apply(apply(raw_data, 2, is.na), 2, sum)
    resp <- outcome
    response <- raw_data[, resp]
    if(cohort=='mild_uni' & outcome=='ga_outcome'){
        j<-1
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 2])]
        covar_list_model <- setdiff(covar_list_model, c('ivi_post', 'oct_bl', 'tx_any','ivi_any','other_tx_2y'))
    }else if(cohort=='mild_uni' & outcome=='adv_outcome'){
        j<-2
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 3])]
    }else if(cohort=='mild_bil' & outcome=='ga_outcome'){
        j<-3
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 4])]
    }else if(cohort=='mild_bil' & outcome=='adv_outcome'){
        j<-4
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 5])]
        covar_list_model <- setdiff(covar_list_model, c('ct_ivi_2y', 'other_tx_2y'))
    }else{
        j<-5
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 6])]
        covar_list_model <- setdiff(covar_list_model, c('ivi_any','tx_any','ivi_post'))
    }
    covar_list_model <- as.vector(covar_list_model)
    model_data <- cbind(raw_data[, covar_list_model], response)
    na_check <- apply(apply(model_data, 2, is.na), 2, sum)
    training_data_lasso<-model_data
    
    
    #  for estimated test auc on stepwise using 10-folds CV
    if (cohort =='mild_uni' & outcome=='ga_outcome'){
        n.fold <- 5
    }else{
        n.fold<- 10
    }
    k.folds<-n.fold                                                       
    foldid<- nrow(training_data_lasso)
    foldid[training_data_lasso$response==1]<-sample(rep(1:k.folds, length=length(which(training_data_lasso$response==1))))
    foldid[training_data_lasso$response==0]<- sample(rep(1:k.folds, length=length(which(training_data_lasso$response==0))))
    table(training_data_lasso$response , foldid) # QC
    #list_lasso_foldid<- rbind(list_lasso_foldid , foldid)
    
    # Calculating initial lambda and the lambda sequence
    training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
    initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=1, standardize=F)$lambda  # calculating the initial lambda 
    length(initial_lambda) #100  decreasing 
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500)) # get a length=500 descending sequence from initial lambda to 0
    length(lambda_seq) #599 (0is the last one)
    $cv_auc <- matrix(nr=k.folds , nc=length(lambda_seq))
    dim(cv_auc)  #[1]   n.fold 599
    
    # Tenfold/five fold cross-validation
    cv_auc <- numeric()
    alpha_seq <- seq(0, 1, 0.1) #change by test result
    length(alpha_seq)   #[1] 11
    
    training_data <- training_data_lasso
    for(i in 1:k.folds){
        cv_training_data<- training_data[foldid!=i,]
        cv_test_data<- training_data[foldid==i,]
        
        # Standard logistic regression
        cv_fit_stepwise<- glm(response~., data=cv_training_data, family=binomial) 
        cv_step_wise<-step(cv_fit_stepwise,direction="both")
        coef_stepwise<- data.frame(coefficient=coef(cv_step_wise) , odds_ratio=exp(coef(cv_step_wise)))
        coef_list[[i]] <<- coef_stepwise
        test_pred<- predict(cv_step_wise, cv_test_data, type="response")
        test_pred_avg<- auc(cv_test_data$response , test_pred)
        
        cv_auc[i,]<- test_pred_avg
    }
    j <- 1
    for(j in alpha_seq){
        for(i in 1:k.folds){
            cv_training_data_lasso<- training_data_lasso[foldid!=i,]                                            # select 9 folds as training data
            cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
            cv_test_data_lasso<- training_data_lasso[foldid==i,]                                                # select 1 fold as test data
            cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1] #[1] 81 18
            
            fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                               lambda=lambda_seq, family="binomial", alpha=j, standardize=F)
            test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
            dim(test_pred) #[1]  81 599
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
    return(auc_output)
    
    
}

#10-folds elastic regression
cohort <- 'mild_uni'
outcome <- 'ga_outcome'
Model_Evaluate_elastic_v2 <- function(cohort, outcome, seed){
    
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check1 <- apply(apply(raw_data, 2, is.na), 2, sum)
    resp <- outcome
    response <- raw_data[, resp]
    if(cohort=='mild_uni' & outcome=='ga_outcome'){
        j<-1
        k.folds <- 5
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 2])]
        covar_list_model <- setdiff(covar_list_model, c('ivi_post', 'oct_bl', 'tx_any','ivi_any','other_tx_2y'))
    }else if(cohort=='mild_uni' & outcome=='adv_outcome'){
        j<-2
        k.folds <- 10
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 3])]
    }else if(cohort=='mild_bil' & outcome=='ga_outcome'){
        j<-3
        k.folds <- 5
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 4])]
    }else if(cohort=='mild_bil' & outcome=='adv_outcome'){
        j<-4
        k.folds <- 10
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 5])]
        covar_list_model <- setdiff(covar_list_model, c('ct_ivi_2y', 'other_tx_2y'))
    }else{
        j<-5
        k.folds <- 5
        covar_list_model <- covar_model_df$Covar[!is.na(covar_model_df[, 6])]
        covar_list_model <- setdiff(covar_list_model, c('ivi_any','tx_any','ivi_post'))
    }
    covar_list_model <- as.vector(covar_list_model)
    model_data <- cbind(raw_data[, covar_list_model], response)
    na_check <- apply(apply(model_data, 2, is.na), 2, sum)
    training_data_lasso<-model_data
    training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
    
    
    #run elastic
    foldid<- nrow(training_data_lasso)
    set.seed(seed)
    foldid[training_data_lasso$response==1]<-sample(rep(1:k.folds, length=length(which(training_data_lasso$response==1))))
    set.seed(seed)
    foldid[training_data_lasso$response==0]<- sample(rep(1:k.folds, length=length(which(training_data_lasso$response==0))))
    table(training_data_lasso$response , foldid) # QC
    
    alpha_seq <- seq(0, 1, 0.01)
    model_results <- numeric()
    alpha <- alpha_seq[1]
    for(alpha in alpha_seq){
        # Calculating initial lambda and the lambda sequence
        initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=alpha, standardize=F)$lambda  # calculating the initial lambda 
        length(initial_lambda) #100  decreasing 
        lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500)) # get a length=500 descending sequence from initial lambda to 0
        length(lambda_seq) #599 (0is the last one)
        cv_auc <- matrix(nr=k.folds , nc=length(lambda_seq))
        dim(cv_auc)  #[1]   n.fold 599
        
        #run 5/10 folds CV
        for(i in 1:k.folds){
            cv_training_data_lasso<- training_data_lasso[foldid!=i,]                                            # select 9 folds as training data
            cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
            cv_test_data_lasso<- training_data_lasso[foldid==i,]                                                # select 1 fold as test data
            cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1] #
            
            fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                               lambda=lambda_seq, family="binomial", alpha=alpha, standardize=F)
            test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
            dim(test_pred) 
            test_pred_avg<- apply(test_pred, 2, function(x){auc(cv_test_data_lasso$response , x)})
            test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
            cv_auc[i,] <- test_pred_avg
        }
        cv_auc_mean <- apply(cv_auc, 2, function(x){mean(x[!is.na(x)])})
        optimum_model <- which.max(cv_auc_mean)
        total_model <- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                              lambda=lambda_seq, family="binomial", alpha=alpha, standardize=F)
        optimum_lambda <- lambda_seq[optimum_model]
        #get AUC of training and test
        training_obs <- predict(total_model, training_matrix, type='response')[, optimum_model]
        training_auc <- auc(training_data_lasso$response, training_obs)
        test_auc <- max(cv_auc_mean)
        #get coef, odds ratio, p_value
        model_coef <- total_model$beta[, optimum_model]
        model_coef_keep <- model_coef[model_coef!=0]
        odds_ratio <- exp(model_coef_keep)
        coef_nm <- names(model_coef_keep)
        #get p_value
        model_var <- c('response', coef_nm)
        re_fit <- glm(response~., data=training_data_lasso[, match(coef_nm, names(training_data_lasso))], family=binomial)
        p_value <- summary(re_fit)$coef[, 'Pr(>|z|)'][-1]
        p_value <- p_value[match(coef_nm, names(p_value))]
        #store coef, odds ratio, p_value
        temp <- data.frame(alpha, coef_nm, model_coef_keep, odds_ratio, p_value, training_auc, test_auc)
        model_results <- rbind(model_results, temp)
        
    }#end of alpha
    model_select <- model_results[model_results$test_auc==max(model_results$test_auc),]
    model_select$Definition <- covar_definition[match(coef_nm, covar_definition[, 1]), 2]
    return(model_select)
}
output1 <- Model_Evaluate_elastic_v2('mild_uni', 'ga_outcome', 1234)
output2 <- Model_Evaluate_elastic_v2('mild_uni', 'adv_outcome', 1234)
output3 <- Model_Evaluate_elastic_v2('mild_bil', 'ga_outcome', 1234)
output4 <- Model_Evaluate_elastic_v2('mild_bil', 'adv_outcome', 1234)
output5 <- Model_Evaluate_elastic_v2('ga_all', 'wet_outcome', 1234)
