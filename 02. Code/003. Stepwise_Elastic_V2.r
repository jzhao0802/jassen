
#==========================================================================================
#  Project:  Janssen project

#    Part II: frequency table by outcome

#    Develop time: 18/3/2015 - .

#    Developer: Jie Zhao
#==========================================================================================
rm(list=ls())

library(glmnet)
library(xlsx)

options(digits=7)

data_path<- 'D:\\working materials\\Jassen Project\\01. Raw_data\\ForModel'
out_path<-'D:\\working materials\\Jassen Project\\03.Output\\Feb27\\sendout_Mar18'
setwd(out_path)
#names(raw_data)<- tolower(names(raw_data))
covar_definition <- read.table(paste(data_path, 'covar_definition.csv', sep='\\'), sep=',', head=T)
covar_definition[, 1]<- gsub(' $', '', covar_definition[, 1])
#covar_model_df <- read.table(paste(data_path, 'covar_list_all.csv', sep='\\'), sep=',', head=T)
#covar_model_df[, 1] <- gsub(' $', '', covar_model_df[, 1])


var_list_old <- covar_definition[covar_definition[, 3]==0, 1]
length(var_list_old) # 22
#var_list_old <- gsub(' +$', '', var_list_old, perl=T)

cohort <- 'mild_bil'
binary_list <- c('mi', 'cva', 'tia', 'othercvs', 'hypertension', 'cataract', 'glaucoma', 'diabetes')
conti_list <- c('central1mmretinalthickness', 'macularvolume', 'fovealthickness')
var_list <- c(binary_list, conti_list)
var_list_all <- tolower(c(var_list_old, var_list)) #33=22(old)+11(new)
conti_list_all <- c(conti_list, 'age', 'va_bl', grep('^ct', var_list_all, value=T)) # 13
binary_list_all <- setdiff(var_list_all, conti_list_all) #20
#write.csv(var_list_all, paste(data_path, 'covar_list_all.csv', sep='\\'))

#prepare the model data according to Ning's covariate selection result.
#for mild_bil_by_ga mild_uni_by_ga mild_uni_by_adv ga_all_wet there are 14 covariate kept for stepwise
#for mild_bil_by_adv there are 13 covariate kept for stepwise, the one more excluded covariate is ct_ivi_2y
#note that otherCVs is an complicated one which will be coded according to mi, cva, tia and initial othercvs

covarForModel <- c('female', 'iop_bl', 'oct_bl', 'hypertension', 'cataract', 'glaucoma', 'diabetes', 'othercvs', 'age', 'va_bl', grep('^ct.+2y$', var_list_all, perl=T, value=T))
covarForModel_1 <- setdiff(covarForModel, 'othercvs')

prepare_modelData <- function(cohort, outcome){
    data_file<- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check1 <- apply(apply(raw_data, 2, is.na), 2, sum)
    resp <- outcome
    response <- raw_data[, resp]
    othercvs <- ifelse(apply(raw_data[, c('mi', 'cva', 'tia', 'othercvs')], 1, sum)>0, 1, 0)
    modelData <- raw_data[, covarForModel_1]
    modelData$othercvs <- othercvs
    modelData$response <- response
    if (cohort == 'mild_bil' & outcome == 'adv_outcome'){
        modelData$ct_ivi_2y <- NULL
    }
    na_check <- apply(apply(modelData, 2, is.na), 2, sum)
    write.csv(modelData, file=paste(data_path, '\\ModelData_',cohort, '_by_', outcome, '.csv', sep=''), row.names=F)
    
}
prepare_modelData('mild_bil', 'ga_outcome')
prepare_modelData('mild_bil', 'adv_outcome')
prepare_modelData('mild_uni', 'ga_outcome')
prepare_modelData('mild_uni', 'adv_outcome')
prepare_modelData('ga_all', 'wet_outcome')
#Model data preparation ended


#correlation of the covariates: grep('^ct', var_list_all, value=T)
cohort <- 'mild_bil'
outcome <- 'ga_outcome'
corr <- function(cohort, outcome, thresh){
    data_file<- paste('ModelData_', cohort, '_by_', outcome, '.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    
    covar_forCorr <- grep('^ct', names(raw_data), value=T)
    
    data_forCorr <- raw_data[, covar_forCorr]
    covariance_matrix <- cor(data_forCorr)
    write.xlsx(covariance_matrix , 'covariance_matrix.xlsx', sheetName=cohort, row.names=T, append=T, showNA=T)
    pdf("covariance_matrix.pdf", height=6, width=8, pointsize=12)
    heatmap(covariance_matrix)
    dev.off()
    
    high_corr <- numeric()
    for(i in 2:length(covar_forCorr)-1){
        for(j in (i+1):length(covar_forCorr)){
            if(covariance_matrix[i,j] > thresh){
                #print(paste(covar_forCorr[i] , covar_forCorr[j] , sep=' : '))
                high_corr <- rbind(high_corr, c(Cohort=cohort, Var1=covar_forCorr[i], Var2=covar_forCorr[j], Corr=covariance_matrix[i,j]))
            }
        }
    }
    high_corr <- as.data.frame(high_corr)
    #write.xlsx(high_corr , 'high_covariance_list.xlsx', sheetName=cohort, row.names=T, append=T, showNA=T)
    return(high_corr)
    
}
output1 <- corr('mild_bil', 'ga_outcome', 0.3)
output2 <- corr('mild_uni', 'ga_outcome',  0.3)
output3 <- corr('ga_all', 'wet_outcome',  0.3)
output_high_corr <- rbind(output1, output2, output3)
write.xlsx(output_high_corr , 'high_correlation.xlsx', sheetName=cohort, row.names=T, append=T, showNA=T)



#stepwise regression using model data created above
cohort <- 'mild_bil'
outcome <- 'ga_outcome'
covar_definition <- read.table(paste(data_path, 'covar_definition_ning.csv', sep='\\'), sep=',', header=T)
covar_definition[, 2] <- tolower(covar_definition[, 2])
x <- gsub('^ +| +$', '', covar_definition[, 2], perl=T,)
stepwise_v3 <- function(cohort, outcome){
    data_file<- paste('ModelData_', cohort, '_by_', outcome, '.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    if(cohort=='mild_uni' & outcome=='ga_outcome'){
        raw_data[, 'oct_bl'] <- NULL
        raw_data[, 'hypertension']<- NULL
    }
    training_data <- raw_data
    fit<- glm(response~., data=training_data, family=binomial)
    step_wise<- step(fit , direction="both")
    
    coef<- data.frame(Coefficient=round(coef(step_wise), 7) , Odds_ratio=round(exp(coef(step_wise)), 7))
    #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
    
    p_value<- round(summary(step_wise)$coef[, "Pr(>|z|)"], 7)
    CIs_2.5 <- confint(step_wise)[, 1]
    CIs_97.5 <- confint(step_wise)[, 2]
    covariate <- rownames(coef)
    covar_definition_matched <- covar_definition[match(rownames(coef), covar_definition[, 2]), 'Description'] 
    stepwise_output <- data.frame(Covariable=covariate, Definition=covar_definition_matched, coef, P_value=p_value, CIs_2.5=CIs_2.5, CIs_97.5=CIs_97.5, AIC=round(step_wise$aic, 2))
    write.xlsx(stepwise_output, file=paste('stepwise_output_v2', cohort, '.xlsx', sep=''), sheetName=paste(cohort, '_by_', outcome, sep=''), row.names=F, append=T, showNA=T)
    #add flag for later rbind txt file
    stepwise_output_flag <- data.frame(cbind(Cohort=cohort, Response=resp, covariate, stepwise_output))
    return(stepwise_output_flag)
    #write.table(stepwise_output, paste(cohort, '_', resp, '_stepwise_output.txt', sep=''), quote=F, row.names=F, sep='\t\t')
    
}
output1 <- stepwise_v3('mild_uni', 'ga_outcome') #warnings
output2 <- stepwise_v3('mild_uni', 'adv_outcome')
output3 <- stepwise_v3('mild_bil', 'ga_outcome') #warnings
output4 <- stepwise_v3('mild_bil', 'adv_outcome') 
output5 <- stepwise_v3('ga_all', 'wet_outcome')

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
outcome <- 'ga_outcome'
seed <- 1234
covar_model_df <- read.table(paste(data_path, 'covar_list_all.csv', sep='\\'), sep=',', head=T)
covar_model_df[, 1] <- gsub(' $', '', covar_model_df[, 1])

Model_Evaluate <- function(cohort, outcome, seed){
    data_file<- paste('ModelData_', cohort, '_by_', outcome, '.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check1 <- apply(apply(raw_data, 2, is.na), 2, sum)
    
    n.pt <- dim(raw_data)[1]
    n.pt.outcome1 <- length(raw_data$response[raw_data$response==1]) 
    training_data <- raw_data
    fit<- glm(response~., data=training_data, family=binomial)
    step_wise<- step(fit , direction="both")
    training_obs<- predict(step_wise, training_data, type="response")
    training_auc<- auc(training_data$response , training_obs)
    
    #  for estimated test auc on stepwise using 10-folds CV
    fold.num<-10
    if(cohort =='mild_uni' & outcome=='ga_outcome'){
        fold.num <- 10
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
    auc_esti <- mean(cv_auc, na.rm=T)
    #auc_esti <- auc_foldAvg_stepwise
    overfitting_diff <- training_auc-auc_esti
    overfitting_ratio <- overfitting_diff/training_auc
    #    flag <- rbind(flag , c(Cohort=cohort, Outcome=outcome))
    auc_output <- data.frame(Cohort=cohort, Outcome=outcome, N_PT=n.pt, N_PT_Outcome1=n.pt.outcome1, AUC_training=training_auc, AUC_esti=auc_esti, Overfitting_diff=overfitting_diff, Overfitting_ratio=overfitting_ratio)
    return(auc_output)
}

output1 <- Model_Evaluate('mild_uni','ga_outcome', 1234)
output2 <- Model_Evaluate('mild_uni','adv_outcome', 1234)
output3 <- Model_Evaluate('mild_bil','ga_outcome', 1234)
output4 <- Model_Evaluate('mild_bil','adv_outcome', 1234)
output5 <- Model_Evaluate('ga_all','wet_outcome', 1234)
output_stepwise <- rbind(output1, output2, output3, output4, output5)
write.xlsx(output_stepwise, file=paste('auc_output_stepwise.xlsx', sep=''), sheetName='overfitting', row.names=F, append=T, showNA=T)



#10-folds elastic regression
cohort <- 'ga_all'
outcome <- 'wet_outcome'
Model_Evaluate_elastic_v2 <- function(cohort, outcome, seed){
    
    data_file<- paste('ModelData_', cohort, '_by_', outcome, '.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check1 <- apply(apply(raw_data, 2, is.na), 2, sum)
    
    n.pt <- dim(raw_data)[1]
    n.pt.outcome1 <- length(raw_data$response[raw_data$response==1]) 
    
    training_data_lasso <- raw_data
    k.folds<-10                                                       
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
        cv_training_response_pool <- list()
        cv_test_response_pool <- list()
        test_pred_pool <- list()
        #run 5/10 folds CV
        i <- 1
        for(i in 1:k.folds){
            cv_training_data_lasso<- training_data_lasso[foldid!=i,]                                            # select 9 folds as training data
            cv_training_response_pool[[i]]<- cv_training_data_lasso$response
            cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
            cv_test_data_lasso<- training_data_lasso[foldid==i,]                                                # select 1 fold as test data
            cv_test_response_pool[[i]] <- cv_test_data_lasso$response
            cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1] #
            
            fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                               lambda=lambda_seq, family="binomial", alpha=alpha, standardize=F)
            test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
            test_pred_pool[[i]] <- test_pred
            dim(test_pred) 
            test_pred_avg1<- apply(test_pred, 2, function(x){auc(cv_test_data_lasso$response , x)})
            test_pred_avg<- c(test_pred_avg1 , rep(NA , length(lambda_seq) - length(test_pred_avg1))) # some small lambda may not be reached
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
        re_fit <- glm(response~., data=training_data_lasso[, match(model_var, names(training_data_lasso))], family=binomial)
        p_value <- summary(re_fit)$coef[, 'Pr(>|z|)'][-1]
        p_value <- p_value[match(coef_nm, names(p_value))]
        #store coef, odds ratio, p_value
        temp <- data.frame(alpha, coef_nm, model_coef_keep, odds_ratio, p_value, training_auc, test_auc)
        model_results <- rbind(model_results, temp)
        
    }#end of alpha
    model_results$overfit <- model_results$training_auc-model_results$test_auc
    model_select1 <- model_results[model_results$test_auc==max(model_results$test_auc),]
    model_select2 <- model_select1[model_select1$overfit==min(model_select1$overfit),]
    alpha_levels <- levels(as.factor(model_select2$alpha))
    model_select <- model_select2[model_select2$alpha==alpha_levels[1], ]
    model_select$Definition <- covar_definition[match(model_select$coef_nm, covar_definition[, 2]), 4]
    #model_select[, c('N_PT', 'N_PT_Outcome1')] <- c(n.pt, n.pt.outcome1)
    write.xlsx(model_select, file=paste('elastic_output_', cohort, '_noseed.xlsx', sep=''), sheetName=outcome, row.names=F, append=T, showNA=T)
    
    return(model_select)
}
output1 <- Model_Evaluate_elastic_v2('mild_uni', 'ga_outcome', 1234)
output2 <- Model_Evaluate_elastic_v2('mild_uni', 'adv_outcome', 1234)
output3 <- Model_Evaluate_elastic_v2('mild_bil', 'ga_outcome', 1234)
output4 <- Model_Evaluate_elastic_v2('mild_bil', 'adv_outcome', 1234)
output5 <- Model_Evaluate_elastic_v2('ga_all', 'wet_outcome', 1234)


