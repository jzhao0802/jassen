library(glmnet)
library(xlsx)

options(digits=2)

data_path<- 'D:\\working materials\\Jassen Project\\01. Raw_data'
out_path<-'D:\\working materials\\Jassen Project\\03.Output'
setwd(out_path)



#data QC on missing, constant for 4 cohort (ga_bil ga_all mild_bil mild_uni) without missing data
covar_definition <- read.table(paste(data_path, 'covar_definition.csv', sep='\\'), sep=',', head=T)


cohort <- 'mild_uni'
data_file<- paste(cohort, '_2y.csv', sep='')
raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
var_unused_list <- names(raw_data)[1:5]

cohort <- 'mild_uni'
resp <- 'ga_outcome'


stepwise_drop_recode <- function(cohort, response_input, log){
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
        if (i == 'ct_ivi_2y' ){
            raw_data_omitMiss[, paste(i, '_dich', sep='')] <- as.numeric(as.vector(cut(var, breaks=c(0, 3.1, max(var)), right=TRUE, include.lowest=TRUE, labels=c('0', '1'))))
            raw_data_omitMiss[, i] <- NULL
            
        }else if(i=='ct_oct_2y'){
            raw_data_omitMiss[, paste(i, '_dich', sep='')] <- as.numeric(as.vector(cut(var, breaks=c(0, 10.1, max(var)), right=TRUE, include.lowest=TRUE, labels=c('0', '1'))))
            raw_data_omitMiss[, i] <- NULL
            
        }else{
            raw_data_omitMiss[, paste(i, '_dich', sep='')] <- as.numeric(as.vector(cut(var, breaks=c(0, 0.01, max(var)), right=TRUE, include.lowest=TRUE, labels=c('0', '1'))))
            raw_data_omitMiss[, i] <- NULL
        }
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
    #fit<- glm(response~ age+bl_va+ct_ivi_2y_dich+ct_oct_2y_dich, data=training_data, family=binomial)
    step_wise<- step(fit , direction="both")
    coef<- data.frame(Coefficient=round(coef(step_wise), 7) , Odds_ratio=round(exp(coef(step_wise)), 7))
    #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
    
    p_value<- round(summary(step_wise)$coef[, "Pr(>|z|)"], 7)
    covar_definition_matched <- covar_definition[match(rownames(coef), covar_definition[, 1]), 'Definition'] 
    stepwise_output <- cbind(Definition=covar_definition_matched, coef, P_value=p_value)
    write.xlsx(stepwise_output, file='stepwise_output_recode.xlsx', sheetName=paste(cohort, '_by_', resp, sep=''), row.names=T, append=T, showNA=T)
    #add flag for later rbind txt file
    covariate <- rownames(stepwise_output)
    stepwise_output_flag <- data.frame(cbind(Cohort=cohort, Response=resp, covariate, stepwise_output))
    return(stepwise_output_flag)
    #write.table(stepwise_output, paste(cohort, '_', resp, '_stepwise_output.txt', sep=''), quote=F, row.names=F, sep='\t\t')
    
}



output1 <- stepwise_drop_recode('mild_uni', 'ga_outcome', F)
output2 <- stepwise_drop_recode('mild_uni', 'wet_outcome', F)
output3 <- stepwise_drop_recode('mild_bil', 'ga_outcome', F)
output4 <- stepwise_drop_recode('mild_bil', 'wet_outcome', F)
#rbind all the 4 model results and save as txt file
stepwise_output_bind <- rbind(output1, output2, output3, output4)
stepwise_output_bind <- output1
write.table(stepwise_output_bind, 'stepwise_output_recode.txt', quote=F, row.names=F, sep='\t\t')


#recode the continuous variables according to sigStepwise test
stepwise_recode2 <- function(cohort, response_input){
    data_file<- paste(cohort, '_2y.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    raw_data <- raw_data[, setdiff(names(raw_data), var_unused_list)]
    raw_data_omitMiss <- na.omit(raw_data)
    var_list <- names(raw_data_omitMiss)
    #response_list <- c('ga_outcome', 'wet_outcome')
    covar_list <- var_list[(length(var_list)-12):length(var_list)]
    conti_list1 <- c('age', 'bl_va')
    conti_list2 <- grep('^ct', covar_list, value=TRUE)
    conti_list <- c(conti_list1, conti_list2)
    binary_list <- setdiff(covar_list, c(log_conti_list, dich_conti_list))
    for(i in conti_list){
        if (cohort == 'mild_uni' & response_input == 'ga_outcome'){
            var <-  raw_data_omitMiss[, 'ct_ivi_2y']
            ct_ivi_2y_dich <- as.numeric(as.vector(cut(var, breaks=c(0, 3.1, max(var)), right=TRUE, include.lowest=TRUE, labels=c('0', '1'))))
            
            var <-  raw_data_omitMiss[, 'ct_oct_2y']
            ct_oct_2y_dich <- as.numeric(as.vector(cut(var, breaks=c(0, 10.1, max(var)), right=TRUE, include.lowest=TRUE, labels=c('0', '1'))))
            
            var <-  raw_data_omitMiss[, 'age']
            age_quarter <- cut(var, breaks=c(min(var), 59, 83, 85, max(var)), labels=c(0, 1, 2, 3), include.lowest=T)
            for_modelMtx <- data.frame(ct_ivi_2y_dich, ct_oct_2y_dich, age_quarter)
            modelMtx <- model.matrix(~., for_modelMtx)
        }
        
        if (cohort == 'mild_uni' & response_input == 'wet_outcome'){
            var <-  raw_data_omitMiss[, 'ct_ivi_2y']
            ct_ivi_2y_dich <- as.numeric(as.vector(cut(var, breaks=c(0, 0.01, max(var)), right=TRUE, include.lowest=TRUE, labels=c('0', '1'))))

            var <-  raw_data_omitMiss[, 'ct_oct_2y']
            ct_oct_2y_dich <- as.numeric(as.vector(cut(var, breaks=c(0, 0.01, max(var)), right=TRUE, include.lowest=TRUE, labels=c('0', '1'))))
                
            var <-  raw_data_omitMiss[, 'age']
            age_quarter <- cut(i, breaks=c(min(var), 65, 74, 81, max(var)), labels=c(0, 1, 2, 3), include.lowest=T)

            var <-  raw_data_omitMiss[, 'bl_va']
            bl_va_quarter <- cut(i, breaks=c(min(var), 60, 75, 78, max(var)), labels=c(0, 1, 2, 3), include.lowest=T)
            
            for_modelMtx <- data.frame(ct_ivi_2y_dich, ct_oct_2y_dich, age_quarter, bl_va_quarter)
            modelMtx <- model.matrix(~., for_modelMtx)
            
        }
        
        
        if (cohort == 'mild_bil' & response_input == 'ga_outcome'){
            var <-  raw_data_omitMiss[, 'ct_ivi_2y']
            ct_ivi_2y_dich <- as.numeric(as.vector(cut(var, breaks=c(0, 0.01, max(var)), right=TRUE, include.lowest=TRUE, labels=c('0', '1'))))
            
            var <-  raw_data_omitMiss[, 'ct_oct_2y']
            ct_oct_2y_dich <- as.numeric(as.vector(cut(var, breaks=c(0, 0.01, max(var)), right=TRUE, include.lowest=TRUE, labels=c('0', '1'))))
            
            var <-  raw_data_omitMiss[, 'bl_va']
            bl_va_quarter <- cut(var, breaks=c(min(var), 60, 75, 78, max(var)), labels=c(0, 1, 2, 3), include.lowest=T)
            
            for_modelMtx <- data.frame(ct_ivi_2y_dich, ct_oct_2y_dich, bl_va_quarter)
            modelMtx <- model.matrix(~., for_modelMtx)
            
        }
            
        
        if (cohort == 'mild_bil' & response_input == 'wet_outcome'){
            var <-  raw_data_omitMiss[, 'ct_oct_2y']
            ct_oct_2y_dich <- as.numeric(as.vector(cut(var, breaks=c(0, 10.1, max(var)), right=TRUE, include.lowest=TRUE, labels=c('0', '1'))))
                
            var <-  raw_data_omitMiss[, 'age']
            age_quarter <- cut(i, breaks=c(min(i), 65, 74, 81, max(i)), labels=c(0, 1, 2, 3), include.lowest=T)
            
            var <-  raw_data_omitMiss[, 'bl_va']
            bl_va_quarter <- cut(i, breaks=c(min(i), 60, 75, 78, max(i)), labels=c(0, 1, 2, 3), include.lowest=T)
            
            for_modelMtx <- data.frame(ct_oct_2y_dich, age_quarter, bl_va_quarter)
            modelMtx <- model.matrix(~., for_modelMtx)
            
        }
    }
    
    
    response <- raw_data_omitMiss[, response_input]
    model_data <- cbind(raw_data_omitMiss[, covar_trans_list], response)
    model_data1 <- model.matrix()
    training_data <- model_data
    fit<- glm(response~., data=training_data, family=binomial)
    #fit<- glm(response~ age+bl_va+ct_ivi_2y_dich+ct_oct_2y_dich, data=training_data, family=binomial)
    step_wise<- step(fit , direction="both")
    coef<- data.frame(Coefficient=round(coef(step_wise), 7) , Odds_ratio=round(exp(coef(step_wise)), 7))
    #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
    
    p_value<- round(summary(step_wise)$coef[, "Pr(>|z|)"], 7)
    covar_definition_matched <- covar_definition[match(rownames(coef), covar_definition[, 1]), 'Definition'] 
    stepwise_output <- cbind(Definition=covar_definition_matched, coef, P_value=p_value)
    write.xlsx(stepwise_output, file='stepwise_output_recode.xlsx', sheetName=paste(cohort, '_by_', resp, sep=''), row.names=T, append=T, showNA=T)
    #add flag for later rbind txt file
    covariate <- rownames(stepwise_output)
    stepwise_output_flag <- data.frame(cbind(Cohort=cohort, Response=resp, covariate, stepwise_output))
    return(stepwise_output_flag)
    #write.table(stepwise_output, paste(cohort, '_', resp, '_stepwise_output.txt', sep=''), quote=F, row.names=F, sep='\t\t')
    
}



output1 <- stepwise_drop_recode('mild_uni', 'ga_outcome', F)
output2 <- stepwise_drop_recode('mild_uni', 'wet_outcome', F)
output3 <- stepwise_drop_recode('mild_bil', 'ga_outcome', F)
output4 <- stepwise_drop_recode('mild_bil', 'wet_outcome', F)
#rbind all the 4 model results and save as txt file
stepwise_output_bind <- rbind(output1, output2, output3, output4)
stepwise_output_bind <- output1
write.table(stepwise_output_bind, 'stepwise_output_recode.txt', quote=F, row.names=F, sep='\t\t')




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

