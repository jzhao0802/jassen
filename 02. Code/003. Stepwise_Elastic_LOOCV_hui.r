
#==========================================================================================
#  Project:  Janssen project

#    Part II: stepwise and elastic using LOOCV (Leave-one-out Cross Validation)

#    Develop time: 12/3/2015 - .

#    Developer: Jie Zhao
#==========================================================================================
rm(list=ls())

library(glmnet)
library(xlsx)
library(ROCR)

options(digits=7)

data_path<- 'D:\\working materials\\Jassen Project\\01. Raw_data\\ForModel'
out_path<-'D:\\working materials\\Jassen Project\\03.Output\\Feb27\\sendout_Mar19'
setwd(out_path)


cohort <- 'mild_uni'
outcome <- 'ga_outcome'
coef_allModel_list <- list()



LOOCV_stepwise_v1 <- function(cohort, outcome){
  
  
    data_file<- paste('ModelData_',cohort,'_by_',outcome,'.csv', sep='')
    model_data_raw <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    data_file2 <- paste(cohort, '_2y2_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file2, sep='\\'), sep=',', head=T)
    #raw_data <- raw_data[1:10, ] #for code test
    names(raw_data)<- tolower(names(raw_data))
    names(model_data_raw) <- tolower(names(model_data_raw))
    
    bl_va <- raw_data$bl_va
    model_data_temp <- subset(model_data_raw, select=-c(ct_ivi_2y, va_bl))
    model_data <- data.frame(cbind(model_data_temp, bl_va))
    
    training_data<-model_data
    
    fit_stepwise<- glm(response~., data=training_data, family=binomial)
    step_wise<- step(fit_stepwise , direction="both")                                      # select backword method
    coef<- data.frame(Coefficient=round(coef(step_wise), 7) , Odds_ratio=round(exp(coef(step_wise)), 7))
    #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
    
    p_value<- round(summary(step_wise)$coef[, "Pr(>|z|)"], 7)
    covariate <- rownames(coef)
    covar_definition_matched <- covar_definition[match(rownames(coef), covar_definition[, 1]), 'Definition'] 
    stepwise_output <- data.frame(Covariable=covariate, Definition=covar_definition_matched, coef, P_value=p_value, AIC=round(step_wise$aic, 2))
    #write.xlsx(stepwise_output, file=paste('stepwise_output_loocv', cohort, '_v1.xlsx', sep=''), sheetName=paste(cohort, '_by_', resp, sep=''), row.names=F, append=T, showNA=T)
    
    training_obs<- predict(step_wise, training_data, type="response")
    
    training_auc<- auc(training_data$response , training_obs)
    
    #cv_auc <- matrix(nr=dim(training_data)[1] , nc=1)                                         # store AUC for each observation left out of 
    test_pred_list <- numeric()
    coef_list <- list()

    for(i in 1:nrow(training_data)){
        cv_training_data<- training_data[-i, ]
        cv_test_data<- training_data[i,]
        
        # Stepwise logistic regression
        cv_fit_stepwise<- glm(response~., data=cv_training_data, family=binomial) 
        cv_step_wise<-step(cv_fit_stepwise,direction="both")
        coef_stepwise<- data.frame(coefficient=coef(cv_step_wise) , odds_ratio=exp(coef(cv_step_wise)))
        coef_list[[i]] <- coef_stepwise
        test_pred<- predict(cv_step_wise, cv_test_data, type="response")
        test_pred_list <- rbind(test_pred_list, test_pred)
        #test_pred_avg<- auc(cv_test_data$response , test_pred)
        
        #cv_auc[i,]<- test_pred_avg   
    }
    coef_allModel_list[[j]] <<- coef_list
    #cv_auc_mean<- apply(cv_auc , 2 , function(x) mean(x[!is.na(x)])) 
    #auc_esti <- cv_auc_mean
    #auc_esti <- auc_foldAvg_stepwise
    #    flag <- rbind(flag , c(Cohort=cohort, Outcome=outcome))
    roc <- roc(response, test_pred_list, plot=T, auc=T)
    auc_esti <- roc$auc 
    
    loocv_auc_stepwise <- auc(training_data$response, test_pred_list)
    
    
    overfitting_diff <- training_auc-auc_esti
    overfitting_ratio <- overfitting_diff/auc_esti
    
    auc_output <- data.frame(Cohort=cohort, Outcome=outcome, AUC_training=training_auc, AUC_esti=auc_esti, Overfitting_diff=overfitting_diff, Overfitting_ratio=overfitting_ratio)
    return(auc_output)
    
    
}

#output1 <- LOOCV_stepwise_v1('mild_uni','ga_outcome')
#output2 <- LOOCV_stepwise_v1('mild_uni','adv_outcome')
output3 <- LOOCV_stepwise_v1('mild_bil','ga_outcome')
output4 <- LOOCV_stepwise_v1('mild_bil','adv_outcome')
output5 <- LOOCV_stepwise_v1('ga_all','wet_outcome')
output <- rbind(output1, output2, output3, output4, output5)
#output <- rbind(output3,  output5)
write.xlsx(output, file=paste('LOOCV_stepwise_v1.xlsx', sep=''), sheetName='overfitting', row.names=F, append=T, showNA=T)



#10-folds elastic regression
cohort <- 'mild_uni'
outcome <- 'ga_outcome'
loocv_elastic_v2 <- function(cohort, outcome){
    
  data_file<- paste('ModelData_',cohort,'_by_',outcome,'.csv', sep='')
  model_data_raw <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
  data_file2 <- paste(cohort, '_2y2_v2.csv', sep='')
  raw_data <- read.table(paste(data_path, data_file2, sep='\\'), sep=',', head=T)
  #raw_data <- raw_data[1:10, ] #for code test
  names(raw_data)<- tolower(names(raw_data))
  names(model_data_raw) <- tolower(names(model_data_raw))
  
  bl_va <- raw_data$bl_va
  model_data_temp <- subset(model_data_raw, select=-c(ct_ivi_2y, va_bl))
  model_data <- data.frame(cbind(model_data_temp, bl_va))
  
  training_data_lasso<-na.omit(model_data)
    training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
    
    
    #run elastic
    alpha_seq <- seq(0, 1, 0.01)
    model_results <- numeric()
    alpha <- 0.02
    for(alpha in alpha_seq){
        # Calculating initial lambda and the lambda sequence
        initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=alpha, standardize=F)$lambda  # calculating the initial lambda 
        length(initial_lambda) #100  decreasing 
        lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500)) # get a length=500 descending sequence from initial lambda to 0
        length(lambda_seq) #599 (0is the last one)
        #cv_auc <- matrix(nr=dim(training_data_lasso)[1] , nc=length(lambda_seq))
        #dim(cv_auc)  #[1]   n.fold 599
        
        #run LOOCV
        test_pred_list <- matrix(nr=nrow(training_data_lasso), nc=length(lambda_seq))
       i <- 1
        for(i in 1:nrow(training_data_lasso)){
            cv_training_data_lasso<- training_data_lasso[-i,]                                            # select data except ith row as training data
            cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
            cv_test_data_lasso<- training_data_lasso[i,]                                                # select the ith row as test data
            cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1] #
            cv_test_matrix <- t(cv_test_matrix) #transpose vector as matrix whose dim is [1, 26]
            
            fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                               lambda=lambda_seq, family="binomial", alpha=alpha, standardize=F)
            test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
            dim(test_pred) 
            test_pred_list[i, ] <- test_pred
            #test_pred_avg<- apply(test_pred, 2, function(x){auc(cv_test_data_lasso$response , x)})
            #test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
            #cv_auc[i,] <- test_pred_avg
        }
        
        #cv_auc_mean <- apply(cv_auc, 2, function(x){mean(x[!is.na(x)])})
        auc_summary <- apply(test_pred_list, 2, function(x){auc(training_data_lasso$response, x)})
        optimum_model <- which.max(auc_summary)
        total_model <- glmnet(training_matrix, training_data_lasso$response, 
                              lambda=lambda_seq, family="binomial", alpha=alpha, standardize=F) #using the full training data
        
        optimum_lambda <- lambda_seq[optimum_model]
        #get AUC of training and test
        training_obs <- predict(total_model, training_matrix, type='response')[, optimum_model]
        training_auc <- as.vector(auc(training_data_lasso$response, training_obs))
        test_auc <- max(auc_summary)
        #get coef, odds ratio, p_value
        model_coef <- total_model$beta[, optimum_model]
        model_coef_keep <- model_coef[model_coef!=0]
        if(length(model_coef_keep)==0){
            temp <- data.frame(alpha, coef_nm=NA, model_coef_keep=NA, odds_ratio=NA, p_value=NA, training_auc, test_auc=test_auc)
            
        }else{
            odds_ratio <- exp(model_coef_keep)
            coef_nm <- names(model_coef_keep)
            #get p_value
            model_var <- c('response', coef_nm)
            re_fit <- glm(response~., data=training_data_lasso[, match(model_var, names(training_data_lasso))], family=binomial)
            p_value <- summary(re_fit)$coef[, 'Pr(>|z|)'][-1]
            p_value <- p_value[match(coef_nm, names(p_value))]
            #store coef, odds ratio, p_value
            temp <- data.frame(alpha, coef_nm, model_coef_keep, odds_ratio, p_value, training_auc, test_auc)
            
        }
        model_results <- rbind(model_results, temp)
        
    }#end of alpha
    model_select <- model_results[model_results$test_auc==max(model_results$test_auc),]
    #model_select$Definition <- covar_definition[match(coef_nm, covar_definition[, 1]), 2]
    return(model_select)
}
#output1 <- Model_Evaluate_elastic_v2('mild_uni', 'ga_outcome')
#output2 <- Model_Evaluate_elastic_v2('mild_uni', 'adv_outcome')
output3 <- Model_Evaluate_elastic_v2('mild_bil', 'ga_outcome')
output4 <- Model_Evaluate_elastic_v2('mild_bil', 'adv_outcome')
output5 <- Model_Evaluate_elastic_v2('ga_all', 'wet_outcome')



