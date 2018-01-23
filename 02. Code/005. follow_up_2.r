#==========================================================================================
#  Project:  Janssen project

#    Part II: frequency table by outcome

#    Develop time: 18/3/2015 - .

#    Developer: Jie Zhao
#==========================================================================================
rm(list=ls())

library(glmnet)
library(xlsx)
library(snowfall)
options(digits=7)

data_path<- 'C:\\work\\jzhao\\working materials\\Jassen Project\\01. Raw_data\\ForModel'
out_path<-'C:\\work\\jzhao\\working materials\\Jassen Project\\03.Output\\sendout_May19'
setwd(out_path)

#names(raw_data)<- tolower(names(raw_data))
cohort <- 'mild_uni'
outcome <- 'ga_outcome'
covar_definition <- read.table(paste(data_path, 'covar_definition_ning.csv', sep='\\'), sep=',', header=T)
covar_definition[, 2] <- tolower(covar_definition[, 2])
x <- gsub('^ +| +$', '', covar_definition[, 2], perl=T,)

#correlation of the covariates: grep('^ct', var_list_all, value=T)
cohort <- 'mild_bil'
outcome <- 'ga_outcome'
corr <- function(cohort, outcome, thresh){
  data_file<- paste('ModelData_', cohort, '_by_', outcome, '_v2.csv', sep='')
  raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
  names(raw_data)<- tolower(names(raw_data))
  
  dim(raw_data)
  na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
  raw_data <- na.omit(raw_data)
  na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
  #covariance_matrix <- cor(subset(raw_data, select=-(response)))
  covariance_matrix <- data.frame(cor(raw_data))
  definition <- covar_definition[match(rownames(covariance_matrix), covar_definition[, 2]), 'Description']
  covariance_matrix <- data.frame(Definition=definition, covariance_matrix)
  write.xlsx(covariance_matrix , 'covariance_matrix_resp.xlsx', sheetName=paste(cohort, '_by_', outcome), row.names=T, append=T, showNA=T)
  pdf("covariance_matrix.pdf", height=6, width=8, pointsize=12)
  heatmap(as.matrix(covariance_matrix[, -1]))
  dev.off()
  covar_matrix_diab <- covariance_matrix[rownames(covariance_matrix)=='diabetes',]
  write.xlsx(covar_matrix_diab , 'covariance_matrix_diab_resp.xlsx', sheetName=paste(cohort, '_by_', outcome), row.names=T, append=T, showNA=T)
  
  
}
output1 <- corr('mild_bil', 'ga_outcome', 0.3)
output2 <- corr('mild_bil', 'adv_outcome',  0.3)

corr <- function(cohort, outcome){
  data_file<- paste('ModelData_', cohort, '_by_', outcome, '_v2.csv', sep='')
  raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
  names(raw_data)<- tolower(names(raw_data))
  
  dim(raw_data)
  na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
  raw_data <- na.omit(raw_data)
  na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
  covariance_matrix <- data.frame(cor(subset(raw_data, select=-(response))))
  definition <- covar_definition[match(rownames(covariance_matrix), covar_definition[, 2]), 'Description']
  covariance_matrix <- data.frame(Definition=definition, covariance_matrix)
  
  #covariance_matrix <- cor(raw_data)
  write.xlsx(covariance_matrix , 'covariance_matrix.xlsx', sheetName=paste(cohort, '_by_', outcome), row.names=T, append=T, showNA=T)
  pdf("covariance_matrix.pdf", height=6, width=8, pointsize=12)
  heatmap(as.matrix(covariance_matrix[, -1]))
  dev.off()
  covar_matrix_diab <- covariance_matrix[rownames(covariance_matrix)=='diabetes',]
  write.xlsx(covar_matrix_diab , 'covariance_matrix_diab.xlsx', sheetName=paste(cohort, '_by_', outcome), row.names=T, append=T, showNA=T)
  
  
}
output1 <- corr('mild_bil', 'ga_outcome')
output2 <- corr('mild_bil', 'adv_outcome')


#rerun stepwise for bil_ga and bil_adv after removing glaucoma and diabetic
cohort <- 'mild_uni'
outcome <- 'ga_outcome'
covar_definition <- read.table(paste(data_path, 'covar_definition_ning.csv', sep='\\'), sep=',', header=T)
covar_definition[, 2] <- tolower(covar_definition[, 2])
x <- gsub('^ +| +$', '', covar_definition[, 2], perl=T,)
stepwise_v3 <- function(cohort, outcome){
  data_file<- paste('ModelData_', cohort, '_by_', outcome, '_v2.csv', sep='')
  raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
  names(raw_data)<- tolower(names(raw_data))
  
  dim(raw_data)
  raw_data <- subset(raw_data, select=-c(glaucoma, diabetes))
  na_check1 <- apply(apply(raw_data, 2, is.na), 2, sum)
  raw_data <- na.omit(raw_data)
  na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
  
  if(cohort=='mild_uni' & outcome=='ga_outcome'){
    raw_data[, 'oct_bl'] <- NULL
    raw_data[, 'hypertension']<- NULL
  }
  training_data <- raw_data
  fit<- glm(response~., data=training_data, family=binomial)
  step_wise<- step(fit , direction="both", keep=c("age", "bl_va"))
  
  coef<- data.frame(Coefficient=round(coef(step_wise), 7) , Odds_ratio=round(exp(coef(step_wise)), 7))
  #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
  
  p_value<- round(summary(step_wise)$coef[, "Pr(>|z|)"], 7)
  CIs_2.5 <- confint(step_wise)[, 1]
  CIs_97.5 <- confint(step_wise)[, 2]
  covariate <- rownames(coef)
  covar_definition_matched <- covar_definition[match(rownames(coef), covar_definition[, 2]), 'Description'] 
  stepwise_output <- data.frame(Covariable=covariate, Definition=covar_definition_matched, coef, P_value=p_value, Lower_bound=CIs_2.5, Upper_bound=CIs_97.5, AIC=round(step_wise$aic, 2))
  write.xlsx(stepwise_output, file=paste('stepwise_output_', cohort, '.xlsx', sep=''), sheetName=paste(cohort, '_by_', outcome, sep=''), row.names=F, append=T, showNA=T)
  #add flag for later rbind txt file
  stepwise_output_flag <- data.frame(cbind(Cohort=cohort, Response=outcome, covariate, stepwise_output))
  return(stepwise_output_flag)
  #write.table(stepwise_output, paste(cohort, '_', resp, '_stepwise_output.txt', sep=''), quote=F, row.names=F, sep='\t\t')
  
}
output3 <- stepwise_v3('mild_bil', 'ga_outcome') 
output4 <- stepwise_v3('mild_bil', 'adv_outcome') 

cohort <- 'mild_uni'
outcome <- 'ga_outcome'
covar_definition <- read.table(paste(data_path, 'covar_definition_ning.csv', sep='\\'), sep=',', header=T)
covar_definition[, 2] <- tolower(covar_definition[, 2])
x <- gsub('^ +| +$', '', covar_definition[, 2], perl=T,)

data_path<- 'C:\\work\\jzhao\\working materials\\Jassen Project\\01. Raw_data\\ForModel'
out_path<-'C:\\work\\working materials\\Jassen Project\\03.Output\\sendout_Jun19'
setwd(out_path)

stepwise_v4 <- function(cohort, outcome){
  data_file<- paste('ModelData_', cohort, '_by_', outcome, '_v2.csv', sep='')
  raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
  names(raw_data)<- tolower(names(raw_data))
  
  dim(raw_data)
  raw_data <- subset(raw_data, select=-c(glaucoma, diabetes))
  na_check1 <- apply(apply(raw_data, 2, is.na), 2, sum)
  raw_data <- na.omit(raw_data)
  na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
  
  if(cohort=='mild_uni' & outcome=='ga_outcome'){
    raw_data[, 'oct_bl'] <- NULL
    raw_data[, 'hypertension']<- NULL
  }
  training_data <- raw_data
  fit<- glm(response~., data=training_data, family=binomial)
  step_wise<- step(fit , direction="both", scope=list(lower=response ~ age + bl_va))
  
  coef<- data.frame(Coefficient=round(coef(step_wise), 7) , Odds_ratio=round(exp(coef(step_wise)), 7))
  #        coef<- data.frame(coefficient=round(coef(fit), 2) , odds_ratio=sprintf('%.2f', exp(coef(fit))))
  
  p_value<- round(summary(step_wise)$coef[, "Pr(>|z|)"], 7)
  CIs_2.5 <- confint(step_wise)[, 1]
  CIs_97.5 <- confint(step_wise)[, 2]
  covariate <- rownames(coef)
  covar_definition_matched <- covar_definition[match(rownames(coef), covar_definition[, 2]), 'Description'] 
  stepwise_output <- data.frame(Covariable=covariate, Definition=covar_definition_matched, coef, P_value=p_value, Lower_bound=CIs_2.5, Upper_bound=CIs_97.5, AIC=round(step_wise$aic, 2))
  write.xlsx(stepwise_output, file=paste('stepwise_output_', cohort, '.xlsx', sep=''), sheetName=paste(cohort, '_by_', outcome, sep=''), row.names=F, append=T, showNA=T)
  #add flag for later rbind txt file
  stepwise_output_flag <- data.frame(cbind(Cohort=cohort, Response=outcome, covariate, stepwise_output))
  return(stepwise_output_flag)
  #write.table(stepwise_output, paste(cohort, '_', resp, '_stepwise_output.txt', sep=''), quote=F, row.names=F, sep='\t\t')
  
}
output1 <- stepwise_v4('ga_all', 'wet_outcome') 
output2 <- stepwise_v4('mild_uni', 'adv_outcome') 


#10-folds elastic regression
cohort <- 'ga_all'
outcome <- 'wet_outcome'
Model_Evaluate_elastic_v2 <- function(cohort, outcome, seed){
    
    data_file<- paste('ModelData_', cohort, '_by_', outcome, '_v2.csv', sep='')
    raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    raw_data <- subset(raw_data, select=-c(glaucoma, diabetes)) # delete glaucoma and diabetes
    
    dim(raw_data)
    na_check1 <- apply(apply(raw_data, 2, is.na), 2, sum)
    
    raw_data_noMiss <- na.omit(raw_data)
    
    n.pt <- dim(raw_data)[1]
    n.pt.outcome1 <- length(raw_data$response[raw_data$response==1]) 
    n.pt.noMiss <- dim(raw_data_noMiss)[1]
    n.pt.outcome1.noMiss <- length(raw_data_noMiss$response[raw_data_noMiss$response==1])
    training_data_lasso <- raw_data_noMiss
    training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
    k.folds<-10                                                       
    
    
    #run elastic
    foldid<- nrow(training_data_lasso)
    set.seed(seed)
    foldid[training_data_lasso$response==1]<-sample(rep(1:k.folds, length=length(which(training_data_lasso$response==1))))
    set.seed(seed)
    foldid[training_data_lasso$response==0]<- sample(rep(1:k.folds, length=length(which(training_data_lasso$response==0))))
    table(training_data_lasso$response , foldid) # QC
    
    alpha_seq <- seq(0, 1, 0.01)
    model_results <- numeric()
    alpha <- alpha_seq[2]
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
        temp <- data.frame(alpha, optimum_lambda, coef_nm, model_coef_keep, odds_ratio, p_value, training_auc, test_auc)
        model_results <- rbind(model_results, temp)
        
    }#end of alpha
    model_results$overfit <- model_results$training_auc-model_results$test_auc
    model_select1 <- model_results[model_results$test_auc==max(model_results$test_auc),]
    model_select2 <- model_select1[model_select1$overfit==min(model_select1$overfit),]
    alpha_levels <- levels(as.factor(model_select2$alpha))
    model_select <- model_select2[model_select2$alpha==alpha_levels[1], ]
    model_select$Definition <- covar_definition[match(model_select$coef_nm, covar_definition[, 2]), 4]
    #model_select[, c('N_PT', 'N_PT_Outcome1')] <- c(n.pt, n.pt.outcome1)
    write.xlsx(model_select, file=paste('elastic_AUC_24_', cohort, '.xlsx', sep=''), sheetName=outcome, row.names=F, append=T, showNA=T)
    
    return(model_select)
}
#output1 <- Model_Evaluate_elastic_v2('mild_uni', 'ga_outcome')
#output2 <- Model_Evaluate_elastic_v2('mild_uni', 'adv_outcome')
output3 <- Model_Evaluate_elastic_v2('mild_bil', 'ga_outcome', 1234)
output4 <- Model_Evaluate_elastic_v2('mild_bil', 'adv_outcome', 1234)
#output5 <- Model_Evaluate_elastic_v2('ga_all', 'wet_outcome', 1234)

