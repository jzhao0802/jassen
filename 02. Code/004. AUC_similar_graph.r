#Y-Axis: % of cases with disease progression
#X-axis: % of cases targeted 
# for John
library(glmnet)
library(xlsx)
library(snowfall)
library(Hmisc)
options(digits=7)


data_path <- "C:\\work\\jzhao\\working materials\\Jassen Project\\01. Raw_data\\ForModel"
out_path<-'C:\\work\\jzhao\\working materials\\Jassen Project\\03.Output\\sendout_Apr16'
setwd(out_path)
inFile <- 'mild_uni_2y2_v2'
Ext <- '.csv'

raw_data <- read.table(paste(path, inFile, Ext, sep=''), sep=',', header=T)
#names(raw_data)<- tolower(names(raw_data))
covar_definition <- read.table(paste(data_path, 'covar_definition.csv', sep='\\'), sep=',', head=T)
covar_definition[, 1]<- gsub(' $', '', covar_definition[, 1])
#covar_model_df <- read.table(paste(data_path, 'covar_list_all.csv', sep='\\'), sep=',', head=T)
#covar_model_df[, 1] <- gsub(' $', '', covar_model_df[, 1])

cohort <- 'ga_all'
outcome <- 'wet_outcome'
Model_Evaluate_elastic_v2 <- function(cohort, outcome){
  
  data_file<- paste('ModelData_', cohort, '_by_', outcome, '_v2.csv', sep='')
  raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
  names(raw_data)<- tolower(names(raw_data))
  
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
  #set.seed(seed)
  foldid[training_data_lasso$response==1]<-sample(rep(1:k.folds, length=length(which(training_data_lasso$response==1))))
  #set.seed(seed)
  foldid[training_data_lasso$response==0]<- sample(rep(1:k.folds, length=length(which(training_data_lasso$response==0))))
  table(training_data_lasso$response , foldid) # QC
  
  #alpha_seq <- seq(0, 1, 0.01)
  alpha_seq <- 0.08
  model_results <- numeric()
  alpha <- alpha_seq
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
    optimum_lambda <- lambda_seq[optimum_model]
    total_model <- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                          lambda=lambda_seq, family="binomial", alpha=alpha, standardize=F)
    optimum_lambda <- lambda_seq[optimum_model]
    #get AUC of training and test
    training_obs <- predict(total_model, training_matrix, type='response')[, optimum_model]
    training_auc <- auc(training_data_lasso$response, training_obs)
    obsPred <- data.frame(obs=training_data_lasso$response, pred=training_obs)
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
  write.xlsx(obsPred, file=paste('forGraph_10folds_r3_', cohort, '.xlsx', sep=''), sheetName=outcome, row.names=F, append=T, showNA=T)
  write.xlsx(model_select, file=paste('elastic_coefResult_10folds_r3_', cohort, '.xlsx', sep=''), sheetName=outcome, row.names=F, append=T, showNA=T)
  
 # write.csv(obsPred, file=paste('forGraph_', cohort, '.xlsx', sep=''))
  #return(model_select)
 
  return(obsPred)
}
obsPred<- Model_Evaluate_elastic_v2('mild_uni', 'adv_outcome')
optAlpha <- model_result$alpha[1]
optLamb <- model_result$optimum_lambda[1]

#get the colums for graph for 10 folds separately
cohort <- 'ga_all'
outcome <- 'wet_outcome'
seed <- 1234
tarfolds <- c(2,3,4,5,10)

forGraph_10folds <- function(cohort, outcome, seed, tarfolds, optAlpha, optLamb){
  data_file<- paste('ModelData_', cohort, '_by_', outcome, '_v2.csv', sep='')
  raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
  names(raw_data)<- tolower(names(raw_data))
  
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
  alpha <- optAlpha
  lambda <- optLamb
  foldid<- nrow(training_data_lasso)
  set.seed(seed)
  foldid[training_data_lasso$response==1]<-sample(rep(1:k.folds, length=length(which(training_data_lasso$response==1))))
  set.seed(seed)
  foldid[training_data_lasso$response==0]<- sample(rep(1:k.folds, length=length(which(training_data_lasso$response==0))))
  table(training_data_lasso$response , foldid) # QC
  tarfolds <- tarfolds
  i <- 2
  for(i in tarfolds){
    cv_training_data_lasso<- training_data_lasso[foldid!=i,]                                            # select 9 folds as training data
    cv_training_response_pool[[i]]<- cv_training_data_lasso$response
    cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
    cv_test_data_lasso<- training_data_lasso[foldid==i,]                                                # select 1 fold as test data
    cv_test_response_pool[[i]] <- cv_test_data_lasso$response
    cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1] #
    
    fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                       lambda=lambda, family="binomial", alpha=alpha, standardize=F)
    test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
    test_pred_pool[[i]] <- test_pred
    test_obs <- cv_test_data_lasso$response
    obsPred_test <- data.frame(obs=test_obs, pred=test_pred)
    write.xlsx(obsPred_test, file=paste('forGraph_10folds_s_', cohort, '_', outcome, '.xlsx', sep=''), sheetName=paste('fold_', i, sep=''), row.names=F, append=T, showNA=T)
    
  }
  
}
forGraph_10folds('ga_all', 'wet_outcome', 1234, c(2,3,4,5,10), optAlpha, optLamb)

#write.csv(training_pred, file=paste(out_path, '\\forGraph_', cohort, '_', 'outcome', '.csv', sep=''))
obsPredInc <- training_pred[rev(order(training_pred[, 2])), ]
A <- obsPredInc[, 2]



options(digits=7)

data_path<- 'C:\\work\\jzhao\\working materials\\Jassen Project\\01. Raw_data\\ForModel'
out_path<-'C:\\work\\jzhao\\working materials\\Jassen Project\\03.Output\\sendout_Apr16'
setwd(out_path)
#names(raw_data)<- tolower(names(raw_data))
covar_definition <- read.table(paste(data_path, 'covar_definition_ning.csv', sep='\\'), sep=',', header=T)
covar_definition[, 2] <- tolower(covar_definition[, 2])
x <- gsub('^\\s+|\\s+$', '', covar_definition[, 2], perl=T,)

cohort <- 'mild_uni'
outcome <- 'adv_outcome'
Model_Evaluate_elastic_v2 <- function(cohort, outcome){
  
  data_file<- paste('ModelData_', cohort, '_by_', outcome, '_v2.csv', sep='')
  raw_data <- read.table(paste(data_path, data_file, sep='\\'), sep=',', head=T)
  names(raw_data)<- tolower(names(raw_data))
  
  dim(raw_data)
  na_check1 <- apply(apply(raw_data, 2, is.na), 2, sum)
  raw_data_noMiss <- na.omit(raw_data)
  na_check <- apply(apply(raw_data_noMiss, 2, is.na), 2, sum)
  n.pt <- dim(raw_data)[1]
  n.pt.outcome1 <- length(raw_data$response[raw_data$response==1]) 
  n.pt.noMiss <- dim(raw_data_noMiss)[1]
  n.pt.outcome1.noMiss <- length(raw_data_noMiss$response[raw_data_noMiss$response==1])
  training_data_lasso<-raw_data_noMiss
  training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
  
  
  #run elastic
  alpha_seq <- 0.96
  model_results <- numeric()
  #alpha <- alpha_seq[1]
  #alpha <- 0.04
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
    obsPred <- data.frame(obs=training_data_lasso$response, pred=training_obs)
    
  }#end of alpha
  
  #write.xlsx(obsPred, file=paste('forGraph.xlsx', sep=''), sheetName=paste(cohort, '_by_', outcome, sep=''), row.names=F, append=T, showNA=T)
  
  return(obsPred)
}

obsPred <- Model_Evaluate_elastic_v2('ga_all', 'wet_outcome')


#readin obsPred data for model 2, mild_uni by adv_outcome
dt <- read.table('forGraph_mild_uni_outcome.csv', sep=',', head=T)
dtOrder <- dt[order(dt[, 3]),c(2, 3)]
obsPred <- dtOrder
n.buck=2
posDistributeBucket <- function(obsPred, model, n.buck){
  obsPred <- obsPred
  pred <- obsPred[,2]
  predCut <- cut2(pred, g=n.buck)
  
  num_pos <- aggregate(obsPred[, 1], by=list(predCut), sum)[, 2]
  num_buck <- table(predCut)
  perc_pos <- num_pos/num_buck
  result <- data.frame(Bucket_number=seq(1, n.buck), Actual_number_of_positive_outcomes=num_pos, Perc_with_positive_outcomes=perc_pos)
  result[, 3]<- NULL
  write.xlsx(result, file=paste('PostiveDistributeForBucketModel', model,'_r3.xlsx', sep=''), sheetName=paste('bucket_', n.buck, sep=''), row.names=F, append=T, showNA=T)
  
}
posDistributeBucket(obsPred, 2, 2)
posDistributeBucket(obsPred, 2, 3)
posDistributeBucket(obsPred, 2, 4)
posDistributeBucket(obsPred, 2, 5)
posDistributeBucket(obsPred, 2, 10)
pred <- obsPred[,2]
predCut <- cut2(pred, g=n.buck)

num_pos <- aggregate(obsPred[, 1], by=list(predCut), sum)[, 2]
num_buck <- table(predCut)
perc_pos <- num_pos/num_buck

data_path <- 'C:\\Users\\jzhao\\Documents\\MyJabberFiles\\hjin@ucs.imshealth.com\\bucket LR\\bucket LR'
setwd(data_path)
n.buck <- 2
posDistributeBucket <- function(n.buck){
  num_pos_all <- numeric()
  perc_pos_all <- numeric()
  model <- 1
  for (model in 1:100){
    obsPred <- read.table(paste(data_path, '\\output_', model, '.csv', sep=''), header=T, sep=',')
    obsPred[, 1]<- NULL
    pred <- obsPred[,2]
    predCut <- cut2(pred, g=n.buck)
    
    num_pos <- aggregate(obsPred[, 1], by=list(predCut), sum)[, 2]
    num_buck <- table(predCut)
    perc_pos <- num_pos/num_buck
    num_pos_all <- rbind(num_pos_all, num_pos)
    perc_pos_all <- rbind(perc_pos_all, perc_pos)
  }
  num_pos_avg <- apply(num_pos_all, 2, mean)
  perc_pos_avg <- apply(perc_pos_all, 2, mean)
  result <- data.frame(Bucket_number=seq(1, n.buck), Actual_number_of_positive_outcomes=num_pos_avg, Perc_with_positive_outcomes=perc_pos_avg)
  #result[, 3]<- NULL
  write.xlsx(result, file=paste('PostiveDistributeForBucketModel.xlsx', sep=''), sheetName=paste('bucket_', n.buck, sep=''), row.names=F, append=T, showNA=T)
  
}
posDistributeBucket(2)
posDistributeBucket(3)
posDistributeBucket(4)
posDistributeBucket(5)
posDistributeBucket(10)


