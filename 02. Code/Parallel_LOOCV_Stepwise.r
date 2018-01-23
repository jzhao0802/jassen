library(snowfall)
sfInit( parallel=TRUE, cpus=5 )
b <- c( 3.4, 5.7, 10.8, 8, 7 )
## Export a and b in their current state to all slaves.
sfExport('b')
parWrapper <- function( datastep, add1, add2 ) {
    cat( 'Data:', datastep, 'ADD1:', add1, 'ADD2:', add2, '\n' )
    ## Only possible as ''b'' is exported!
    cat( 'b:', b[datastep] )
    ## Do something
    return( datastep )
}
## Calls parWrapper with each value of a and additional
## arguments 2 and 3.
result <- sfLapply( 1:5, parWrapper, 2, 3 )
sfStop()


sfInit( parallel=TRUE, cpus=2 )
calcPar <- function( x ) {
    cat('\nrun: ', x, '\n')
    x1 <- matrix( 0, x, x )
    x2 <- matrix( 0, x, x )
    for( var in 1:nrow( x1 ) ) x1[var,] = runif( ncol( x1 ) )
    for( var in 1:nrow( x2 ) ) x2[var,] = runif( ncol( x1 ) )
    b <- sum( diag( ( x1 %*% x2 ) %*% x1 ) )
    return( b )
}
result <- sfClusterApplyLB( 50:100, calcPar )
sfStop()

#################################################################
#               parallel running on LOOCV_stepwise              #
#################################################################
rm(list=ls())

library(glmnet)
library(xlsx)
library(snowfall)
sfLibrary("snowfall", character.only = TRUE)
sfLibrary("glmnet", character.only = TRUE)
sfLibrary("xlsx", character.only = TRUE)
options(digits=7)

data_path<- 'D:\\working materials\\Jassen Project\\01. Raw_data\\ForModel'
out_path<-'D:\\working materials\\Jassen Project\\03.Output\\Feb27\\sendout_Mar19'
setwd(out_path)
covar_definition <- read.table(paste(data_path, 'covar_definition_ning.csv', sep='\\'), sep=',', header=T)
covar_definition[, 2] <- tolower(covar_definition[, 2])
x <- gsub('^ +| +$', '', covar_definition[, 2], perl=T,)

cohort<- 'ga_all'
outcome <- 'wet_outcome'
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
training_data <- raw_data_noMiss
coef_list <- list()
#test_pred <- matrix(nr=nrow(training_data), nc=1)


sfInit( parallel=TRUE, cpus=8 )
sfExport('training_data', 'coef_list')
#test_pred <- matrix(nr=nrow(traing_data), cl=1)
LOOCV <- function(i){
    timeStart <- proc.time()
    cv_training_data<- training_data[-i, ]
    cv_test_data<- training_data[i,]
    
    # Standard logistic regression
    cv_fit_stepwise<- glm(response~., data=cv_training_data, family=binomial) 
    cv_step_wise<-step(cv_fit_stepwise,direction="both")
    coef_stepwise<- data.frame(coefficient=coef(cv_step_wise) , odds_ratio=exp(coef(cv_step_wise)))
    coef_list[[i]] <- coef_stepwise
    test_pred<- predict(cv_step_wise, cv_test_data, type="response")
    cat('LOOCV ',i, 'running time: ', proc.time()-timeStart, '\n')
    
    return(test_pred)
}

result <- sfClusterApplyLB( 1:nrow(training_data), LOOCV )
sfStop()
pred_test_vct <- unlist(result)
auc_test_esti <- auc(training_data$response, pred_test_vct)


#################################################################
#               parallel running on LOOCV_elastic  (remain to recorrect!!!)             #
#################################################################

rm(list=ls())

library(glmnet)
library(xlsx)
library(snowfall)
sfLibrary("snowfall", character.only = TRUE)
sfLibrary("glmnet", character.only = TRUE)
sfLibrary("xlsx", character.only = TRUE)
options(digits=7)

data_path<- 'D:\\working materials\\Jassen Project\\01. Raw_data\\ForModel'
out_path<-'D:\\working materials\\Jassen Project\\03.Output\\Feb27\\sendout_Mar19'
setwd(out_path)
covar_definition <- read.table(paste(data_path, 'covar_definition_ning.csv', sep='\\'), sep=',', header=T)
covar_definition[, 2] <- tolower(covar_definition[, 2])
x <- gsub('^ +| +$', '', covar_definition[, 2], perl=T,)

cohort<- 'mild_bil'
outcome <- 'ga_outcome'
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
training_data_lasso <- raw_data_noMiss
training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term


LOOCV_elastic <- function(i){
    cv_training_data_lasso<- training_data_lasso[-i,]                                            # select data except ith row as training data
    cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
    cv_test_data_lasso<- training_data_lasso[i,]                                                # select the ith row as test data
    cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1] #
    cv_test_matrix <- t(cv_test_matrix) #transpose vector as matrix whose dim is [1, 26]
    
    fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                       lambda=lambda_seq, family="binomial", alpha=alpha, standardize=F)
    test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
    #dim(test_pred) 
    #test_pred_list[i] <- test_pred
    #test_pred_avg<- apply(test_pred, 2, function(x){auc(cv_test_data_lasso$response , x)})
    #test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
    #cv_auc[i,] <- test_pred_avg
    return(test_pred)
}
result1 <- sfClusterApplyLB( 1:nrow(training_data_lasso), LOOCV_elastic )
test_pred_vct <- unlist(result1)
auc_esti <- auc(training_data_lasso$response, test_pred_vct)


alpha_seq <- seq(0, 1, 0.01)
sfInit( parallel=TRUE, cpus=8 )
sfExport('training_matrix', 'training_data_lasso', 'alpha_seq')
loop_alpha <- function(alpha){
    sfLibrary("snowfall", character.only = TRUE)
    sfLibrary("glmnet", character.only = TRUE)
    sfLibrary("xlsx", character.only = TRUE)
    
    timeStart <- proc.time()
    initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=alpha, standardize=F)$lambda  # calculating the initial lambda 
    length(initial_lambda) #100  decreasing 
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500)) # get a length=500 descending sequence from initial lambda to 0
    test_pred_list <- matrix(nr=nrow(training_data_lasso), nc=length(lambda_seq))
    LOOCV_elastic(alpha)
    result1 <- sfClusterApplyLB( 1:nrow(training_data_lasso), LOOCV_elastic )
    test_pred_vct <- unlist(result1)
    auc_esti <- auc(training_data_lasso$response, test_pred_vct)
    
    cat('LOOCV elastic running time:alpha=', alpha, ' :', proc.time-timeStart, '\n')
    return(auc_esti)
}
result2 <- sfClusterApplyLB( alpha_seq, loop_alpha)
sfStop()

#################################################################
#               parallel running on LOOCV_elastic  V2             #
#################################################################
rm(list=ls())

sfSource('D:\\working materials\\Jassen Project\\02. Code\\loop_elastic_f.r')
library(glmnet)
library(xlsx)
library(snowfall)
sfLibrary("snowfall", character.only = TRUE)
sfLibrary("glmnet", character.only = TRUE)
sfLibrary("xlsx", character.only = TRUE)

options(digits=7)

alpha_seq <- seq(0, 1, 0.01)
sfInit( parallel=TRUE, cpus=4 )
sfExport('training_matrix', 'training_data_lasso', 'alpha_seq')
sfClusterEval(library(glmnet))  #pay attention to the place

timeStart <- proc.time()
result2 <- sfClusterApplyLB( alpha_seq, loop_alpha_1)
sfStop()

timeEnd <- proc.time()
timeUsed <- timeEnd-timeStart





