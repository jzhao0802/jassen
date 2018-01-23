loop_alpha_1 <- function(alpha){
    initial_lambda<- glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=alpha, standardize=F)$lambda  # calculating the initial lambda 
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
    auc_diff <- training_auc-test_auc
    #get coef, odds ratio, p_value
    model_coef <- total_model$beta[, optimum_model]
    model_coef_keep <- model_coef[model_coef!=0]
    if(length(model_coef_keep)==0){
        temp <- data.frame(alpha, coef_nm=NA, model_coef_keep=NA, odds_ratio=NA, p_value=NA, training_auc, test_auc=test_auc, auc_diff)
        
    }else{
        odds_ratio <- exp(model_coef_keep)
        coef_nm <- names(model_coef_keep)
        #get p_value
        model_var <- c('response', coef_nm)
        re_fit <- glm(response~., data=training_data_lasso[, match(model_var, names(training_data_lasso))], family=binomial)
        p_value <- summary(re_fit)$coef[, 'Pr(>|z|)'][-1]
        p_value <- p_value[match(coef_nm, names(p_value))]
        #store coef, odds ratio, p_value
        temp <- data.frame(alpha, coef_nm, model_coef_keep, odds_ratio, p_value, training_auc, test_auc, auc_diff)
        
    }
    #model_results <- rbind(model_results, temp)
    return(temp)
}
