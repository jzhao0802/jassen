# Codes for Calculating performance measure, i.e.TPR, FPR, TNR, FNR, Sensitivity, Precision(PPV), rate_accuracy
# Developed by Hui, Jin
# Feb,9,2015

# Take a LR model as an example to show how to calculate performance measure
options(digits = 7)
library(glmnet)

#1. set up working folder and read in model data
data_path <- 'D:\\Project_2014\\BRACE Relapse project\\004 Output'
data_file <- 'final_model_data_comb.csv';
raw_data <- read.table(paste(data_path,'\\',data_file,sep=''), header=T, sep=',')

all_variables_list <- names(raw_data)
treatment_variables <- c('idx_IFN','idx_GA','pre_idx_IFN','pre_idx_GA','pre_idx_NOMS')

switching_flag <- c('flag_NT_IFN','flag_NT_GA','flag_IFN_GA','flag_IFN_IFN', 'flag_GA_IFN')

reference_variables <- c('idx_GA','pre_idx_NOMS','der_sexF','pat_regionMW','idx_paytypeSMRU','idx_prodtypeP','idx_spec3',
                       'pre_non_ms_total_allowed4','pre_ms_total_allowed4','pre_ms_pharmacy_allowed2','pre_non_ms_pharmacy_allowed4',
                       'pre_non_ms_medical_allowed4','pre_ms_medical_allowed4','num_pre_meds4','num_pre_op_dx4','age4','num_pre_mri_any4',
                       'pchrlson3','num_pre_cort_oral3','num_pre_cort_iv3','num_pre_relapse_1yr1','')

variable_list_v1 <- setdiff(all_variables_list,c(treatment_variables,switching_flag,reference_variables))

# 2. Divided whole data into 2 parts, 75% traning sample and 25% test sample
source('D:\\Project_2014\\BRACE Relapse project\\003 R Code\\Sampling_ims.r')

model_data <- raw_data[,variable_list_v1]
datasets <- sampling_ims(model_data,0.75,'response',setseed=T,10)

training_data <- datasets[[1]]
test_data <- datasets[[2]]

# 3. Run LR on training sample 
fit_std <- glm(response~., data=training_data, family=binomial)

# Get the predicted value on training sample and test sample
training_obs <- predict(fit_std, training_data, type="response")
test_obs <- predict(fit_std, test_data, type="response")

# 4. Calculate Performance Measures                                                     
# 1) The threshold, could change
pred_thresh <- mean(training_data$response)

# 2) Sort the predicted value on test sample, in case top PPV is required. (Optional)
pred_data <- sort(test_obs,T)

# 3) rename actual_data, could be test sample or training sample, depends on which measures you want to compute 
actual_data <- test_data

# 4) Compute performance measures
num_actual_positive <- sum(actual_data$response)              # Number of actual positive cases
num_actual_negative <- sum(1 - actual_data$response)          # Number of actual negative cases
num_pred_positive <- length(which(pred_data >= pred_thresh))  # Number of positive predictions
num_pred_negative <- length(which(pred_data < pred_thresh))   # Number of negative predictions

# positive cases in predicted value, corresponding rows in actual data
pred_pos_in_actual <-actual_data[rownames(actual_data) %in% names(which(pred_data>=pred_thresh)),]

# Negative cases in predicted value, corresponding rows in actual data
pred_neg_in_actual <-actual_data[rownames(actual_data) %in% names(which(pred_data<pred_thresh)),]

true_post_rate <- sum(pred_pos_in_actual$response) / num_actual_positive      # True positive rate 
false_post_rate <- sum(1 - pred_pos_in_actual$response) / num_actual_negative # False positive rate 
true_neg_rate <- sum(1 - pred_neg_in_actual$response) / num_actual_negative   # True negative rate 
false_neg_rate <- sum(pred_neg_in_actual$response) / num_actual_positive      # False negative rate

rate_post <- num_pred_positive/nrow(actual_data)                              # Proportion of cases predicted as positive
sensitivity <- sum(pred_pos_in_actual$response) / num_actual_positive         # Recall / sensitivity
precision <- sum(pred_pos_in_actual$response) / num_pred_positive             # Precision / PPV
rate_accuracy <- (sum(pred_pos_in_actual$response) +sum(1 - pred_neg_in_actual$response)) / nrow(actual_data) # Classification accuracy (proportion of cases predicted correctly)


# Ends










