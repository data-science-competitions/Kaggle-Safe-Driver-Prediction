#' ---
#' title: "GBM with Different Objective Functions"
#' author: "Harel Lustiger"
#' ---
# <https://gist.github.com/mndrake/7105b93d71ace38dc42b>


#########
# Setup #
#########
nboot = 200 # number of bootstrap samples
seed_number = 1816
f <- formula(paste(target_var_name,"~. -id"))
# Skew rate
r_s = table(X_tr[,target_var_name])["0"]/table(X_tr[,target_var_name])["1"]


#######################
# Dataset Preparation #
#######################
dX_tr = xgb.DMatrix(data=as.matrix(X_tr[,-2:-1]),
                    label=as.numeric(as.character(X_tr[,2])))
dX_ev = xgb.DMatrix(data=as.matrix(X_ev[,-2:-1]),
                    label=as.numeric(as.character(X_ev[,2])))
dX_te = xgb.DMatrix(data=as.matrix(X_te[,-2:-1]),
                    label=as.numeric(as.character(X_te[,2])))
dX_va = xgb.DMatrix(data=as.matrix(X_va[,-2:-1]),
                    label=as.numeric(as.character(X_va[,2])))
watchlist <- list(train=dX_tr, test=dX_ev)


##########################
# Fit Models to the Data #
##########################
model <- xgb.train(data=dX_tr,
                   # Parameter for Tree Booster
                   max_depth=6,
                   eta=0.01,
                   subsample=1,
                   # Early Stopping to Avoid Overfitting
                   early_stopping_rounds=10,
                   watchlist=watchlist,
                   nrounds=1e3,
                   # Task Parameters
                   objective="binary:logistic",
                   eval_metric=c("error",paste0("error@",round(1/r_s,3)),"auc","map","rmse","mlogloss")[3],
                   # Information
                   verbose=1, print_every_n=10L,
                   seed=1137)


##############################
# Predict the Validation Set #
##############################
pred <- predict(model, dX_te)




# ########################
# # Fit Bootstrap Models #
# ########################
# # Start CPU cluster
# availableCores = detectCores()
# cl <- makeCluster(availableCores, type="PSOCK", outfile="")
# registerDoSNOW(cl)
# # Measure execution time
# start_time <- Sys.time()
# # Fit models
# pred_list <- foreach(i=1:nboot,
#                      .errorhandling='stop', .packages=c('foreach','ROCR'), 
#                      .inorder=FALSE) %dopar% {
#                          
#                          # 1. Create a bootstrap sample
#                          set.seed(seed_number+i)
#                          X_bs = X_tr[sample(nrow(X_tr), replace=TRUE),]
#                          
#                          # 2. Fit model to the data
#                          mdl <- glm(formula=f,
#                                     data=X_bs,
#                                     family=binomial())
#                          
#                          # 3. Predict the train set
#                          y_hat_bs = predict(mdl, newdata=X_bs, type="response")
#                          y_hat_bs = unlist(round(y_hat_bs, 5))
#                          stopifnot(!any(is.na(y_hat_bs)))
#                          
#                          # 4. Predict the test set
#                          y_hat_te = predict(mdl, newdata=X_te, type="response")
#                          y_hat_te = unlist(round(y_hat_te, 5))
#                          stopifnot(!any(is.na(y_hat_te)))
#                          
#                          # 5. Find precision and recall best thresholds (using the train set)
#                          pred = prediction(y_hat_bs, X_bs[,target_var_name])
#                          perf = performance(pred, "f") # Precision-recall F measure
#                          F1_cutoff = unlist(perf@x.values)[which.max(unlist(perf@y.values))]
#                          list(y_hat_te=y_hat_te,
#                               F1_cutoff=unname(F1_cutoff))
#                      } # end foreach
# # Stop CPU cluster
# end_time <- Sys.time()
# stopCluster(cl)
# 
# 
# ######################
# # Results Processing #
# ######################
# ## 1. Test set predictons
# Y_hat_te = data.frame(matrix(as.numeric(NA),nrow(X_te),nboot))
# colnames(Y_hat_te) = paste0("mdl_",1:nboot)
# for(i in 1:length(pred_list))
#     Y_hat_te[,i] = unlist(pred_list[[i]]["y_hat_te"])
# 
# ## 2. Recall and Precision cutoffs
# Cutoffs = data.frame()
# for(i in 1:length(pred_list))
#     Cutoffs["F1",i] = pred_list[[i]]["F1_cutoff"]
# colnames(Cutoffs) = paste0("mdl_",1:nboot)
# 
# 
# ######################
# # Export the results #
# ######################
# stopifnot(!any(is.na(Y_hat_te)),!any(is.na(Cutoffs)))
# file_name = file.path(getwd(),"pred",
#                       paste0("(","boosted_glm",")","(","nboot=",nboot,")"))
# write_csv(cbind(mdl_0=as.numeric(X_te[,target_var_name])-1,Y_hat_te), 
#           gzfile(paste0(file_name,"(y_test)",".csv.gz")))
# write.csv(Cutoffs, 
#           paste0(file_name,"(cutoffs)",".csv"), row.names=TRUE)
# 
# 
# print(difftime(end_time,start_time))