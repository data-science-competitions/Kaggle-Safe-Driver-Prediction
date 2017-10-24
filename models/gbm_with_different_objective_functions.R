#' ---
#' title: "GBM with Different Objective Functions"
#' author: "Harel Lustiger"
#' ---
# <https://gist.github.com/mndrake/7105b93d71ace38dc42b>


############
# A. Setup #
############
source("src/helper_functions.R")
nboot = 4 # number of bootstrap samples
f <- formula(paste(target_var_name,"~. -id"))
eval_metrics=c("error","error@0.096","auc","map","rmse")
objective="binary:logistic"
tasks = expand.grid(eval_metric=eval_metrics, nboot=1:nboot, 
                    stringsAsFactors=FALSE)
seed_number = 1252


##########################
# B. Dataset Preparation #
##########################
dX_ev = xgb.DMatrix(data=as.matrix(X_ev[,-2:-1]),
                    label=as.numeric(as.character(X_ev[,2])))
dX_te = xgb.DMatrix(data=as.matrix(X_te[,-2:-1]),
                    label=as.numeric(as.character(X_te[,2])))


###########################
# C. Fit Bootstrap Models #
###########################
# Start CPU cluster
availableCores = detectCores()
cl <- makeCluster(availableCores, type="PSOCK", outfile="")
registerDoSNOW(cl)
# Measure execution time
start_time <- Sys.time()
# Fit models
pred_list <- foreach(i=1:nrow(tasks),
                     .errorhandling='stop', .packages=c('foreach','ROCR','xgboost'),
                     .inorder=FALSE) %do% {
                         
                         # 0. Extract task params
                         eval_metric = tasks[i,"eval_metric"]
                         k = tasks[i,"nboot"]
                         cat("\nModel ",i,"/",nrow(tasks),sep="")
                         
                         # 1. Create a bootstrap sample
                         set.seed(seed_number+k)
                         X_bs = X_tr[sample(nrow(X_tr), replace=TRUE),]
                         
                         # 2. Contruct xgb.DMatrix objects
                         dX_bs = xgb.DMatrix(data=as.matrix(X_bs[,-2:-1]),
                                             label=as.numeric(as.character(X_bs[,2])))
                         watchlist <- list(train=dX_bs, test=dX_ev)
                         
                         # 3. Fit a model to the bootstap set
                         model <- xgb.train(data=dX_bs,
                                            # Parameter for Tree Booster
                                            max_depth=6,
                                            eta=0.01,
                                            subsample=1,
                                            # Early Stopping to Avoid Overfitting
                                            early_stopping_rounds=10,
                                            watchlist=watchlist,
                                            nrounds=1e3,
                                            # Task Parameters
                                            objective=objective,
                                            eval_metric=eval_metric,
                                            # Information
                                            verbose=0, print_every_n=10L,
                                            seed=1257)
                         
                         # 4. Find the F1 threshold
                         y_hat_bs = predict(model, dX_bs)
                         pred = prediction(y_hat_bs, X_bs[,target_var_name])
                         perf = performance(pred, "f") # Precision-recall F measure
                         F1_cutoff = unlist(perf@x.values)[which.max(unlist(perf@y.values))]

                         # 5. Predict the test set
                         y_hat_te = predict(model, dX_te)
                         
                         # 6. Information
                         seconds_passed = difftime(Sys.time(),start_time,units="secs")
                         seconds_left = (nrow(tasks)-i) * seconds_passed / i
                         cat("\t ETA: ",round(seconds_left), " [sec]",sep="")
                         
                         # 7. Return results
                         list(k=k, y_hat=round(y_hat_te,5), F1_cutoff=unname(F1_cutoff))
                     }# end foreach loop
end_time <- Sys.time()
stopCluster(cl)


#########################
# D. Results Processing #
#########################
cor_plot = file.path(getwd(), "(results)(same_obj_func_diff_metric).csv")
bar_plot = file.path(getwd(), "(results)(diff_obj_func_diff_metric).csv")
scores = data.frame(matrix(as.numeric(NA),nboot,length(eval_metrics)+1))
colnames(scores) = c("gini",eval_metrics)

for(i in 1:nrow(tasks)){
    # Extract Data
    k = pred_list[[i]]$k
    eval_metric = tasks[i,"eval_metric"]
    cutoff = pred_list[[i]]$F1_cutoff
    
    # Use models with the same (arbitrary) eval metric
    if(eval_metric != eval_metrics[1]) next
    
    # Calculate performance measures
    y_hat = pred_list[[i]]$y_hat
    pred = ROCR::prediction(predictions=y_hat, labels=X_te[,2])
    
    ## GINI
    scores[k,"gini"] = SumModelGini(solution=y_hat, submission=X_te[,2])
    
    ## AUC
    if(any(eval_metrics %in% "auc")){
        perf = ROCR::performance(pred,"auc")
        scores[k,"auc"] = perf@y.values[[1]]
    }
    
    ## RMSE
    if(any(eval_metrics %in% "rmse")){
        perf = ROCR::performance(pred,"rmse")
        scores[k,"rmse"] = perf@y.values[[1]]
    }
}# end corr plot


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
print(difftime(end_time,start_time))