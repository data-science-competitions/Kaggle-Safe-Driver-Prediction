#' ---
#' title: "GBM with Different Objective Functions"
#' author: "Harel Lustiger"
#' ---
# <https://gist.github.com/mndrake/7105b93d71ace38dc42b>


############
# A. Setup #
############
source("src/helper_functions.R")
nboot = 200 # number of bootstrap samples
f <- formula(paste(target_var_name,"~. -id"))
eval_metrics=c("error","error@0.096","auc","map","rmse")
objective="binary:logistic"
tasks = expand.grid(eval_metric=eval_metrics, nboot=1:nboot, 
                    stringsAsFactors=FALSE)
seed_number = 2111


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
                                            early_stopping_rounds=1e3,
                                            watchlist=watchlist,
                                            nrounds=1e1,
                                            # Task Parameters
                                            objective=objective,
                                            eval_metric=eval_metric,
                                            # Information
                                            verbose=0, print_every_n=10L,
                                            seed=2145)
                         
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
cor_plot = file.path(getwd(),"pred","(results)(same_obj_func_diff_metric).csv")
bar_plot = file.path(getwd(),"pred","(results)(diff_obj_func_diff_metric).csv")
scores1 = scores2 = data.frame(matrix(as.numeric(NA),nboot,length(eval_metrics)+1))
colnames(scores1) = colnames(scores2) = c("gini",eval_metrics)
# Scores for the correletion plots
for(i in 1:nrow(tasks)){
    # Extract Data
    k = pred_list[[i]]$k
    eval_metric = tasks[i,"eval_metric"]
    cutoff = pred_list[[i]]$F1_cutoff
    
    # Use models with the same (arbitrary) eval metric
    if(eval_metric != "auc") next
    
    # Calculate performance measures
    y = as.numeric(as.character(X_te[,2]))
    y_hat = pred_list[[i]]$y_hat
    pred = ROCR::prediction(predictions=y_hat, 
                            labels=as.numeric(as.character(X_te[,2])))
    
    ## GINI
    scores1[k,"gini"] = SumModelGini(solution=y, submission=y_hat) / 
        SumModelGini(solution=y, submission=y)
    
    ## AUC
    if(any(eval_metrics %in% "auc")){
        perf = ROCR::performance(pred,"auc")
        scores1[k,"auc"] = perf@y.values[[1]]
    }
    
    ## RMSE
    if(any(eval_metrics %in% "rmse")){
        perf = ROCR::performance(pred,"rmse")
        scores1[k,"rmse"] = perf@y.values[[1]]
    }
    
    ## PRECISION
    if(any(eval_metrics %in% "map")){
        perf = ROCR::performance(pred,"prec")
        cutoff_index = which.min(abs(perf@x.values[[1]]-cutoff))
        scores1[k,"map"] = perf@y.values[[1]][cutoff_index]
    }
    
    ## ACCURECY
    if(any(eval_metrics %in% "error")){
        perf = ROCR::performance(pred,"acc")
        cutoff_index = which.min(abs(perf@x.values[[1]]-0.5))
        scores1[k,"error"] = perf@y.values[[1]][cutoff_index]
    }
    
    ## (skew-sensitive) ACCURECY
    if(any(eval_metrics %in% eval_metrics[grep("error@.",eval_metrics)])){
        eval_metric = eval_metrics[grep("error@.",eval_metrics)]
        perf = ROCR::performance(pred,"acc")
        cutoff_index = which.min(abs(perf@x.values[[1]]-cutoff))
        scores1[k,eval_metric] = perf@y.values[[1]][cutoff_index]
    }
    
}# end corr plot
scores1 = round(scores1,5)

# Scores for the boxplot
for(i in 1:nrow(tasks)){
    # Extract Data
    k = pred_list[[i]]$k
    eval_metric = tasks[i,"eval_metric"]
    cutoff = pred_list[[i]]$F1_cutoff
    
    # Calculate performance measures
    y = as.numeric(as.character(X_te[,2]))
    y_hat = pred_list[[i]]$y_hat
    pred = ROCR::prediction(predictions=y_hat,
                            labels=as.numeric(as.character(X_te[,2])))
    
    # Calculate performance measures
    ## Gini Benchmark
    set.seed(1201)
    scores2[k,"gini"] = SumModelGini(solution=y, submission=y_hat*0) / 
        SumModelGini(solution=y, submission=y)   
    ## The corresponding Gini coeff for the eval metric
    scores2[k,eval_metric] = SumModelGini(solution=y, submission=y_hat) / 
        SumModelGini(solution=y, submission=y)
    
} # end boxplot


######################
# Export the results #
######################
write_csv(scores1,cor_plot)
write_csv(scores2,bar_plot)


print(difftime(end_time,start_time))