################################################################################
#                                Split the Data                                #
################################################################################
set.seed(GLOBALS[["local_evaluation_seed"]])

frac_tr = 0.1
frac_te = 0.1

#' all_data = train U test
#' train = X_tr U X_te U X_va

shuffled_index = sample(nrow(train))
train_index = sample(shuffled_index, round(length(shuffled_index)*frac_tr))
shuffled_index = setdiff(shuffled_index,train_index)
test_index = sample(shuffled_index, round(length(shuffled_index)*frac_te))
validation_index = setdiff(shuffled_index,test_index)

X_tr = train[train_index,]
X_te = train[test_index,]
X_va = train[validation_index,]

rm(frac_tr, frac_te,
   shuffled_index, train_index, test_index, validation_index)