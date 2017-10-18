################################################################################
#                                Load the Data                                 #
################################################################################
target_var_name = "target" 
path.codebook = file.path(getwd(),"data","codebook.csv")
path.train = file.path(getwd(),"data","train.zip")
path.test = file.path(getwd(),"data","test.zip")

codebook =  as.data.frame(read_csv(path.codebook))
train = as.data.frame(read_csv(path.train, na=c("","NA",-1)))
test =  as.data.frame(read_csv(path.test, na=c("","NA",-1)))
all_data = bind_rows(train, test, .id="set")


########################
# Change Columns Types #
########################
var_names = intersect(colnames(all_data), codebook[["name"]])
for(var_name in var_names){
    
    var_type = subset(codebook, name %in% var_name, select=type)
    if(var_type=="numeric")
        all_data[,var_name] <- as.numeric(all_data[,var_name])
    else if(var_type=="binary" || var_type=="categorical")
        all_data[,var_name] <- as.factor(all_data[,var_name])
    else if(var_type=="ordinal")
        all_data[,var_name] <- as.ordered(all_data[,var_name])
    
}# end changing cols types


####################################
# Revert to the Original Stracture #
####################################
train = as.data.frame(subset(all_data, set %in% 1, select=-set))
test = as.data.frame(subset(all_data, set %in% 2, select=-set))


##################
# Split the Data #
##################
frac_tr = 0.5
frac_te = 0.3
#' all_data = train U test
#' train = X_tr U X_te U X_va
set.seed(GLOBALS[["local_evaluation_seed"]])
shuffled_index = sample(nrow(train))
train_index = sample(shuffled_index, round(length(shuffled_index)*frac_tr))
shuffled_index = setdiff(shuffled_index,train_index)
test_index = sample(shuffled_index, round(length(shuffled_index)*frac_te))
validation_index = setdiff(shuffled_index,test_index)

X_tr = train[train_index,]
X_te = train[test_index,]
X_va = train[validation_index,]


rm(path.codebook, path.train, path.test, 
   frac_te, frac_tr, var_name, var_names, var_type,
   shuffled_index, train_index, test_index, validation_index,
   codebook, all_data)