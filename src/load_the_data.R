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


rm(path.codebook, path.train, path.test, 
   var_name, var_names, var_type,
   codebook, all_data)