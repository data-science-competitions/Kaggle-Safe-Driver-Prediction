#' ---
#' title: "Load the Data"
#' author: "Harel Lustiger"
#' ---
target_var_name = "target" 
path.codebook = file.path(getwd(),"data","codebook.csv")
path.train = file.path(getwd(),"data","train.zip")
path.test = file.path(getwd(),"data","test.zip")

codebook = as.data.frame(read_csv(path.codebook))
train = as.data.frame(read_csv(path.train, na=c("","NA",-1)))
test = as.data.frame(read_csv(path.test, na=c("","NA",-1)))
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


##############################################
# Detect Missing Values in Numeric Variables #
##############################################
#' In this competition, -1 represents missing values. 
#' Therefore, we change -1 to NA.
for(var_name in var_names){
    
    var_type = subset(codebook, name %in% var_name, select=type)
    
    if(var_type=="numeric")
        all_data[all_data[[var_name]] %in% -1,var_name] <- NA
    
}# end changing -1 to NA


####################################
# Revert to the Original Stracture #
####################################
train = as.data.frame(subset(all_data, set %in% 1, select=-set))
test = as.data.frame(subset(all_data, set %in% 2, select=-set))


rm(path.codebook, path.train, path.test, 
   var_name, var_names, var_type,
   codebook, all_data)