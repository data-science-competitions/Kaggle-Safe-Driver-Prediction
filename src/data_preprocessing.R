################################################################################
#                              Data Preprocessing                              #
################################################################################
all_data = bind_rows(train, test, .id="set")

##################
# Developer Zone #
##################
# 1. Discard all non numeric variables
all_data = all_data[,unlist(sapply(all_data, function(v) class(v)[1])) %in% "numeric" |
                        colnames(all_data) %in% c("set",target_var_name)]
# 2. Remove rows with missing values
all_data = all_data[complete.cases(all_data[,!colnames(all_data) %in% target_var_name]),]



# 1. Imputing missing values
# all_data = all_data[complete.cases(all_data[,!colnames(all_data) %in% target_var_name]),]


# # 2. Collapsing rare levels into 'other'
# col_names = setdiff(colnames(all_data),c("set",target_var_name))
# for(col_name in col_names){
#     
#     if(is.factor(all_data[,col_name])){
#         # Lump values that appear less than 10% of the time
#         all_data[,col_name] = fct_lump(all_data[,col_name], prop=0.1)
#         
#     }# end if factor
# }# end for colnames
# 
# 
# ####################################
# # Revert to the Original Stracture #
# ####################################
train = as.data.frame(subset(all_data, set %in% 1, select=-set))
test = as.data.frame(subset(all_data, set %in% 2, select=-set))
# 
# 
rm(col_name, col_names, all_data)