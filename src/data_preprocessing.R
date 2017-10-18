################################################################################
#                              Data Preprocessing                              #
################################################################################
all_data = bind_rows(train, test, .id="Set")

# Collapsing rare levels into 'other'
col_names = setdiff(colnames(all_data),c("set",target_var_name))
for(col_name in col_names){
    
    if(is.factor(all_data[,col_name])){
        # Lump values that appear less than 10% of the time
        all_data[,col_name] = fct_lump(all_data[,col_name], prop=0.1)
        
    }# end if factor
}# end for colnames


####################################
# Revert to the Original Stracture #
####################################
train = as.data.frame(subset(all_data, Set %in% 1, select=-Set))
test = as.data.frame(subset(all_data, Set %in% 2, select=-Set))


rm(col_name, col_names, all_data)