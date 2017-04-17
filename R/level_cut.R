level_cut <- function(dataset, prediction_set, factor_col){

  #count number of variable that type is either factor or character
  num_factor_var <- length(factor_col)

  #extract factor level from the dataset which will be used in regression
  factor_list <- prediction_set[ ,factor_col] %>% distinct()

  #prepare result matrix
  check_table <- matrix(nrow = NROW(dataset),ncol = length(factor_col))

  #check the level of factor variable in prediction dataset then put TRUE if the level exists in estimation dataset
  for(j in 1:num_factor_var){
    check_list <- factor_list[,j] %>% unlist()
    check_flag <- sapply(dataset[,factor_col[j]], "%in%", check_list)[,1]
    check_table[,j] <- check_flag
  }

  #delete rows if some factor variable in the row have contain non exsising levels in the estimation dataset
  return(dataset[apply(check_table, 1, sum) == num_factor_var, ])
}
