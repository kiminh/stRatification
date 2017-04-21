
#one iteration
fullsample_stratification_step <- function(r, data, Y, treatment, Xvar, adjusted, ntilen){

  bst_data <- data[sample(NROW(data), NROW(data), replace = T), ]

  fs_result_bst <- fullsample_stratification(bst_data,
                                             Y = Y,
                                             treatment = treatment,
                                             Xvar = Xvar,
                                             adjusted = adjusted,
                                             ntilen = ntilen) %>%
    mutate(r = r)

  return(fs_result_bst)
}



#bootstrap for R times
fullsample_stratification_bst <- function(R, data, Y, treatment, Xvar, adjusted, ntilen, parallel){

  if(parallel == T){
    pforeach(r = 1:R, .combine = rbind)({

      fullsample_stratification_step(r,
                                     data,
                                     Y,
                                     treatment,
                                     Xvar,
                                     adjusted,
                                     ntilen)

    })

  }else if(parallel == F){

    lapply(X = 1:R,
           fullsample_stratification_step,
           data = data,
           Y = Y,
           treatment = treatment,
           Xvar = Xvar,
           adjusted = adjusted,
           ntilen = ntilen) %>%
      bind_rows()
  }

}
