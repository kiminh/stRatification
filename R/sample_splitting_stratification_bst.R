
#one iteration
sample_splitting_stratification_step <- function(r, data, Y, treatment, Xvar, M, adjusted, ntilen, parallel){

  bst_data <- data[sample(NROW(data), NROW(data), replace = T), ]

  ss_result_bst <- sample_splitting_estimation(bst_data,
                                               Y = Y,
                                               treatment = treatment,
                                               Xvar = Xvar,
                                               M = M,
                                               adjusted = adjusted,
                                               ntilen = ntilen) %>%
    mutate(r = r)

  return(ss_result_bst)
}



#bootstrap for R times
sample_splitting_stratification_bst <- function(R, data, Y, treatment, Xvar, M, adjusted, ntilen, parallel){

  if(parallel == T){

    pforeach(r = 1:R, .combine = rbind)({

      sample_splitting_stratification_step(r,
                                           data,
                                           Y,
                                           treatment,
                                           Xvar,
                                           M,
                                           adjusted,
                                           ntilen)

    })

  }else if(parallel == F){

    lapply(X = 1:R,
           sample_splitting_stratification_step,
           data = data,
           Y = Y,
           treatment = treatment,
           Xvar = Xvar,
           adjusted = adjusted,
           ntilen = ntilen,
           M = M) %>%
      bind_rows()

  }

}
