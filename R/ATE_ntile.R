ATE_ntile <- function(model, Y, treatment, X, adjusted, estimation_dataset, treatment_dataset, ntilen, i){

  #get the control group dataset with its prediction result
  pred_control <- data.frame(pred = predict(model, newdata = estimation_dataset),
                             estimation_dataset)

  #get the treatment group dataset with its prediction result
  pred_treatment <- data.frame(pred = predict(model, newdata = treatment_dataset),
                               treatment_dataset)

  #dummy regression
  estimate_result <- ATE_estimate(pred_control, pred_treatment, i, adjusted, Y, X, treatment, ntilen)


  #calculate the size of the dataset
  dataset_count <- rbind(pred_control , pred_treatment) %>%
    mutate(nt = paste("ntile", ntile(pred, n = ntilen), sep = "_")) %>%
    group_by_(.dots = c("nt", treatment)) %>%
    summarise(count = n())

  return(estimate_result)
}




