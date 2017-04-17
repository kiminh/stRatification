ATE_estimate <- function(pred_control, pred_treatment, i, adjusted, Y, X, treatment, ntilen){

  if(adjusted == T){

    ate_formula <- as.formula(paste(Y, "~", treatment))

    rbind(pred_control , pred_treatment) %>%
      mutate(nt = paste("ntile", ntile(pred, n = ntilen), sep = "_")) %>%
      group_by(nt) %>%
      do(mod = lm(data = ., formula = ate_formula)) %>%
      broom::tidy(mod) %>%
      filter(term == treatment) %>%
      select(nt, ATE = estimate, stder = std.error) %>%
      ungroup() %>%
      mutate(M = i)


  }else if(adjusted == F){

    ate_formula <- as.formula(paste(Y, "~", treatment, "+", paste(X, collapse = " + ")))

    rbind(pred_control , pred_treatment) %>%
      mutate(nt = paste("ntile", ntile(pred, n = ntilen), sep = "_")) %>%
      group_by(nt) %>%
      do(mod = lm(data = ., formula = ate_formula)) %>%
      broom::tidy(mod) %>%
      filter(term == treatment) %>%
      select(nt, ATE = estimate, stder = std.error) %>%
      ungroup() %>%
      mutate(M = i)

  }else{

    print("adjusted must be TRUE/FALSE")
    break()

  }


}
