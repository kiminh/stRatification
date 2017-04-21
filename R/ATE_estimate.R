ATE_estimate <- function(pred_control, pred_treatment, i, adjusted, Y, Xvar, treatment, ntilen){

  #prepare function for estimate ATE within each group
  group_reg <- function(pred_control, pred_treatment, i, ate_formula, treatment, ntilen){
    rbind(pred_control , pred_treatment) %>%
      mutate(nt = paste("ntile", ntile(pred, n = ntilen), sep = "_")) %>%
      group_by(nt) %>%
      do(mod = lm(data = ., formula = ate_formula)) %>%
      broom::tidy(mod) %>%
      filter(term == treatment) %>%
      select(nt, ATE = estimate, stder = std.error) %>%
      ungroup() %>%
      mutate(M = i)
  }


  if(adjusted == F){ #unadjusted which is dummy regression without any control variable

    ate_formula <- as.formula(paste(Y, "~", treatment))
    group_reg(pred_control, pred_treatment, i, ate_formula, treatment, ntilen)

  }else if(adjusted == T){ #adjusted which is dummy regression with control variable(indicated as X)

    ate_formula <- as.formula(paste(Y, "~", treatment, "+", paste(Xvar, collapse = " + ")))
    group_reg(pred_control, pred_treatment, i, ate_formula, treatment, ntilen)

  }


}


