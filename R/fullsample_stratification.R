fullsample_stratification <- function(data, Y, treatment, X, adjusted, ntilen){

  #fail fast
  stopifnot(is.data.frame(data) || is_null(data))
  stopifnot(is.character(Y) || is_null(Y))
  stopifnot(is.character(X) || is_null(X))
  stopifnot(is.character(treatment) || is_null(treatment))
  stopifnot(is.logical(adjusted) || is_null(adjusted))
  stopifnot(is.numeric(ntilen) || is_null(ntilen))

  ##separate dataset into control group and treatment group
  control_set <- data %>% filter_(paste(treatment, "==", 0))
  treatment_set <- data %>% filter_(paste(treatment, "==", 1))

  #get data type of each column
  factor_col <- which(sapply(data, class) %in% c("character", "factor"))

  treatment_set_cut <- level_cut(dataset = treatment_set, prediction_set = control_set, factor_col = factor_col)

  # take a regression in the non-treated group called prediction_set
  ntile_formula <- as.formula(paste(Y, "~", paste(X, collapse = " + ")))
  reg <- control_set %>%
    lm(data = ., formula = ntile_formula)

  #estimate ATE with dummy regression
  out <- ATE_ntile(model = reg,
                   Y = Y,
                   treatment = treatment,
                   X = X,
                   adjusted = adjusted,
                   estimation_dataset = control_set,
                   treatment_dataset = treatment_set_cut,
                   ntilen = ntilen,
                   i = 1)
  return(out)

}
