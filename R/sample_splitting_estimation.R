sample_splitting_estimation <- function(data, Y, treatment, Xvar, M, ntilen, adjusted){

  #fail fast
  stopifnot(is.data.frame(data) || is_null(data))
  stopifnot(is.character(Y) || is_null(Y))
  stopifnot(is.character(Xvar))
  stopifnot(is.character(treatment) || is_null(treatment))
  stopifnot(is.logical(adjusted) || is_null(adjusted))
  stopifnot(is.numeric(ntilen) || is_null(ntilen))
  stopifnot(is.numeric(M) || is_null(M))

  ## separate dataset into control group and treatment group
  control_set <- data %>% filter_(paste(treatment, "==", 0))
  treatment_set <- data %>% filter_(paste(treatment, "==", 1))

  # get data type of each column
  factor_col <- which(sapply(data, class) %in% c("character", "factor"))


  result <- lapply(1:M, sample_splitting_step,
                   Y = Y,
                   Xvar = Xvar,
                   treatment = treatment,
                   adjusted = adjusted,
                   control_set = control_set,
                   factor_col = factor_col,
                   treatment_set = treatment_set,
                   ntilen = ntilen) %>%
    bind_rows()

  return(result)

}









sample_splitting_step <- function(x, Y, Xvar, treatment, adjusted, factor_col,
                                  ntilen, control_set, treatment_set){
  # split non-treated data into two group at random
  sample_flag_control <- sample(NROW(control_set), NROW(control_set)/2, replace = F)
  prediction_set <- control_set[sample_flag_control, ]
  estimation_set <- control_set[-sample_flag_control, ]

  estimation_set_cut <- level_cut(dataset = estimation_set, prediction_set = prediction_set, factor_col = factor_col)
  treatment_set_cut <- level_cut(dataset = treatment_set, prediction_set = prediction_set, factor_col = factor_col)

  # take a regression in the non-treated group called prediction_set
  ntile_formula <- as.formula(paste(Y, "~", paste(Xvar, collapse = " + ")))
  reg <- prediction_set %>%
    lm(data = ., formula = ntile_formula)

  # estimate ATE in each group
  out <- ATE_ntile(model = reg,
                   Y = Y,
                   Xvar = Xvar,
                   treatment = treatment,
                   adjusted = adjusted,
                   estimation_dataset = estimation_set_cut,
                   treatment_dataset = treatment_set_cut,
                   ntilen = ntilen,
                   i = x)
  return(out)
}




