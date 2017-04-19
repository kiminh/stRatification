sample_splitting_estimation <- function(data, Y, treatment, X, M, ntilen, adjusted){

  #fail fast
  stopifnot(is.data.frame(data) || is_null(data))
  stopifnot(is.character(Y) || is_null(Y))
  stopifnot(is.character(X) || is_null(X))
  stopifnot(is.character(treatment) || is_null(treatment))
  stopifnot(is.logical(adjusted) || is_null(adjusted))
  stopifnot(is.numeric(ntilen) || is_null(ntilen))
  stopifnot(is.numeric(M) || is_null(M))

  ## separate dataset into control group and treatment group
  control_set <- data %>% filter_(paste(treatment, "==", 0))
  treatment_set <- data %>% filter_(paste(treatment, "==", 1))

  # get data type of each column
  factor_col <- which(sapply(data, class) %in% c("character", "factor"))

  ## set M which indicates how many time iterates the estimation
  M <- M
  result <- data.frame()

  for(i in 1:M){

    # split non-treated data into two group at random
    sample_flag_control <- sample(NROW(control_set), NROW(control_set)/2, replace = F)
    prediction_set <- control_set[sample_flag_control, ]
    estimation_set <- control_set[-sample_flag_control, ]

    estimation_set_cut <- level_cut(dataset = estimation_set, prediction_set = prediction_set, factor_col = factor_col)
    treatment_set_cut <- level_cut(dataset = treatment_set, prediction_set = prediction_set, factor_col = factor_col)

    # take a regression in the non-treated group called prediction_set
    ntile_formula <- as.formula(paste(Y, "~", paste(X, collapse = " + ")))
    reg <- prediction_set %>%
      lm(data = ., formula = ntile_formula)

    # estimate ATE in each group
    out <- ATE_ntile(model = reg,
                     Y = Y,
                     X = X,
                     treatment = treatment,
                     adjusted = adjusted,
                     estimation_dataset = estimation_set_cut,
                     treatment_dataset = treatment_set_cut,
                     ntilen = ntilen,
                     i = i)

    #stack the result
    result <- rbind(result, out)

    }

  return(result)

}
