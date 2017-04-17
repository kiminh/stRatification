\name{sample_splitting_estimation}
\alias{sample_splitting_estimation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sample_splitting_estimation
}
\description{
Estimate average treatment effect in each subgroup created from predictionresult.
}
\usage{
sample_splitting_estimation(data, Y, treatment, X, M, ntilen, adjusted)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
an optional data frame containing the variables in the model.
}
  \item{Y}{
string which indicates the outcome variable in the estimation.
}
  \item{treatment}{
string which indicates the treatment variable.
}
  \item{X}{
string that indicates the control variables. you are able to set like c("a", "b") to implement multiple control variables.
}
  \item{M}{
numeric that how many time will you iterate the splitting estimation. M in Abadie et al. (2016).
}
  \item{ntilen}{
int that show how many group will you make in the dataset
}
  \item{adjusted}{
TRUE/FALSE to set which estimation you want to implement, adjusted estimation or unadjusted estimation.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, Y, treatment, X, M, ntilen, adjusted)
{
    control_set <- data \%>\% filter_(paste(treatment, "==", 0))
    treatment_set <- data \%>\% filter_(paste(treatment, "==",
        1))
    factor_col <- which(sapply(data, class) \%in\% c("character",
        "factor"))
    M <- M
    result <- data.frame()
    for (i in 1:M) {
        sample_flag_control <- sample(NROW(control_set), NROW(control_set)/2,
            replace = F)
        prediction_set <- control_set[sample_flag_control, ]
        estimation_set <- control_set[-sample_flag_control, ]
        estimation_set_cut <- level_cut(dataset = estimation_set,
            prediction_set = prediction_set, factor_col = factor_col)
        treatment_set_cut <- level_cut(dataset = treatment_set,
            prediction_set = prediction_set, factor_col = factor_col)
        ntile_formula <- as.formula(paste(Y, "~", paste(X, collapse = " + ")))
        reg <- prediction_set \%>\% lm(data = ., formula = ntile_formula)
        out <- ATE_ntile(model = reg, Y = Y, X = X, treatment = treatment,
            adjusted = adjusted, estimation_dataset = estimation_set_cut,
            treatment_dataset = treatment_set_cut, ntilen = ntilen,
            i = i)
        result <- rbind(result, out)
    }
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
