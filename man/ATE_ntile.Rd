\name{ATE_ntile}
\alias{ATE_ntile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ATE_ntile
}
\description{
Estimate adjusted or nonadjusted ATE in subgroup.
}
\usage{
ATE_ntile(model, Y, treatment, X, adjusted, estimation_dataset, treatment_dataset, ntilen, i)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{treatment}{
%%     ~~Describe \code{treatment} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{adjusted}{
%%     ~~Describe \code{adjusted} here~~
}
  \item{estimation_dataset}{
%%     ~~Describe \code{estimation_dataset} here~~
}
  \item{treatment_dataset}{
%%     ~~Describe \code{treatment_dataset} here~~
}
  \item{ntilen}{
%%     ~~Describe \code{ntilen} here~~
}
  \item{i}{
%%     ~~Describe \code{i} here~~
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
function (model, Y, treatment, X, adjusted, estimation_dataset,
    treatment_dataset, ntilen, i)
{
    pred_control <- data.frame(pred = predict(model, newdata = estimation_dataset),
        estimation_dataset)
    pred_treatment <- data.frame(pred = predict(model, newdata = treatment_dataset),
        treatment_dataset)
    estimate_result <- ATE_estimate(pred_control, pred_treatment,
        i, adjusted, Y, X, treatment, ntilen)
    dataset_count <- rbind(pred_control, pred_treatment) \%>\%
        mutate(nt = paste("ntile", ntile(pred, n = ntilen), sep = "_")) \%>\%
        group_by_(.dots = c("nt", treatment)) \%>\% summarise(count = n())
    return(estimate_result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
