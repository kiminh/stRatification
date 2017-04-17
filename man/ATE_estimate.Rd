\name{ATE_estimate}
\alias{ATE_estimate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ATE_estimate
}
\description{
Do dummy regression
}
\usage{
ATE_estimate(pred_control, pred_treatment, i, adjusted, Y, X, treatment, ntilen)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pred_control}{
%%     ~~Describe \code{pred_control} here~~
}
  \item{pred_treatment}{
%%     ~~Describe \code{pred_treatment} here~~
}
  \item{i}{
%%     ~~Describe \code{i} here~~
}
  \item{adjusted}{
%%     ~~Describe \code{adjusted} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{treatment}{
%%     ~~Describe \code{treatment} here~~
}
  \item{ntilen}{
%%     ~~Describe \code{ntilen} here~~
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
function (pred_control, pred_treatment, i, adjusted, Y, X, treatment,
    ntilen)
{
    if (adjusted == T) {
        ate_formula <- as.formula(paste(Y, "~", treatment))
        rbind(pred_control, pred_treatment) \%>\% mutate(nt = paste("ntile",
            ntile(pred, n = ntilen), sep = "_")) \%>\% group_by(nt) \%>\%
            do(mod = lm(data = ., formula = ate_formula)) \%>\%
            broom::tidy(mod) \%>\% filter(term == treatment) \%>\%
            select(nt, ATE = estimate, stder = std.error) \%>\%
            ungroup() \%>\% mutate(M = i)
    }
    else if (adjusted == F) {
        ate_formula <- as.formula(paste(Y, "~", treatment, "+",
            paste(X, collapse = " + ")))
        rbind(pred_control, pred_treatment) \%>\% mutate(nt = paste("ntile",
            ntile(pred, n = ntilen), sep = "_")) \%>\% group_by(nt) \%>\%
            do(mod = lm(data = ., formula = ate_formula)) \%>\%
            broom::tidy(mod) \%>\% filter(term == treatment) \%>\%
            select(nt, ATE = estimate, stder = std.error) \%>\%
            ungroup() \%>\% mutate(M = i)
    }
    else {
        print("adjusted must be TRUE/FALSE")
        (break)()
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
