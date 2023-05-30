\name{dparetocounts}
\alias{dparetocounts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~Density function for truncated pareto~~
}
\description{
%%  ~~ Density function for truncated pareto with a value equal to x, lambda equal to mu, a minimum equal to vreal2 and a maximum equal to vreal3.~~
}
\usage{
dparetocounts(x, mu, vreal2, vreal3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~vector of \code{x} values~~
}
  \item{mu}{
%%     ~~Value of the lambda expoenent. It is called \code{mu} here for compatibility with the brms~~
}
  \item{vreal2}{
%%     ~~Minimum value for truncated pareto. This may be the theoretical minimum \code{x} for a dataset or the minimum value in the vector \code{x}. It must be less than or equal to min(\code{x})~~
}
  \item{vreal3}{
%%     ~~Maximum value for truncated pareto. This may be the theoretical maximum \code{x} for a dataset or the maximum value in the vector \code{x}. It must be greater than or equal to min(\code{x})~~
}
}
\details{
%%  ~~ This function assumes that the values of x are ungrouped (i.e., there is no variable for 'counts' that represents the density or number of repeated \code{x} values) ~~
}
\value{
%%  ~Returns probabilities between 0 and 1
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
x = 2
mu = -2,
vreal2 = 1,
vreal3 = 1000

function (x, mu, vreal2, vreal3)
{
    if (vreal2 <= 0 || vreal2 >= vreal3)
        stop("Parameters out of bounds in dPLB")
    if (x < vreal2 || x > vreal3)
        return(0)
    if (mu != -1) {
        density <- (mu + 1) * (x^(mu + 1))/(vreal3^(mu + 1) -
            vreal2^(mu + 1))
    }
    else {
        density <- x^(-2)/(vreal2 * log(vreal3/vreal2))
    }
    density
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
