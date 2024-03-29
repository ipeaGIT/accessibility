% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decay_logistic.R
\name{decay_logistic}
\alias{decay_logistic}
\title{Logistic decay function}
\usage{
decay_logistic(cutoff, sd)
}
\arguments{
\item{cutoff}{A \code{numeric} vector. The cost value that serves as the
inflection point of the cumulative logistic curve.}

\item{sd}{A \code{numeric} vector with same length as \code{cutoff}. The standard
deviation of the logistic curve. Values near 0 result in weighting curves
that approximate binary decay, while higher values tend to linearize the
decay.}
}
\value{
A \code{function} that takes a generic travel cost vector (\code{numeric}) as
input and returns a vector of weights (\code{numeric}).
}
\description{
Returns a logistic weighting function (in which the weights
follow the distribution of a reversed cumulative logistic curve) to be
used inside accessibility calculating functions. The logistic curve is
parameterized with the cutoff that sets its inflection point and the standard
deviation that sets its steepness.

This function is generic over any kind of numeric travel cost,
such as distance, time and money.
}
\details{
When using a function created with \code{decay_logistic()}, the output is
named after the combination of cutoffs (\code{"c"}) and standard deviations
(\code{"sd"}) - e.g. given the cutoffs \code{c(30, 40)} and the standard deviations
\code{c(10, 20)}, the first element of the output will be named \code{"c30;sd10"} and
the second will be named \code{"c40;sd20"}. This function uses the adjusted
logistic decay curve proposed by
\insertCite{bauer2016measuring;textual}{accessibility}, in which the
condition f(0) = 1 is met (i.e. the weight of an opportunity whose cost to
reach is 0 is 1).
}
\examples{
\dontshow{if (identical(tolower(Sys.getenv("NOT_CRAN")), "true")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
weighting_function <- decay_logistic(cutoff = 30, sd = 5)

weighting_function(c(0, 30, 45, 60))

weighting_function <- decay_logistic(cutoff = c(30, 45), sd = c(5, 10))

weighting_function(c(0, 30, 45, 60))
\dontshow{\}) # examplesIf}
}
\references{
\insertAllCited{}
}
\seealso{
Other decay functions: 
\code{\link{decay_binary}()},
\code{\link{decay_exponential}()},
\code{\link{decay_linear}()},
\code{\link{decay_power}()},
\code{\link{decay_stepped}()}
}
\concept{decay functions}
