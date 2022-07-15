% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decay_stepped.R
\name{decay_stepped}
\alias{decay_stepped}
\title{Stepped decay function}
\usage{
decay_stepped(steps, weights)
}
\arguments{
\item{steps}{A \code{numeric} vector. The travel cost steps, in ascending order.
Please do not include travel cost 0 as a step: this is already handled by
the function.}

\item{weights}{A \code{numeric} vector with same length as \code{steps}. The values,
between 0 and 1, that the function assumes at each step. Please do not
include weight 1 as the first value: this is already handled by the
function. The function considers the steps' intervals "open on the right",
meaning that the function assumes the step value at the actual step, not
after it. Please see the illustrative examples for effects of this
assumption on the results.}
}
\value{
A \code{function} that takes a generic travel cost (\code{numeric}) as an input
and returns an impedance factor (\code{numeric}).
}
\description{
Returns a stepped impedance function to be used inside accessibility
calculating functions.

This function is generic over any kind of numeric travel cost,
such as distance, time and money.
}
\examples{
impedance <- decay_stepped(
  c(10, 20, 30, 40),
  weights = c(0.75, 0.5, 0.25, 0)
)

impedance(c(5, 25, 35, 45))

# intervals are open on the right, so the values change exactly at each step
impedance(c(0, 10, 20, 30, 40))

}
\seealso{
Other decay functions: 
\code{\link{decay_binary}()},
\code{\link{decay_exponential}()},
\code{\link{decay_linear}()},
\code{\link{decay_power}()}
}
\concept{decay functions}