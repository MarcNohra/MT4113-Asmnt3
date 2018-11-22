\name{MonteCarlo}
\alias{MonteCarlo}

\title{
%%  ~~function to do ... ~~
}
\description{
Run a monte carlo simulation depending on the distributionType. 
It can be normally distributed or truncated
}
\usage{
MonteCarlo(reps, sampleSize, mean1, mean2,
          sd1, sd2, distributionType = "Normal",
          min1 = NULL, min2 = NULL,
          max1 = NULL, max2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reps}{
   integer - number of monte carlo repetitions
}
  \item{sampleSize}{
   integer - the size of the samples to create
}
  \item{mean1, mean2, sd1, sd2}{
   decimals - parameters for the distributions
}
  \item{distributionType}{
   character - can be "Normal" or "Truncate"
}
  \item{min1, min2, max1, max2}{
   decimals - parameters for the distributions
}
}
\details{
This function is an example of the monte carlo algorithm
}
\value{
simulations - 3 dimentional array - the rows represent the samples,
the columns represent the Groups and the 3rd dimension represent the repetitions or resamples
}
\references{
Lecture 6 - MT4113
}
\author{
ID: 180024947
}

\examples{
monteCarlo(reps = 1e3, sampleSize = 100,
            mean1 = 30, mean2 = 35,
            sd1 = 10, sd2 = 15)
}