\name{hist.inv}
\alias{hist.inv}
\title{
Inverted or Rotated Histogram
}
\description{
This is a function that displays a rotated or inverted histogram, given data. Depending on the side, it displays the histogram in a different way.
}
\usage{
hist.inv(x, side = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector of numbers}
  \item{side}{1: bottom side histogram, 2: left side histogram}
}
\details{
This function is used within the superplot function to display histograms of certain data from the dataset "PIMA.csv".
}
\value{
plots an inverse histogram
}
\author{
Michael Wang, code by Prof. Cabrera
}
