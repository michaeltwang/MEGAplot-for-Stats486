\name{superplot}
\alias{superplot}
\title{
Superplot Function
}
\description{
Creates a layout that plots a scatterplot of data as well as two inverted histograms of each data corresponding to each axis.
}
\usage{
superplot(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Data 1 - e.g. corresponds to PLASMA data in PIMA.csv dataset}
  \item{y}{Data 2 - e.g. corresponds to BP data in PIMA.csv dataset}
}
\details{
Creates a layout, scatterplot put in section 1 and the two histograms are put in sections 2 and 3.
}
\value{
Returns two inverted histograms on the x and y axes and also plots a scatterplot in the middle.
}
\author{
Michael Wang, code by Prof. Cabrera
}
