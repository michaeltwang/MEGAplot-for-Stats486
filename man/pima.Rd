\name{PIMA.csv}
\alias{PIMA.csv}
\docType{data}
\title{
Pima Indians Dataset
}
\description{
Dataset of health related numerical data of Pima Indians related to diabetes.
}
\usage{data("PIMA.csv")}
\format{
  A data frame with 768 observations on the following 9 variables.
  \describe{
    \item{\code{PRG}}{a numeric vector}
    \item{\code{PLASMA}}{a numeric vector}
    \item{\code{BP}}{a numeric vector}
    \item{\code{THICK}}{a numeric vector}
    \item{\code{INSULIN}}{a numeric vector}
    \item{\code{BODY}}{a numeric vector}
    \item{\code{PEDIGREE}}{a numeric vector}
    \item{\code{AGE}}{a numeric vector}
    \item{\code{RESPONSE}}{a factor with levels \code{0} \code{1}}
  }
}
\details{
Pima Indians Diabetes Health info. This dataset is used in the code.R script.
}
\examples{
data(PIMA.csv)
superplot(pima[,2], pima[,3])
}
\keyword{datasets}
