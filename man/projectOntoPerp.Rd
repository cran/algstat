\name{projectOntoPerp}
\alias{projectOntoPerp}
\title{Project a vector onto the orthogonal complement of the column space of a matrix}
\usage{
projectOntoPerp(A, x)
}
\arguments{
  \item{A}{a matrix}

  \item{x}{a vector}
}
\value{
...
}
\description{
Project a vector onto the orthogonal complement of the
column space of a matrix
}
\examples{
A <- diag(5)[,1:2]
x <- 1:5
projectOnto(A, x)
}

