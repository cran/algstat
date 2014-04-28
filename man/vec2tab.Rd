\name{vec2tab}
\alias{vec2tab}
\title{Convert a vector into an array}
\usage{
vec2tab(vec, dim)
}
\arguments{
  \item{vec}{a vector}

  \item{dim}{the desired array dimensions, oftentimes a
  vector of the number of levels of each variable in order}
}
\value{
an array
}
\description{
Convert a vector into an array
}
\details{
Convert a vector into an array.  This is the reverse
operation of tab2vec
}
\examples{
data(Titanic)
Titanic
tab2vec(Titanic)
vec2tab(tab2vec(Titanic), dim(Titanic))
vec2tab(tab2vec(Titanic), dim(Titanic)) == Titanic
all(vec2tab(tab2vec(Titanic), dim(Titanic)) == Titanic)
}
\seealso{
\code{\link{tab2vec}}
}

