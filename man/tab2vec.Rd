\name{tab2vec}
\alias{tab2vec}
\title{Convert a contingency table into a vector}
\usage{
tab2vec(tab)
}
\arguments{
  \item{tab}{an array of counts}
}
\value{
a named integer vector.  the names correspond to the cell
indices in the table.
}
\description{
Convert a contingency table into a vector
}
\details{
Convert a contingency table into a vector.  The ordering of
the cells is lexicographical.
}
\examples{
data(Titanic)
tab2vec(Titanic)
Titanic[1,1,1,1]
Titanic[1,1,1,2]
}
\seealso{
\code{\link{vec2tab}}
}

