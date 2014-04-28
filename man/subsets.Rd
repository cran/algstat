\name{subsets}
\alias{subsets}
\title{Compute the subsets of a given set}
\usage{
subsets(set, sizes = 1:length(set), include_null = FALSE)
}
\arguments{
  \item{set}{the original set}

  \item{sizes}{desired size(s) of subsets}

  \item{include_null}{should the empty vector be included?}
}
\value{
a list of subsets as vectors
}
\description{
Compute the subsets of a given set
}
\examples{
subsets(1:3)
subsets(1:3, size = 2)
subsets(1:3, include_null = TRUE)

subsets(c('a','b','c','d'))
subsets(c('a','b','c','d'), include_null = TRUE)
}

