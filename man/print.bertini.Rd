\name{print.bertini}
\alias{print.bertini}
\title{Pretty printing of Bertini output.}
\usage{
\method{print}{bertini}(x, digits = 3, ...)
}
\arguments{
  \item{x}{an object of class bertini}

  \item{digits}{digits to round to}

  \item{...}{additional parameters}
}
\value{
Invisible string of the printed object.
}
\description{
Pretty printing of Bertini output.
}
\examples{
\dontrun{

# see ?bertini

variety("x^2 + 1")
variety(c("x^2 + 1 + y","y"))

}
}

