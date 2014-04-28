#' Compute the Lp norm of a vector
#'
#' Compute the Lp norm of a vector
#' 
#' @param x x
#' @param p p
#' @return ...
#' @export lpnorm
#' @examples
#'
#' lpnorm(1:10)
#' lpnorm(matrix(1:25, 5, 5))
#' lpnorm(split(1:25, rep(1:5, each = 5)))
#'
#' lpnorm(1:10, 1)
#' lpnorm(matrix(1:25, 5, 5), 1)
#' lpnorm(split(1:25, rep(1:5, each = 5)), 1)
#'
#' lpnorm(rnorm(10), 0)
#' lpnorm(matrix(rnorm(25), 5, 5), 0)
#' lpnorm(split(rnorm(25), rep(1:5, each = 5)), 0)
#'
#' lpnorm(-5:5, Inf)
#' lpnorm(matrix(-25:-1, 5, 5), Inf)
#' lpnorm(split(-25:-1, rep(1:5, each = 5)), Inf)
#'
lpnorm <- function(x, p = 2){
  if(is.vector(x) && !is.list(x)){
  	if(p == Inf) return(max(abs(x)))
    if(p >= 1) return( sum(abs(x)^p)^(1/p) )
    if(0 <= p && p < 1) return( sum(abs(x)^p) )
  }
  if(is.matrix(x)) return(apply(x, 1, lpnorm, p))
  if(is.list(x)) return(sapply(x, lpnorm, p))
  NA  
}








#' Generate spectral data
#'
#' Generate spectral data
#' 
#' @param nVoters number of voters voting
#' @param nObjects number of objects up for selection
#' @param kSelected number of objects selected by each voter
#' @return ...
#' @export rvotes
#' @examples
#' rvotes(100, 10, 3)
#'
rvotes <- function(nVoters, nObjects, kSelected){
  t(replicate(nVoters, sort(sample(nObjects, kSelected))))
}







#' Create an upper triangular matrix
#'
#' Create an upper triangular matrix
#' 
#' @param x a vector
#' @return ...
#' @export upper
#' @examples
#' upper(1:3)
#' lower(1:3)
#'
#' upper(1:6)
#' lower(1:6)
#' 
#' upper(rnorm(6))
#'
upper <- function(x){
  l <- length(x)
  p <- round( (1+sqrt(1+8*l))/2 )	
  ndcs <- unlist(sapply(as.list(0:(p-2)), function(k){
    k*p + (k+2):p
  }))  
  
  if(all(is.integer(x))){
    m <- as.integer( matrix(rep(0, p^2), p, p) )
    m[ndcs] <- as.integer(x)
  } else {
    m <- matrix(rep(0, p^2), p, p)
    m[ndcs] <- x
  }
  
  dim(m) <- c(p,p)
  t(m)
}




#' Create a lower triangular matrix
#'
#' Create a lower triangular matrix
#' 
#' @param x a vector
#' @return ...
#' @export lower
#' @examples
#' upper(1:3)
#' lower(1:3)
#'
#' upper(1:6)
#' lower(1:6)
#' 
#' upper(rnorm(6))
#'
lower <- function(x) t(upper(x))










#' Project a vector onto the column space of a matrix
#'
#' Project a vector onto the column space of a matrix
#' 
#' @param A a matrix
#' @param x a vector
#' @return ...
#' @export projectOnto
#' @examples
#' 
#' A <- diag(5)[,1:2]
#' x <- 1:5
#' projectOnto(A, x)
#' 
#'
projectOnto <- function(A, x) qr.fitted(qr(A), x) 









#' Project a vector onto the orthogonal complement of the column space of a matrix
#'
#' Project a vector onto the orthogonal complement of the column space of a matrix
#' 
#' @param A a matrix
#' @param x a vector
#' @return ...
#' @export projectOnto
#' @examples
#' 
#' A <- diag(5)[,1:2]
#' x <- 1:5
#' projectOnto(A, x)
#' 
#'
projectOntoPerp <- function(A, x) diag(length(x))%*%x - qr.fitted(qr(A), x) 













#' Multinomial coefficient
#'
#' Compute the multinomial coefficient
#'
#' This function computes the multinomial coefficient by computing the factorial of each number on a log scale, differencing log(n!) - sum(log(x!)), and then exponentiating.  It then checks to see if this is an integer; if it's not, it issues a warning.
#' 
#' @param n an integer
#' @param x a vector of integers
#' @return ...
#' @export mchoose
#' @examples
#' 
#' mchoose(6, c(2,2,1,1))
#' 
#' 
#' 
#'
mchoose <- function(n, x){
  lboth <- lfactorial(c(n,x))
  out <- exp(lboth[1] - sum(lboth[-1]))
  if(out != round(out)) warning("log-retransformed multinomial coefficient != rounded value.")
  as.integer(round(out))
}
