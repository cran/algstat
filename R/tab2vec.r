#' Convert a contingency table into a vector
#'
#' Convert a contingency table into a vector
#'
#' Convert a contingency table into a vector.  The ordering of the cells is lexicographical.
#' 
#' @param tab an array of counts
#' @return a named integer vector.  the names correspond to the cell indices in the table.
#' @export tab2vec
#' @seealso \code{\link{vec2tab}}
#' @examples
#' 
#' data(Titanic)
#' tab2vec(Titanic)
#' Titanic[1,1,1,1]
#' Titanic[1,1,1,2]
#' 
#'
tab2vec <- function(tab){
  u <- aperm(tab, length(dim(tab)):1)
  if(class(tab[1]) == "numeric") u <- as.vector(u)
  if(class(tab[1]) == "integer") u <- as.integer(u)  
  tmpdf <- expand.grid( 
    rev(lapply(as.list(dim(tab)), function(x) 1:x)) 
  )[,length(dim(tab)):1]
  names(u) <- apply(tmpdf, 1, paste, collapse = ',')
  u
}







#' Convert a vector into an array
#'
#' Convert a vector into an array
#'
#' Convert a vector into an array.  This is the reverse operation of tab2vec
#' 
#' @param vec a vector
#' @param dim the desired array dimensions, oftentimes a vector of the number of levels of each variable in order
#' @return an array
#' @export vec2tab
#' @seealso \code{\link{tab2vec}}
#' @examples
#' 
#' data(Titanic)
#' Titanic
#' tab2vec(Titanic)
#' vec2tab(tab2vec(Titanic), dim(Titanic))
#' vec2tab(tab2vec(Titanic), dim(Titanic)) == Titanic
#' all(vec2tab(tab2vec(Titanic), dim(Titanic)) == Titanic)
#' 
#'
vec2tab <- function(vec, dim){
  aperm(
    array(vec, rev(dim)),
    length(dim):1
  )
}