#' Evaluate Macaulay2 code
#'
#' Evaluate Macaulay2 code
#' 
#' @param code Macaulay2 code as either a character string or function; see examples
#' @param dir directory to place the files in
#' @param opts options for m2
#' @return an object of class m2
#' @export
#' @examples
#' \dontrun{
#'
#' options(digits = 20)
#' 13^20
#' m2("13^20") # correct answer
#' m2("toRR(20,(19004963774880800571392-13^20)/13^20)")
#' options(digits = 7)
#'
#' code <- "
#' 1+1
#' 2+3
#' 100!
#' R = QQ[x,y,z]
#' (x+y)^10
#' curve = ideal( x^4-y^5, x^3-y^7 )
#' gens gb curve
#' m = matrix {{x^2, x^2-y^2, x*y*z^7 }}
#' image m
#' R = QQ[a..d]
#' I = ideal(a^3-b^2*c, b*c^2-c*d^2, c^3)
#' G = gens gb I
#' G
#' "
#' m2(code)
#'
#'
#' code <- "
#' R = QQ[x,y,z,t]
#' I = ideal( t^4 - x, t^3 - y, t^2 - z)
#' gens gb I
#' "
#' m2(code)
#'
#' }
#'
m2 <- function(code, dir = tempdir(), opts = "--script"){
	
  write_m2 <- function(code, outFile = "m2Out", codeFile = "m2Code.m2"){

    # pull code from function body
    if(is.function(code)) code <- as.character(body(code))[-1]
    if(is.character(code)){
      if(str_sub(code, 1, 1) != "\n") code <- paste("\n", code, sep = "")
      if(str_sub(code, -1, -1) != "\n") code <- paste(code, "\n", sep = "")      
      code <- strsplit(code, "\\n")[[1]][-1]
    }

    # add lines to save it
    top <- paste('f = "', outFile, '" << ""', sep = "")
    mid <- paste("f << toString(", code, ") << endl")
    bot <- "f << close"
    code <- c(top, mid, bot)
  
    # write code file
    writeLines(code, con = paste(dir, codeFile, sep = "/"))
    invisible(code)
  }	

  write_m2(code)

  oldWd <- getwd()
  setwd(dir)
  system(paste(getOption("m2Path"), opts, paste(dir, "m2Code.m2", sep = "/"))) 
  setwd(oldWd)
  
  out <- readLines(paste(dir, "m2Out", sep = "/"))
  class(out) <- "m2"
  
  out
}
