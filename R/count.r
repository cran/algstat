#' Count Integer Points in a Polytope
#'
#' \code{count} uses LattE's count function to count the (integer) lattice points in a polytope and compute Ehrhart polynomials.
#' 
#' The specification should be one of the following: (1) a character string or strings containing an inequality in the mpoly expression format (see examples), (2) a list of vertices, or (3) raw code for LattE's count program.  If a character vector is supplied, (1) and (3) are distinguished by the number of strings.
#'
#' Behind the scenes, count works by writing a latte file and running count on it.  If a specification other than a length one character is given to it (which is considered to be the code), count attempts to convert it into LattE code and then run count on it.
#' 
#' @param spec specification, see details and examples
#' @param dir directory to place the files in, without an ending /
#' @param opts options for count; "" for a hyperplane representation, "--vrep" for a vertex representation; see the LattE manual at \url{http://www.math.ucdavis.edu/~latte}
#' @param quiet show latte output
#' @param mpoly when opts = "--ehrhart-polynomial", return the mpoly version of it
#' @return the count.  if the count is a number has less than 10 digits, an integer is returned.  if the number has 10 or more digits, an integer in a character string is returned. you may want to use the gmp package's as.bigz to parse it.
#' @export count
#' @examples
#' \dontrun{
#' 
#'
#'
#' 
#' 
#' spec <- c("x + y <= 10", "x >= 1", "y >= 1")
#' count(spec)
#' count(spec, opts = "--dilation=10")
#' count(spec, opts = "--homog")
#' 
#' # by default, the output from LattE is in 
#' list.files(tempdir())
#' list.files(tempdir(), recursive = TRUE)
#' 
#' # ehrhart polynomials
#' count(spec, opts = "--ehrhart-polynomial")
#' count(spec, opts = "--ehrhart-polynomial", mpoly = FALSE)
#  count(spec, opts = "--ehrhart-series")
#  count(spec, opts = "--ehrhart-taylor-3")
#' 
#' 
#' # the number of tables with the same marginals
#' data(politics)
#' count(c(
#'   "x11 + x12 == 10", 
#'   "x21 + x22 == 10", 
#'   "x11 + x21 == 9", 
#'   "x12 + x22 == 11",
#'   "x11 >= 0", "x21 >= 0", "x12 >= 0", "x22 >= 0"
#' ))
#' countTables(politics)
#' 
#' 
#' # by vertices
#' spec <- list(c(1,1),c(10,1),c(1,10),c(10,10))
#' count(spec)
#' count(spec, opts = "--vrep")
#' 
#' code <- "
#' 5 3
#' 1 -1 0
#' 1 0 -1
#' 1 -1 -1
#' 0 1 0
#' 0 0 1
#' "
#' count(code)
#'
#' 
#'
#' }
#' 
count <- function(spec, dir = tempdir(), opts = "", 
  quiet = TRUE, mpoly = TRUE)
{

  specification <- "unknown"
  
  
  
  ## look at opts
  if(opts == "--ehrhart-series" && mpoly){
    stop("this option is not yet supported by algstat.", 
      call. = FALSE)  	
    message("mpoly can't handle rational functions; reverting to raw output.")
    mpoly <- FALSE
  }
    
  if(opts == "--simplified-ehrhart-polynomial"){
    stop("this option is not supported by algstat.", 
      call. = FALSE)
  }
  
  if(str_detect(opts, "--ehrhart-taylor")){
    stop("this option is not yet supported by algstat.", 
      call. = FALSE)
  }  
  
    
    
  ## if the specification is pure code
  if(is.character(spec) && length(spec) == 1){ 
    specification <- "code"
    code <- spec
  }


  ## check for vertex specification
  if(is.list(spec) && !is.mpolyList(spec)){
  	specification <- "vertex"
  	
    if(opts == ""){
      message('setting opts = \"--vrep\"; see ?count')
      opts <- "--vrep"
    }
    
  }
   
  
  ## if giving a character string of equations, parse
  ## each to the poly <= 0 format
  if(is.character(spec) && length(spec) > 1){
  	
  	parsedSpec <- as.list(rep(NA, length(spec)))
  	
  	geqNdcs <- which(str_detect(spec, " >= "))
  	leqNdcs <- which(str_detect(spec, " <= "))  	
  	eeqNdcs <- which(str_detect(spec, " == "))  	  	
  	eqNdcs <- which(str_detect(spec, " = "))  	  	  	

    if(length(geqNdcs) > 0){
      tmp <- strsplit(spec[geqNdcs], " >= ")
      parsedSpec[geqNdcs] <- 
        lapply(tmp, function(v) mp(v[2]) - mp(v[1]))
    } 
    
    if(length(leqNdcs) > 0){
      tmp <- strsplit(spec[leqNdcs], " <= ")
      parsedSpec[leqNdcs] <- 
        lapply(tmp, function(v) mp(v[1]) - mp(v[2]))
    }     
    
    if(length(eeqNdcs) > 0){
      tmp <- strsplit(spec[eeqNdcs], " == ")
      parsedSpec[eeqNdcs] <- 
        lapply(tmp, function(v) mp(v[1]) - mp(v[2]))
    }          	
    
    if(length(eqNdcs) > 0){
      tmp <- strsplit(spec[eqNdcs], " = ")
      parsedSpec[eqNdcs] <- 
        lapply(tmp, function(v) mp(v[1]) - mp(v[2]))
    } 
    
    linearityNdcs <- sort(c(eeqNdcs, eqNdcs))
      	
    spec <- parsedSpec
    class(spec) <- "mpolyList"
  }
  
  
  ## convert the mpoly specification into a matrix, see 
  ## latte manual, p. 8
  if(is.mpolyList(spec)){
  	specification <- "hyperplane"
    if(!all(is.linear(spec))){
      stop("all polynomials must be linear.", call. = FALSE)
    }
    mat <- mpolyListToMat(spec)
    mat <- cbind(-mat[,"coef",drop=FALSE], -mat[,-ncol(mat)])
    
    # convert to code
    code <- paste(nrow(mat), ncol(mat))
    code <- paste0(code, "\n")
    code <- paste0(code, 
      paste(apply(unname(mat), 1, paste, collapse = " "), 
      collapse = "\n")
    ) 
        
    if(length(linearityNdcs) > 0){
      code <- paste0(code, "\n")
      code <- paste0(code,
        paste0("linearity ", length(linearityNdcs), " ", 
          paste(linearityNdcs, collapse = " "))
      )
    }
  }  
  
  
  ## convert vertex specification into a matrix
  if(specification == "vertex"){
	
  	if(any(!sapply(spec, function(v) length(v) != 1))){
  	  stop("if providing a vertex specification,\n each point must have the same number of coordinates.", call. = FALSE)
  	}
  	  	
    mat <- matrix(unlist(spec), ncol = 2, byrow = TRUE)

    # convert to code
    mat <- cbind(1, mat)
    code <- paste(nrow(mat), ncol(mat))
    code <- paste0(code, "\n")
    code <- paste0(code, 
      paste(apply(unname(mat), 1, paste, collapse = " "), 
      collapse = "\n")
    )     
    
  }
	
	
  ## make dir to put latte files in (within the tempdir) timestamped
  timeStamp <- as.character(Sys.time())
  timeStamp <- chartr("-", "_", timeStamp)
  timeStamp <- chartr(" ", "_", timeStamp)
  timeStamp <- chartr(":", "_", timeStamp)
  dir2 <- file.path2(dir, timeStamp)
  suppressWarnings(dir.create(dir2))
	

  ## write code file
  writeLines(code, con = file.path2(dir2, "countCode.latte"))


  ## switch to temporary directory
  oldWd <- getwd()
  setwd(dir2)
  
  
  ## run count
  if(.Platform$OS.type == "unix"){    	
    outPrint <- capture.output(system(
      paste(
        file.path2(getOption("lattePath"), "count"), 
        opts, 
        file.path2(dir2, "countCode.latte")
      ),
      intern = TRUE, ignore.stderr = TRUE
    ))      
  } else { # windows    	
    matFile <- file.path2(dir2, "countCode.latte")
    matFile <- chartr("\\", "/", matFile)
    matFile <- str_c("/cygdrive/c", str_sub(matFile, 3))
    outPrint <- capture.output(system(
      paste(
        paste0("cmd.exe /c env.exe"),
        file.path(getOption("lattePath"), "count"), 
        opts, 
        matFile
      ),
      intern = TRUE, ignore.stderr = TRUE
    ))      
  }

  ## print count output when quiet = FALSE
  if(!quiet || opts == "--ehrhart-polynomial" ||
    opts == "--ehrhart-series" ||
    str_detect(opts, "--ehrhart-taylor")
  ){

    # cut off line numbers
    sval <- str_locate(outPrint[1], '"')[1]
    outPrint <- str_sub(outPrint, start = sval + 1)
  
    # remove quotes
    outPrint <- str_replace_all(outPrint, '"', "")
  
    # replace tabs (best i can do)
    outPrint <- str_replace_all(outPrint, "\\\\t", "\t")
    outPrint <- str_replace_all(outPrint, "\\\\r", "\n")    
  
    # print
    if(!quiet) cat(outPrint, sep = "\n")
    
    if(opts == "--ehrhart-polynomial"){
      out <- rev(outPrint)[2]
      if(!mpoly) return(str_trim(out))      
      out <- str_replace_all(out, " \\* ", " ")
      if(str_sub(out, 1, 5) == " + 1 ") out <- str_sub(out, 6)
      return(mp(str_trim(out)))
    }

    # these are broken...
    if(opts == "--ehrhart-series") return(rev(outPrint)[2])
      
    if(str_detect(opts, "--ehrhart-taylor")) 
      return(rev(outPrint)[2])
        
  }

  
  
  ## figure out what files to keep them, and make 4ti2 object
  out <- readLines("numOfLatticePoints")
  if(nchar(out) < 10) out <- as.integer(out)
  
  
  ## print out stats
  if(!quiet){
    cat(readLines("latte_stats"), sep = "\n")
    cat("\n")
  }

  
  ## migrate back to original working directory
  setwd(oldWd)
  
  
  ## out
  out
}































mpolyListToMat <- function(mpolyList){
  # this only works for linear mpolyList objects
  vars <- vars(mpolyList)
  varsC <- c(vars, "coef")
  vecMpolyList <- unclass(mpolyList)
  vecMpolyList <- lapply(vecMpolyList, unclass)
  vecMpolyList <- lapply(vecMpolyList, lapply, function(v){
    if(names(v)[1] == "coef") return(v)
    o <- v["coef"]
    names(o) <- names(v)[1]
    o
  })
  vecMpolyList <- lapply(vecMpolyList, unlist)  
  vecMpolyList <- lapply(vecMpolyList, function(x){
    varsNeeded <- setdiff(varsC, names(x))
    if(length(varsNeeded) > 0){
      tmp <- rep(0, length(varsNeeded))
      names(tmp) <- varsNeeded
      x <- c(x, tmp)
      x <- x[varsC]
    }
    x
  })   
  vecMpolyList <- lapply(vecMpolyList, function(x){ 
    df <- as.data.frame(t(x))
    row.names(df) <- runif(1) # another way?
    df
  })
  vecMpolyList  <- unsplit(vecMpolyList, 
  1:length(vecMpolyList), drop = TRUE)
  row.names(vecMpolyList) <- 1:nrow(vecMpolyList)
  
  as.matrix(vecMpolyList)
}