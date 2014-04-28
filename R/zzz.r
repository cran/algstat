.onAttach <- function(...) {
  
  if(.Platform$pkgType == "mac.binary"){
  ## find the path on a mac	
  	
    apps <- system("ls /Applications/", intern = TRUE)
    lowerCaseApps <- tolower(apps)
    
    # find Macaulay2
    ndx  <- which(str_detect(lowerCaseApps, "macaulay2"))
    if(length(ndx) == 0){
      options(m2Path = NULL)
      packageStartupMessage("Macaulay2 not found.  Set the location with options(m2Path = [path]).")
    } else {
      macaulayDirFull <- apps[ndx]
      path <- paste("/Applications/", macaulayDirFull, "/bin/M2", sep = "")
      options(m2Path = path)
    }
    
    # find Bertini
    ndx  <- which(str_detect(lowerCaseApps, "bertini"))
    if(length(ndx) == 0){
      options(bertiniPath = NULL)
      packageStartupMessage("Bertini not found.  Set the location with options(bertiniPath = [path]).")
    } else {
      bertiniDirFull <- apps[ndx]
      path <- paste("/Applications/", bertiniDirFull, sep = "")
      options(bertiniPath = path)
    }    
    
    # find latte-integrale 
    ndx  <- which(str_detect(lowerCaseApps, "latte-integrale"))
    if(length(ndx) == 0){
      options(lattePath = NULL)
      packageStartupMessage("latte-integrale not found.  Set the location with options(lattePath = [path]).")
    } else {
      latteDirFull <- apps[ndx]
      path <- paste("/Applications/", latteDirFull, "/dest/bin", sep = "")
      options(lattePath = path)
    }        
    
    
  } else if(.Platform$OS.type == "windows"){
  ## find the path on a pc  	
  	
    options(m2Path = NULL)
    packageStartupMessage("Macaulay2 not found.  Set the location with options(m2Path = [path]).")    
    
    options(bertiniPath = NULL)
    packageStartupMessage("Bertini not found.  Set the location with options(bertiniPath = [path]).")        
    
    options(lattePath = NULL)
    packageStartupMessage("latte-integrale not found.  Set the location with options(lattePath = [path]).")            
    
    
  } else {
  ## find the path on a linux  	
  	
    options(m2Path = NULL)
    packageStartupMessage("Macaulay2 not found.  Set the location with options(m2Path = [path]).")    

    options(bertiniPath = NULL)
    packageStartupMessage("Bertini not found.  Set the location with options(bertiniPath = [path]).")        
    
    options(lattePath = NULL)
    packageStartupMessage("latte-integrale not found.  Set the location with options(lattePath = [path]).")            
    
  }
  
}