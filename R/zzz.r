.onAttach <- function(...) {
  
  if(.Platform$pkgType == 'mac.binary'){ ## find the path on a mac	
  	
    apps <- system('ls /Applications/', intern = TRUE)
    lowerCaseApps <- tolower(apps)
    home <- system('ls ~/', intern = TRUE)
    lowerHome <- tolower(home)
    
    
    # find Macaulay2
    ndxApps  <- which(str_detect(lowerCaseApps, 'macaulay2'))
    ndxHome  <- which(str_detect(lowerHome, 'macaulay2'))   
     
    if(length(ndxApps) == 0 && length(ndxHome) == 0){
      options(m2Path = NULL)
    } else if(length(ndxApps) != 0){
      macaulayDirFull <- apps[ndxApps]
      path <- paste('/Applications/', macaulayDirFull, '/bin', sep = '')
      options(m2Path = path)
    } else if(length(ndxHome) != 0){
      macaulayDirFull <- home[ndxHome]
      path <- paste('~/', macaulayDirFull, '/bin', sep = '')
      options(m2Path = path)      
    }
    
    check_for_m2()
    
    
    # find Bertini
    ndxApps  <- which(str_detect(lowerCaseApps, 'bertini'))
    ndxHome  <- which(str_detect(lowerHome, 'bertini'))   
        
    if(length(ndxApps) == 0 && length(ndxHome) == 0){
      options(bertiniPath = NULL)
    } else if(length(ndxApps) != 0){
      bertiniDirFull <- apps[ndxApps]
      path <- paste('/Applications/', bertiniDirFull, sep = '')
      options(bertiniPath = path)
    } else if(length(ndxHome) != 0){
      bertiniDirFull <- home[ndxHome]
      path <- paste('~/', bertiniDirFull, sep = '')
      options(bertiniPath = path)    	
    }  
        
    check_for_bertini()      
    
    
    # find LattE-integrale 
    ndxApps  <- which(str_detect(lowerCaseApps, 'latte'))
    ndxHome  <- which(str_detect(lowerHome, 'latte'))   
    
    if(length(ndxApps) == 0 && length(ndxHome) == 0){
      options(lattePath = NULL)
      packageStartupMessage(paste(
        'LattE not found. ',
        'Set the location with setLattePath().'
      ))
    } else if(length(ndxApps) != 0){
      latteDirFull <- apps[ndxApps]
      path <- paste('/Applications/', latteDirFull, '/bin', sep = '')
      options(lattePath = path)
      options(markovPath = path)      
    } else if(length(ndxHome) != 0){
      latteDirFull <- home[ndxHome]
      path <- paste('~/', latteDirFull, '/bin', sep = '')
      options(lattePath = path)   
      options(markovPath = path)       
    }
    
    check_for_latte()
    check_for_4ti2() 
    
    
      
    
    
    
    
    
    
    
    
  } else if(.Platform$OS.type == 'windows'){ ## find the path on a pc  	
  	
  	if(!any(str_detect(tolower(list.files("C:\\")), "cygwin"))){
  	  packageStartupMessage("Cygwin is required to run most of algstat on a Windows platform.")
  	  packageStartupMessage("  It needs to be in your C:\\ drive, but wasn't found.")  	  
  	  return(invisible())
  	}
  	
    options(m2Path = NULL)
    x <- system('cmd.exe /c whereis.exe m2', TRUE)
    if(str_detect(x, '/')){
      options(m2Path = dirname(x))
    } else {
      packageStartupMessage(paste(
        'Macaulay2 not found. ',
        'Set the location with setM2Path().'
      ))
    }   
    
    options(bertiniPath = NULL)
    x <- system('cmd.exe /c whereis.exe bertini', TRUE)
    if(str_detect(x, '/')){
      options(bertiniPath = dirname(x))
    } else {
      packageStartupMessage(paste(
        'Bertini not found. ',
        'Set the location with setBertiniPath().'
      ))
    }
    
    options(lattePath = NULL)
    x <- system('cmd.exe /c whereis.exe count', TRUE)
    if(str_detect(x, '/')){
      options(lattePath = dirname(x))
    } else {
      packageStartupMessage(paste(
        'LattE not found. ',
        'Set the location with setLattePath().'
      ))
    }
    
    options(markovPath = NULL)
    x <- system('cmd.exe /c whereis.exe markov', TRUE)
    if(str_detect(x, '/')){
      options(markovPath = dirname(x))
    } else {
      packageStartupMessage(paste(
        '4ti2 not found. ',
        'Set the location with setMarkovPath().'
      ))
    }
    
    
    
    
    
    
    
    
    
    
  } else {  ## find the path on a linux  	
  	
    options(m2Path = NULL)
    packageStartupMessage('Macaulay2 not found.  Set the location with setM2Path().')    

    options(bertiniPath = NULL)
    packageStartupMessage('Bertini not found.  Set the location with setBertiniPath().')        
    
    options(lattePath = NULL)
    packageStartupMessage('LattE-integrale not found.  Set the location with setLattePath().')            
    
    options(markovPath = NULL)
    packageStartupMessage('4ti2 not found.  Set the location with setMarkovPath().')                    
    
  }
  
}
































check_for_m2 <- function(){

  if(is.null(getOption("m2Path"))){
    packageStartupMessage(paste(
      'Macaulay2 not found.',
      'Set the location with setM2Path().'
    ))
    return(invisible())
  }
  
  if(length(list.files(getOption('m2Path'))) == 0){
    packageStartupMessage(paste(
      "Macaulay2 appears to be installed,",
      "but it's not where it was expected."
    ))
    packageStartupMessage("  Suggestion : run setM2Path()")
    return(invisible())    
  }	
	
  if(!any('M2' == list.files(getOption('m2Path')))){
    packageStartupMessage(paste(
      "Macaulay2 appears to be installed,",
      "but it's not where it was expected."
    ))
    return(invisible())
  } 
  
}




check_for_bertini <- function(){

  if(is.null(getOption("bertiniPath"))){
    packageStartupMessage(paste(
      'Bertini not found.',
      'Set the location with setBertiniPath().'
    ))
    return(invisible())
  }
  
  if(length(list.files(getOption('bertiniPath'))) == 0){
    packageStartupMessage(paste(
      "Bertini appears to be installed,",
      "but it's not where it was expected."
    ))
    packageStartupMessage("  Suggestion : run setBertiniPath()")
    return(invisible())    
  }	
	
  if(!any('bertini' == list.files(getOption('bertiniPath')))){
    packageStartupMessage(paste(
      "Bertini appears to be installed,",
      "but it's not where it was expected."
    ))
    return(invisible())
  } 
  
}








check_for_latte <- function(){

  if(is.null(getOption("lattePath"))){
    packageStartupMessage(paste(
      'LattE not found.',
      'Set the location with setLattePath().'
    ))
    return(invisible())
  }
  
  if(length(list.files(getOption('lattePath'))) == 0){
    packageStartupMessage(paste(
      "LattE appears to be installed,",
      "but it's not where it was expected."
    ))
    packageStartupMessage("  Suggestion : run setLattePath()")
    return(invisible())    
  }	
	
  if(!any('count' == list.files(getOption('lattePath')))){
    packageStartupMessage(paste(
      "LattE appears to be installed,",
      "but it's not where it was expected."
    ))
    return(invisible())
  } 
  
}









check_for_4ti2 <- function(){

  if(is.null(getOption("markovPath"))){
    packageStartupMessage(paste(
      '4ti2 not found.',
      'Set the location with setMarkovPath().'
    ))
    return(invisible())
  }
  
  if(length(list.files(getOption('markovPath'))) == 0){
    packageStartupMessage(paste(
      "4ti2 appears to be installed,",
      "but it's not where it was expected."
    ))
    packageStartupMessage("  Suggestion : run setMarkovPath()")
    return(invisible())    
  }	
	
  if(!any('markov' == list.files(getOption('markovPath')))){
    packageStartupMessage(paste(
      "4ti2 appears to be installed,",
      "but it's not where it was expected."
    ))
    return(invisible())
  } 
  
}







