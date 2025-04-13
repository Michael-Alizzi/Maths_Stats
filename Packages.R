packages <- c("Ryacas", "ggplot2", "dplyr", "tidyr", "openxlsx", "tinytex")

for (pkg in packages) {
  
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    
    library(pkg, character.only = TRUE)
    
    cat(pkg, "package Loaded.\n")
    
  } else {
    
    install.packages(pkg)
    
    cat(pkg, "package installed\n")
    
    
  }
}

