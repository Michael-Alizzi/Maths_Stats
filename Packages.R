
packages <- c("bslib", "shinydashboard", "shiny", "Ryacas", "plotly", "ggplot2", "dplyr", "tidyr", "openxlsx")

packages <- c("Ryacas", "ggplot2", "dplyr", "tidyr", "openxlsx", "tinytex")

for (pkg in packages) {
  
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    
    library(pkg, character.only = TRUE)
    
    cat(' \n')
    
    cat(pkg, "package Loaded.\n")
    
    cat(' \n')
    
  } else {
    
    tryCatch({
      
      install.packages(pkg)
      
      library(pkg, character.only = TRUE)
      
      cat(' \n')
      
      cat(pkg, "successfully installed and loaded.\n\n")
      
      cat(' \n')
      
    }, error = function(e) {
      
      cat(' \n')
      
      cat("Failed to install", paste0(pkg, ":"), "\n", e$message, "\n\n")
      
      cat(' \n')
      
    })
    
  }
}
 
