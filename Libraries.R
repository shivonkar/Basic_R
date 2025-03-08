cran <- "https://cran.rstudio.org";  options(repos = cran, verbose = F)

#From Cran
{ 
  
  #Logging
  {
    install.packages("logging", quiet = T)
  }
  
  # Machine learning
  {
    install.packages("caret", quiet = T) #ML names(getModelInfo()) to see all supported packages
  }
  
  # Visualization and graphics  
  {
    install.packages("ggplot2", quiet = T)
    install.packages("ggrepel", quiet = T) # Repel overlapping text labels away from each other.
  }
  
  # Missing imputations
  {
    install.packages("mice", quiet = T) # Multiple Imputation
  }

}
