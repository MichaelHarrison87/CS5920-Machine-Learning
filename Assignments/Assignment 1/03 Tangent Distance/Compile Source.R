# Comile .c source file into a DLL (for use in Windows)

install.packages("utils")
library(utils)
c_file <- "distance.c"

#R CMD SHLIB c_file