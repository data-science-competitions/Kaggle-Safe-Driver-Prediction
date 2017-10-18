################################################################################
#                                Load Libraries                                #
################################################################################
# 1. Add any project specific configuration here.
Sys.setlocale("LC_TIME", "English") # uses english opertaing system naming 
# convention

# 2. Install missing packages for the project and load the required CRAN 
#    packages.
packages.loader <- function(packages.list){
    suppressPackageStartupMessages(
        for (p in packages.list){
            if(!require(p, character.only=TRUE)){
                install.packages(p,dep=FALSE) # install form CRAN
                require(p, character.only=TRUE)
            } # end if require
        } # end for packages list
    ) # end suppressPackageStartupMessages
} # end functions packages.loader
packages.list = c("readr",                                                      # Read csv files form ZIP
                  "tidyverse","magrittr","forcats",                             # Data frames toolbox
                  #"snow","doSNOW","doRNG","doParallel","foreach",              # Parallel computing
                  "ROCR")                                                       # Visualizing the Performance of Scoring Classifiers
packages.loader(packages.list)


# 3. Define Constants
GLOBALS = list(local_evaluation_seed=2015)


# 4. Clean up
rm(packages.list, packages.loader)