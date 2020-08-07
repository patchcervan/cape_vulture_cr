# This script runs all the processing scripts
# This takes quite a while so run only if completely necessary
# and go find something to do.

rm(list = ls())

library(tidyverse)

# Load scripts
scripts <- dir("R/data_processing")

# remove the "main.R" (current script)
scripts <- scripts[str_detect(scripts, "main", negate = T)]

# remove the creation of templates
scripts <- scripts[str_detect(scripts, "0", negate = T)]

# Run
for(f in 1:length(scripts)){
    
    # Need to reload because of the rm functions in the scripts
    # Load scripts
    scripts <- dir("R/data_processing")
    # remove the "main.R" (current script)
    scripts <- scripts[str_detect(scripts, "main", negate = T)]
    # remove the creation of templates
    scripts <- scripts[str_detect(scripts, "0", negate = T)]
    
    source(paste0("R/data_processing/", scripts[f]))
}
