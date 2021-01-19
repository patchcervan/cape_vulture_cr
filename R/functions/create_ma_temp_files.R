library(tidyverse)

# Print the individual bird identifier
unique(new_trk$`individual.local.identifier`)

# Remove everything but the name of the bird. If I add three letters to Marie
# all names have the same format.
new_trk <- new_trk %>% 
    mutate(`individual-local-identifier` = 
               if_else(`individual.local.identifier` == unique(new_trk$`individual.local.identifier`)[9],
                       paste("N111 ", unique(new_trk$`individual.local.identifier`)[9], sep = ""),
                       `individual.local.identifier`))

new_trk$bird_name <- str_sub(new_trk$`individual.local.identifier`, 8, -3)

unique(new_trk$bird_name)

# this takes a while so run only if necessary

for(i in 1:length(unique(new_trk$bird_name))){

    bird <- new_trk %>%
        filter(bird_name == unique(new_trk$bird_name)[i])

    saveRDS(bird, file = paste("data/working/pre_proc_data/temp_ma/", unique(new_trk$bird_name)[i],"_temp.rds", sep = ""))
}
