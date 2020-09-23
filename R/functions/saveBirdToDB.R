saveBirdToDB <- function(new_db, overwrite = FALSE){
    current_db <- read.csv("data/working/bird_db.csv")
    
    if(overwrite == FALSE){
        if(unique(new_db$bird_id) %in% current_db$bird_id){
            print("The bird is already in the database. If you are sure, set overwrite = TRUE")
        } else {
            write.table(new_db, file = "data/working/bird_db.csv", sep = ",", append = TRUE, quote = FALSE,
                        col.names = FALSE, row.names = FALSE)
        }
    }

    # Remove bird if we are sure we want to do this
    if(overwrite == TRUE){
        if(unique(new_db$bird_id) %in% current_db$bird_id){
            print("Bird record was overwritten!")
        }

        current_db <- current_db %>%
            filter(bird_id != unique(new_db$bird_id)) %>%
            write_csv("data/working/bird_db.csv")
        
        write.table(new_db, file = "data/working/bird_db.csv", sep = ",", append = TRUE, quote = FALSE,
                    col.names = FALSE, row.names = FALSE)
        
    }

}