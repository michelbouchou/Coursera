pollutantmean <- function(directory, pollutant, id = 1:332){
    x <- TRUE
    for (i in id) {
        
        # Transforme id au format 3 caracteres en string dans la variable i
        if (i <10) {
            i <- paste("00", i, sep = "")
        }
        else if (i > 9 && i < 100) {
            i <- paste("0", i, sep = "")
        }
        else if (i > 99) {
            i <- paste(i)
        }
        
        # Créer le premier fichier main
        if (x) {
            main <- read.csv(paste(directory, "/", i, ".csv", sep=""))
            x <- FALSE
        }
        
        # Lis les fichiers souhaite un par un et les ajoute à la data.frame main
        else  {
            temp <- read.csv(paste(directory, "/", i, ".csv", sep=""))
            main <- rbind(main, temp)
        }
    }
    
    # Ne garde que la colonne voulue
    main <- main[pollutant]
    
    # Enleve les valeurs manquantes
    na <- is.na(main)
    main <- main[!na]
    
    # Fais la moyenne
    value <- mean(main)
    print(value)
}