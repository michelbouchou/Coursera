corr <- function(directory, threshold = 0) {
    
    x <- TRUE
    y <- TRUE
    z <- TRUE
    
    
    # Write a function that takes a directory of data files and a threshold for complete cases and 
    # calculates the correlation between sulfate and nitrate for monitor locations where the number 
    # of completely observed cases (on all variables) is greater than the threshold.
        
    all <- complete(directory, 1:332)
    
    # Boucle for pour lire le fichier all
    for (i in 1:332) {
    
        # Condition if nobs > threshold
        if (all[i, "nobs"] > threshold) {
            
            # placer les id respectant la condition
            if (y) {
                more <- i
                y <- FALSE
            }
            else
                more <- cbind(more, i)
        }
    }
    
    for (j in more) {
        
        # Transforme id au format 3 caracteres en string dans la variable i
        if (j <10) {
            i <- paste("00", j, sep = "")
        }
        else if (j > 9 && j < 100) {
            i <- paste("0", j, sep = "")
        }
        else if (j > 99) {
            i <- paste(j)
        }
        
        
        temp <- read.csv(paste(directory, "/", i, ".csv", sep=""))
        main <- temp[complete.cases(temp),]

        
        if (z) {
            final <- cor(main[,"sulfate"], main[,"nitrate"])
            z <- FALSE
        }
        else
            final <- c(final, cor(main[,"sulfate"], main[,"nitrate"]))
    
    }
    
    
    final
}