complete <- function (directory, id = 1:332) {
    x <- TRUE
    for (j in id) {
        
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
        na <- complete.cases(temp)
        if (x) {
            value <- c(id = j, nobs = nrow(temp[na,]))
            x <- FALSE
        }
        
        # Lis les fichiers souhaite un par un et les ajoute à la data.frame main
        else  {
            value <-  rbind(value, c(id = j, nrow(temp[na,])))
        }
        
    }
    value
}