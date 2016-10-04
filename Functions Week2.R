
#function 1
pollutantmean <- function(directory, pollutant, id = 1:332) {
    files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
    means <- numeric()
    
    for (i in id) {
    data <- read.csv(files[i])
    means <- c(means, data[[pollutant]])}
    mean(means, na.rm = TRUE)
}

#-------------
# function 2

complete <- function(directory, id = 1:332) {
    files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)    
    nobs <- numeric()
    
    for (i in id) {
        data <- read.csv(files[i])
        nobs <- c(nobs, sum(complete.cases(data)))
    }
data.frame(id, nobs)    
}


#-----------
# function 3


corr <- function(directory, threshold = 0) {
    
    tcorr <- function(fname) {
        data <- read.csv(file.path(directory, fname))
        nobs <- sum(complete.cases(data))
        if (nobs > threshold) {
            return (cor(data$nitrate, data$sulfate, use="complete.obs"))
        }
    }
    tcorrs <- sapply(list.files(directory), tcorr) 
    tcorrs <- unlist(tcorrs[!sapply(tcorrs, is.null)]) 
    return (tcorrs)
}    
    
    