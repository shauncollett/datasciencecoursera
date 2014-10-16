## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
complete <- function(directory, id = 1:332) {
    # clear dataset if it already exists
    if(exists("dataset")){
        rm(dataset)
    }
    
    # character vector e.g. "001.csv", "002.csv" ... "332.csv"
    file_list <- list.files(directory)
    
    for (file in file_list){
        # convert file name to id to see if it's being asked for
        file_id <- as.integer(substr(file, 1, 3))
        
        # if being asked for
        if(file_id %in% id) {
            file_path <- paste(directory, file, sep = "/")
            
            # if the merged dataset doesn't exist, create it
            if (!exists("dataset")){
                dataset <- read.csv(file_path, header=TRUE)
            } 
            else {
                # if the merged dataset does exist, append to it
                temp_dataset <- read.csv(file_path, header=TRUE)
                dataset<-rbind(dataset, temp_dataset)
                rm(temp_dataset)
            }
        }
    }
    #nrow(dataset) #debugging
    
    dataset <- na.omit(dataset)
    a <- aggregate(cbind(sulfate, nitrate) ~ ID, data=dataset, FUN="length",
                   na.action = na.omit)
    names(a) <- c("id", "nobs")
    na.omit(a[order(id), c("id", "nobs")])
}