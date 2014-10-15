## 'directory' is a character vector of length 1 indicating
##      the location of the CSV files
## 'pollutant' is a character vector of length 1 indicating
##      the name of the pollutant for which we will calculate the
##      mean; either "sulfate" or "nitrate".
## 'id' is an integer vector indicating the monitor ID numbers
##      to be used
## Return the mean of the pollutant across all monitors list
##      in the 'id' vector (ignoring NA values)
pollutantmean <- function(directory, pollutant, id = 1:332) {
    
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
    
    # Despite filtering above for performance, additionally filter out ID
    mean(dataset[dataset$ID %in% id,pollutant], na.rm = TRUE)
}
