## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
corr <- function(directory, threshold = 0) {
    # use complete function to get number of cases per monitor
    # remove 1:3 when productionalizing the function
    #cases <- complete(directory, 1:5) #debugging
    cases <- complete(directory)
    # filter out the monitors that fall below the given threshold
    cases <- cases[cases$nobs > threshold, ]
    #print(cases)
    # get polluntants data set
    if(nrow(cases) > 0) {
        pollutants <- getPollutants(directory, as.vector(cases[, "id"]))
        #pollutants[pollutants$ID == 4, ] #debugging
        #names(pollutants) #debugging
        #print(nrow(cases)) #debugging
        for(i in 1:nrow(cases)) {
            id <- cases[i, "id"]
            #print(id) #debugging
            cases[i, "corr"] <- cor(pollutants[pollutants$ID == id, "sulfate"], 
                pollutants[pollutants$ID == id, "nitrate"])
            #print(cases[i, "corr"]) #debugging
        }
        # clear missing values
        result <- na.omit(as.vector(cases[,"corr"]))
    }
    else {
        result <- numeric(length = 0)
    }
    result
}

getPollutants <- function(directory, id = 1:332) {
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
    dataset[complete.cases(dataset$Date, dataset$sulfate, dataset$nitrate, 
                           dataset$ID), ]
}