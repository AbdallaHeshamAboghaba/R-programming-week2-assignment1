Part 1 
pollutantmean <- function(directory, pollutant, id= 1:332){
  
  ## Create an empty vector of pollutants
  pollutants = c()
  
  ## Get a list of filenames
  filenames = list.files(directory)
  
  ## For each .csv file in id
  for(i in id){
    
    ## Concatinate the directory and filename
    ## e.g. directory = "C:/folder", filenames = vector("001.csv", "002.csv", ...), filepath="C:/folder/001.csv"
    filepath=paste(directory,"/" ,filenames[i], sep="")
    
    ## read in each file and store it in data
    data = read.csv(filepath, header = TRUE)
    
    ##Concatinate the vectors from each file of the pollutant('sulfate' or 'nitrate') column to pollutants vector
    pollutants = c(pollutants, data[,pollutant])
    
  }
  ## Get the mean of the pollutants and remove NA values
  pollutants_mean = mean(pollutants, na.rm=TRUE)
  
  ## Return the mean 'pollutants_mean'
  pollutants_mean
}

# Example usage
source("pollutantmean.R")
pollutantmean("specdata", "sulfate", 1:10)

#Solution
[1] 4.064128

# Example usage
source("pollutantmean.R")
pollutantmean("specdata", "nitrate", 70:72)

#Solution
[1] 1.706047 

Part 2

complete <- function(directory, id= 1:332){
  
  ## Create an empty vector of id's
  ids = c()
  
  ## Create an empty vector of nobs
  nobss = c()
  
  ## Get a list of filenames
  filenames = list.files(directory)
  
  ## For each .csv file in id
  for(i in id){
    
    ## Concatinate the directory and filename
    ## e.g. directory = "C:/folder", filenames = vector("001.csv", "002.csv", ...), filepath="C:/folder/001.csv"
    filepath=paste(directory,"/" ,filenames[i], sep="")
    
    ## read in each file and store it in data
    data = read.csv(filepath, header = TRUE)
    
    ##Get a subset of all rows with complete data meaning no NA's
    ##completeCases = subset(data, !is.na(Date) & !is.na(sulfate) & !is.na(nitrate) & !is.na(id),select = TRUE )
    completeCases = data[complete.cases(data), ]
    
    ids =  c(ids, i)                    ## We can use i for id and concatinate a vector of id's
    nobss = c(nobss, nrow(completeCases) )## Concatinates the number of completed rows from the subset into a vector
   
  }
  ## Return the data frame
  data.frame(id=ids, nobs=nobss)
}

#Example usage
source("complete.R")
complete("specdata", c(2, 4, 8, 10, 12))

#Solution
   id nobs
 1  2 1041
 2  4  474
 3  8  192
 4 10  148
 5 12   96

Part 3

source("complete.R")
corr <- function(directory, threshold = 0){
  
  completes = complete(directory, 1:332)
  completes_above_threshold = subset(completes, nobs > threshold )
  
  ## Initialize empty vector variable
  correlations <- vector()
  
  ## Get a list of filenames
  filenames = list.files(directory)
  
  ## For each .csv file in id
  for(i in completes_above_threshold$id){
    
    ## Concatinate the directory and filename
    ## e.g. directory = "C:/folder", filenames = vector("001.csv", "002.csv", ...), filepath="C:/folder/001.csv"
    filepath=paste(directory,"/" ,filenames[i], sep="")
    
    ## read in each file and store it in data
    data = read.csv(filepath, header = TRUE)
    
    ## Calculate and store the number of completed cases
    completeCases = data[complete.cases(data),]
    count = nrow(completeCases)
    
    ## Calculate and store the count of complete cases
    ## if threshhold is reached
    if( count >= threshold ) {
      correlations = c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
    }
  }
  correlations
}
# Example Usage
source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)

#Solution
[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
