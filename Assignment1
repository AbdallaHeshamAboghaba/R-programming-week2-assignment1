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
