
#Función 1

pollutantmean <- function(directory, pollutant, id=1:332) {
  files <- list.files(pattern="*.csv")
  data_list <- lapply(files, read.csv, header = TRUE)
  df <- do.call(rbind, data_list)
  df <- na.omit(df)
  if (length(id) == 1){
    y <- subset(df,ID == id,select=c(pollutant))
    len <- nrow(y)
    x <- sum(y)
    Prom <- x/len
    return(Prom)
  } else {
    x <- 0
    len <- 0
    for (i in 1:332){
      y <- subset(df,ID == i,select=c(pollutant))
      len <- len + nrow(y)
      x <- x + sum(y)
    }
    Prom <- x/len
    return(Prom)
  }
}

mean(df[,3])

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

# Función 2

complete <- function(directory,id=1:332){
  files <- list.files(pattern="*.csv")
  data_list <- lapply(files, read.csv, header = TRUE)
  df <- do.call(rbind, data_list)
  df <- na.omit(df)
  com <- matrix(id,ncol = 2, nrow = length(id))
  colnames(com) <- c("id", "nobs")
  coun <- 0
  for (i in id){
    y <- subset(df,ID == i,select=c(ID))
    len <- nrow(y)
    coun <- coun + 1
    com[coun,2] <- len
  }
  return(com)
}

complete("specdata")
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
cc <- complete("specdata", 54)
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


# Función 3

corr <- function(directory,threshold = 0){
  Mat <- complete(directory)
  id_2 <- data.frame()
  for (i in 1:nrow(Mat)){
   if (Mat[i,2] >= threshold){
    id_2[i,1] <- Mat[i,1]
   }
  }
  id_2 <- na.omit(id_2)
  com <- matrix(ncol = 1, nrow = length(id_2))
  colnames(com) <- "Corr"
  coun <- 0
  for (i in 1:nrow(id_2)){
    y <- subset(df,ID == id_2[i,],select=c(sulfate,nitrate))
    Cal_Corr <- cor(y[,1],y[,2])
    coun <- coun + 1
    com[coun] <- Cal_Corr
  }
  return(com)
}

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

cr <- corr("specdata", 400)
head(cr)
