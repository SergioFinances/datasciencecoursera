
#Función 1
getwd()
setwd("D:/documentos/MAESTRIA/datasciencecoursera/specdata")

pollutantmean <- function(directory, pollutant, id=1:332) {
  files <- list.files(pattern="*.csv")
  data_list <- lapply(files, read.csv, header = TRUE)
  df <- do.call(rbind, data_list)
  df <- na.omit(df)
  View(df)
  if (length(id) == 1){
    y <- subset(df,ID == id,select=c(pollutant))
    len <- nrow(y)
    x <- sum(y)
    Prom <- x/len
    return(Prom)
  } else {
    x <- 0
    len <- 0
    for (i in id){
      y <- subset(df,ID == i,select=c(pollutant))
      len <- len + nrow(y)
      x <- x + sum(y)
    }
    Prom <- x/len
    return(Prom)
  }
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)

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

complete("specdata",c(6, 10, 20, 34, 100, 200, 310))

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

corr("specdata")                

