

element.exists <- function(var, item)
{
  if(length(var) > 0){
    for(i in 1:length(var)){
      if(var[i] == item){
        return(FALSE)
      }
    }
  }

  return(TRUE)

}


read_files <- function() {

  require(miscTools)
  folder <- "D:\\Camila\\Documentos\\Cap5-Testes\\Correlacao\\rq4\\data\\"
  file_list <- list.files(path=folder, pattern="*.csv")
  for (l in 1:length(file_list)){
    
    str = strsplit(file_list[l], "_")[[1]]
    str = as.numeric(str[1])
    try = FALSE
    result = tryCatch({
      variabilities <-  read.csv(paste(folder, file_list[l], sep=''), na.strings = c("","NA"), header=FALSE,   colClasses=c("V1"="character","V2"="character","V3"="character","V4"="character"))
      try = TRUE
    }, error = function(e) {
      try = FALSE
    })
    

    
    if(try){
      a <- c(variabilities$V1)
      b <- c(variabilities$V2)
      for(i in 1:length(a)){
        
         result <- c(str[1],a[i])
         if(l == 1){
          smoke <- matrix(c(result),ncol=length(result),byrow=TRUE)
          colnames(smoke) <- c("index","a")
          smoke <- as.table(smoke)
        }
        if(l > 1){
          smoke <- rbind(smoke, result)
        }
        
      }

     
      
      
      
      
    }
    

    
    if(!try){
      result <- c(str[1],0, 0, 0, 0, 0 )
      
      if(l == 1){
        smoke <- matrix(c(result),ncol=length(result),byrow=TRUE)
        colnames(smoke) <- c("index","a","b","c", "d", "e")
        smoke <- as.table(smoke)
      }
      if(l > 1){
        smoke <- rbind(smoke, result)
      }
      
    }

    
    
    
    

    
   
  }
  return (smoke)
}


read_filesPE <- function() {

  require(miscTools)
  folder <- "D:\\Camila\\Documentos\\Cap5-Testes\\Correlacao\\rq4\\data2\\"
  file_list <- list.files(path=folder, pattern="*.csv")
  for (l in 1:length(file_list)){
    
    str = strsplit(file_list[l], "p")[[1]]
    str = as.numeric(str[1])
    try = FALSE
    result = tryCatch({
      variabilities <-  read.csv(paste(folder, file_list[l], sep=''), na.strings = c("","NA"), header=FALSE,   colClasses=c("V1"="character","V2"="numeric","V3"="character"))
      try = TRUE
    }, error = function(e) {
      try = FALSE
    })
    

    

      a <- c(variabilities$V1)
      b <- c(variabilities$V2)
      c <- c(variabilities$V3)
      for(i in 1:length(a)){
        
         result <- c(str[1],a[i], b[i], c[i])
         if(l == 1){
          smoke <- matrix(c(result),ncol=length(result),byrow=TRUE)
          colnames(smoke) <- c("index","pe","peso","categoria")
          smoke <- as.table(smoke)
        }
        if(l > 1){
          smoke <- rbind(smoke, result)
        }
        
      }

     
      
      
      

    

    


    
    
    
    

    
   
  }
  return (smoke)
}


cat <- function() {


  index <- c(MyData$index)
  variability <- c(MyData$a)
  pe <- c(MyData$b)
  mud<- c(MyData$c)
  cat<- c(MyData$d)
  pi <- c(MyData$e)
  
  index2 <- c(pes$index)
  pe2 <- c(pes$pe)
  peso<- c(pes$peso)
  cat2<- c(pes$categoria)
  
  impacto <- c()
  pesoimpacto <- c()
  notimpacto <- c()
  pesonotimpacto <- c()
  print(peso)
  
  for(i in 1:length(peso)){
    if(peso[i] > 1){
      print(peso[i])
    }
    
  }



}


r <- read_files()
write.csv(r, file = "MyData.csv",row.names=FALSE)
pe <- read_filesPE()
write.csv(pe, file = "pes.csv",row.names=FALSE)
cat()






read_filesP <- function() {
  
  require(miscTools)
  folder <- "D:\\Camila\\Documentos\\Cap5-Testes\\Correlacao\\rq4\\data\\"
  file_list <- list.files(path=folder, pattern="*.csv")
  for (l in 1:length(file_list)){
    
    str = strsplit(file_list[l], "_")[[1]]
    str = as.numeric(str[1])
    try = FALSE
    result = tryCatch({
      variabilities <-  read.csv(paste(folder, file_list[l], sep=''), na.strings = c("","NA"), header=FALSE,   colClasses=c("V1"="character","V2"="character","V3"="character","V4"="character"))
      try = TRUE
    }, error = function(e) {
      try = FALSE
    })
    
    sumW = sumPE = 0
    
    if(try){

      a <- c(variabilities$V1)
      b <- c(variabilities$V2)

    aux <- c()
    pe <- c()
    for(i in 1:length(b)){
      
      if(element.exists(pe, b[i])){
        pe <- c(pe, b[i])
        aux <- c(aux, a[i])
        sumPE = sumPE + 1
        for(j in 1:length(a)){
          if(b[i] == b[j]){
            if(element.exists(aux, a[j])){
              aux <- c(aux, a[j])
              
            }
          }

        }

        sumW = sumW + length(aux)
        aux <- c()

        
      }
      
      
      
    }
    
    result <- c(str[1],sumPE,sumW, sumW/sumPE)
    if(l == 1){
      smoke <- matrix(c(result),ncol=length(result),byrow=TRUE)
      colnames(smoke) <- c("index","pe","weight","avg")
      smoke <- as.table(smoke)
    }
    if(l > 1){
      smoke <- rbind(smoke, result)
    }
    
    

  }
  
  

    
    
  }
  
  if(!try){

    result <- c(str[1],sumPE,sumW, 0)
    if(l == 1){
      smoke <- matrix(c(result),ncol=length(result),byrow=TRUE)
      colnames(smoke) <- c("index","pe","weight","avg")
      smoke <- as.table(smoke)
    }
    if(l > 1){
      smoke <- rbind(smoke, result)
    }
    
    
  }
  return(smoke)
  
}

smoke <- read_filesP()
print(summary(smoke))
write.csv(smoke, "Pew.csv",row.names=FALSE)

