read_files <- function() {
  
  require(miscTools)
  folder <- "D:\\Camila\\Documentos\\IMPACTO\\datanalysis\\CAP3\\"
  
  file_list <- list.files(path=folder, pattern="*.csv")
  for (l in 1:length(file_list)){
    variabilities <-  read.csv(paste(folder, file_list[l], sep=''), na.strings = c("","NA"), header=FALSE,   colClasses=c("V1"="character","V2"="character","V3"="character","V4"="character"))
    print(variabilities)
    col1 <- c(variabilities$V1)
    col2 <- c(variabilities$V2)
    col3 <- c(variabilities$V3)
    col4 <- c(variabilities$V4)
    col7 <- c(variabilities$V7)
    depDel <- c(variabilities$V5)
    depAdd <- c(variabilities$V6)
    depCh <- c(variabilities$V8)
    depNCh <- c(variabilities$V9)

    
    add = del = pres = change = notchange = 0
    aux = 1 ;
    x = 1
    for(i in 1:length(depNCh)){
      del = del + depDel[i];
      add = add + depAdd[i];
      change = change + depCh[i];
      notchange = notchange + depNCh[i];
      aux = aux + 1;
      if (aux == 50){
        result <- c(col1[i], col2[i],col3[i], col4[i], col7[i], del, add, change, notchange)
        if(x == 1){
          smoke <- matrix(c(result),ncol=length(result),byrow=TRUE)
          colnames(smoke) <- c("Date","Evolution","Variabilities","TotalDependencies","Preserved","DependenciesDeleted","DependenciesAdditions","DependenciesChanged","DependenciesNotModified")
          smoke <- as.table(smoke)
        }
        if(x > 1){
          smoke <- rbind(smoke, result)
        }
      
        x = 10
        aux = 1
      }
    }
    write.csv(smoke, file = file_list[l],row.names=FALSE)
  }
 
}

read_files()


