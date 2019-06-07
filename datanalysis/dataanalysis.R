

element.exists <- function(var, item)
{
  for(i in 1:length(var)){
      if(var[i] == item){
        return(FALSE)
      }
  }
  return(TRUE)

}


read_files <- function() {

  require(miscTools)
  folder <- "D:\\Camila\\Documentos\\IMPACTO\\datanalysis\\data\\"
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
    
    sumFunc = sumVar = 0
    FAdd = FExc = FPar = FTyp = FMod = FEsp = FQua = 0
    VAdd = VExc = VTyp = VMod = VEsp = VQua = 0
    
    if(try){
      a <- c(variabilities$V1)
      b <- c(variabilities$V2)
      c <- c(variabilities$V3)
      d <- c(variabilities$V4)
      e <- c(variabilities$V5)
      
      aux <- c(a[1])
      for(i in 1:length(a)){
        
        if(element.exists(aux, a[i])){
          aux <- c(aux, a[i])
        }
        
        if(d[i] == "Function"){
          sumFunc = sumFunc + 1
          FAdd = ifelse(c[i]=="additon",FAdd+1,FAdd)
          FExc = ifelse(c[i]=="exclusion",FExc+1,FExc)
          FPar = ifelse(c[i]=="parameter",FPar+1,FPar)
          FTyp = ifelse(c[i]=="type",FTyp+1,FTyp)
          FMod = ifelse(c[i]=="modifier",FMod+1,FMod)
          FEsp = ifelse(c[i]=="specifier",FEsp+1,FEsp)
          FQua = ifelse(c[i]=="qualifier",FQua+1,FQua)
        }
        if(d[i] == "Variable"){
          sumVar = sumVar + 1
          VAdd = ifelse(c[i]=="additon",VAdd+1,VAdd)
          VExc = ifelse(c[i]=="exclusion",VExc+1,VExc)
          VTyp = ifelse(c[i]=="type",VTyp+1,VTyp)
          VMod = ifelse(c[i]=="modifier",VMod+1,VMod)
          VEsp = ifelse(c[i]=="specifier",VEsp+1,VEsp)
          VQua = ifelse(c[i]=="qualifier",VQua+1,VQua)
        }
        
      }
      result <- c(str[1],e[i],length(aux),sumFunc, FAdd, FExc, FPar, FTyp, FMod, FEsp, FQua ,sumVar, VAdd, VExc, VTyp, VMod, VEsp,VQua )
      
      if(l == 1){
        smoke <- matrix(c(result),ncol=length(result),byrow=TRUE)
        colnames(smoke) <- c("index","Possible","variabilities","sumFunc", "FAdd", "FExc", "FPar", "FTyp", "FMod", "FEsp", "FQua" ,"sumVar", "VAdd", "VExc", "VTyp", "VMod", "VEsp","VQua")
        smoke <- as.table(smoke)
      }
      if(l > 1){
        smoke <- rbind(smoke, result)
      }
      
    }
    

    
    if(!try){
      result <- c(str[1],sumFunc,sumFunc,sumFunc, FAdd, FExc, FPar, FTyp, FMod, FEsp, FQua ,sumVar, VAdd, VExc, VTyp, VMod, VEsp,VQua )
      
      if(l == 1){
        smoke <- matrix(c(result),ncol=length(result),byrow=TRUE)
        colnames(smoke) <- c("index","Possible","variabilities","sumFunc", "FAdd", "FExc", "FPar", "FTyp", "FMod", "FEsp", "FQua" ,"sumVar", "VAdd", "VExc", "VTyp", "VMod", "VEsp","VQua")
        smoke <- as.table(smoke)
      }
      if(l > 1){
        smoke <- rbind(smoke, result)
      }
      
    }

    
    
    
    

    
   
  }
  print(length(aux))
  return (smoke)
}

r <- read_files()
write.csv(r, file = "MyData.csv",row.names=FALSE)

