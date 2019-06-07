read_files <- function() {
  
  require(miscTools)
  folder <- "D:\\Camila\\Documentos\\IMPACTO\\datanalysis\\CAP3Fine\\"
  
  file_list <- list.files(path=folder, pattern="*.csv")
  for (l in 1:length(file_list)){
    variabilities <-  read.csv(paste(folder, file_list[l], sep=''), na.strings = c("","NA"), header=FALSE,   colClasses=c("V1"="character","V2"="character"))
    print(variabilities)
    col1 <- c(variabilities$V1)
    col2 <- c(variabilities$V2)
    col3 <- c(variabilities$V3) #Number of Functions Change
    col4 <- c(variabilities$V4) #Number of Variables Change
    col5 <- c(variabilities$V5) #Function(+)
    col6 <- c(variabilities$V6) #Variable(+)
    col7 <- c(variabilities$V7) #Function(-)
    col8 <- c(variabilities$V8) #Variable(-)	
    col9 <- c(variabilities$V9) #Modifier Change(Var.)
    col10 <- c(variabilities$V10) #Especifier Change(Var.)
    col11 <- c(variabilities$V11) #Qualifier Change(Var.)
    col12 <- c(variabilities$V12) #Type Change(Var.)
    col13 <- c(variabilities$V13) #Function Return Change
    col14 <- c(variabilities$V14) #Modifier Change (Func.)
    col15 <- c(variabilities$V15) #Specifier Change (Func.)
    col16 <- c(variabilities$V16) #Qualifier Change (Func.)
    col17 <- c(variabilities$V17) #Parameters Change (Func.)

    
    scol3 = scol4 = scol5 = scol6 = scol7 = scol8 = scol9 = scol10 = scol11 = scol12 = scol13 = scol14 = scol15 = scol16 = scol17 = 0
    aux = 1 ;
    x = 1
    for(i in 1:length(col1)){
      scol3 = scol3 + col3[i]
      scol4 = scol4 + col4[i] 
      scol5 = scol5 + col5[i]
      scol6 = scol6 + col6[i]
      scol7 = scol7 + col7[i]
      scol8 = scol8 + col8[i]
      scol9 = scol9 + col9[i]
      scol10 = scol10 + col10[i]
      scol11 = scol11 + col11[i]
      scol12 = scol12 + col12[i]
      scol13 = scol13 + col13[i]
      scol14 = scol14 + col14[i]
      scol15 = scol15 + col15[i]
      scol16 = scol16 + col16[i]
      scol17 = scol17 + col17[i]
      
      aux = aux + 1;
      if (aux == 50){
        result <- c(col1[i], col2[i], scol3, scol4, scol5, scol6, scol7, scol8, scol9, scol10, scol11, scol12, scol13, scol14, scol15, scol16, scol17)
        if(x == 1){
          smoke <- matrix(c(result),ncol=length(result),byrow=TRUE)
          colnames(smoke) <- c("Date","Evolution","NumberofFunctionsChange","NumberofVariablesChange","Function(+)","Variable(+)","Function(-)","Variable(-)","ModifierChange(Var.)","Specifier Change(Var.)","QualifierChange(Var.)","Type Change(Var.)","FunctionReturnChange","ModifierChange(Func.)","SpecifierChange(Func.)","QualifierChange(Func.)","ParametersChange(Func.)")
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


