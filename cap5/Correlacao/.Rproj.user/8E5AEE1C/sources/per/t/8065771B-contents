require(readxl)

vpy <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Totem")
load_concat_data <- function(arq,ranges){
  result <- c()
  
  for (l in 1:length( excel_sheets(arq))){
    
    data_column <- read_excel(arq, sheet=l,na = "", range = (ranges))
    
    result <- c(result,na.omit(data_column[[1]]))
    
  }
  
  return ( result)
  
}

scatter_maintenance_vs_impact_concatData <- function(){

  #Capitulo 4
  deps <- load_concat_data("CAP4_FineGrained.xlsx","x1:x20000")
  mp <- load_concat_data("CAP4_FineGrained.xlsx","y1:y20000")
  
  scatter(deps, mp, "", "Total de Depend?cias", "Pontos de Manuten?o")
  
  #Capitulo 5
  pip <- load_concat_data("Cap5_IMPACTO.xlsx","e1:e20000")
  ip <- load_concat_data("Cap5_IMPACTO.xlsx","f1:f20000")
  

  
  result <- c()
  resultb <- c()
  for (i in 1:length(mp)){
    if(mp[i] != 0 ){
      result <- c(result, mp[i]/deps[i])
      
      if(pip[i] != 0){
        resultb <- c(resultb, ip[i]/pip[i])
      }
      if(pip[i] == 0){
        resultb <- c(resultb, 0)
      }
      
    }
    
  }
  
  
  #scatter(result, resultb, "", "Pontos de Manutenção", "Pontos de Impacto")

}

cor_maintenance_vs_impact_concatData <- function(){

  mp <- load_concat_data("Cap5_IMPACTO.xlsx","d1:d20000")
  pip <- load_concat_data("Cap5_IMPACTO.xlsx","e1:e20000")
  ip <- load_concat_data("Cap5_IMPACTO.xlsx","f1:f20000")
  
  result <- c()
  resultb <- c()
  conta = 0
  contb = 0

  for (i in 1:length(mp)){
    if(mp[i] != 0 ){
      result <- c(result, mp[i])
      conta = conta + 1

      if(pip[i] == 0){
        resultb <- c(resultb, 0)
      }
      if(pip[i] != 0){
        resultb <- c(resultb, ip[i])
        if(ip[i] != 0){
          contb = contb + 1
        }
        
      }
      
      
    }
  }
  
  

  
  print(conta)
  print(contb)
  print(contb/conta)
  
  cliff.delta(result, resultb)
  
  return (cor.test(result,resultb, method = "spearman"))
}

cor_o2o_concatData <- function(){
  #Capitulo 4
  cap4_Fadd <- load_concat_data("CAP4_FineGrained.xlsx","g1:g20000")
  cap4_Frmv <- load_concat_data("CAP4_FineGrained.xlsx","i1:i20000")
  cap4_FRet <- load_concat_data("CAP4_FineGrained.xlsx","o1:o20000")
  cap4_FMod <- load_concat_data("CAP4_FineGrained.xlsx","p1:p20000")
  cap4_FSpe <- load_concat_data("CAP4_FineGrained.xlsx","q1:q20000")
  cap4_FQua <- load_concat_data("CAP4_FineGrained.xlsx","r1:r20000")
  cap4_FPar <- load_concat_data("CAP4_FineGrained.xlsx","s1:s20000")
  
  
  cap4_Vadd <- load_concat_data("CAP4_FineGrained.xlsx","h1:h20000")
  cap4_Vrmv <- load_concat_data("CAP4_FineGrained.xlsx","j1:j20000")
  cap4_VMod <- load_concat_data("CAP4_FineGrained.xlsx","k1:k20000")
  cap4_VSpe <- load_concat_data("CAP4_FineGrained.xlsx","l1:l20000")
  cap4_VQua <- load_concat_data("CAP4_FineGrained.xlsx","m1:m20000")
  cap4_Vtyp <- load_concat_data("CAP4_FineGrained.xlsx","n1:n20000")
  
  #Capitulo 5
  cap5_Fadd <- load_concat_data("Cap5_IMPACTO.xlsx","h1:h20000")
  cap5_Frmv <- load_concat_data("Cap5_IMPACTO.xlsx","i1:i20000")
  cap5_FRet <- load_concat_data("Cap5_IMPACTO.xlsx","k1:k20000")
  cap5_FMod <- load_concat_data("Cap5_IMPACTO.xlsx","l1:l20000")
  cap5_FSpe <- load_concat_data("Cap5_IMPACTO.xlsx","m1:m20000")
  cap5_FQua <- load_concat_data("Cap5_IMPACTO.xlsx","n1:n20000")
  cap5_FPar <- load_concat_data("Cap5_IMPACTO.xlsx","j1:j20000")
  
  
  cap5_Vadd <- load_concat_data("Cap5_IMPACTO.xlsx","p1:p20000")
  cap5_Vrmv <- load_concat_data("Cap5_IMPACTO.xlsx","q1:q20000")
  cap5_VMod <- load_concat_data("Cap5_IMPACTO.xlsx","s1:s20000")
  cap5_VSpe <- load_concat_data("Cap5_IMPACTO.xlsx","t1:t20000")
  cap5_VQua <- load_concat_data("Cap5_IMPACTO.xlsx","u1:u20000")
  cap5_Vtyp <- load_concat_data("Cap5_IMPACTO.xlsx","r1:r20000")



    d3 = data.frame(cap4_Fadd = cap4_Fadd, 
                    cap4_Frmv = cap4_Frmv,
                    cap4_FRet = cap4_FRet, 
                    cap4_FMod = cap4_FMod, 
                    cap4_FSpe = cap4_FSpe, 
                    cap4_FQua = cap4_FQua, 
                    cap4_FPar = cap4_FPar, 
                    cap4_Vadd = cap4_Vadd, 
                    cap4_Vrmv = cap4_Vrmv, 
                    cap4_VMod = cap4_VMod, 
                    cap4_VSpe = cap4_VSpe, 
                    cap4_VQua = cap4_VQua, 
                    cap4_Vtyp = cap4_Vtyp, 
                    cap5_Fadd = cap5_Fadd,
                    cap5_Frmv = cap5_Frmv, 
                    cap5_FRet = cap5_FRet, 
                    cap5_FMod = cap5_FMod, 
                    cap5_FSpe = cap5_FSpe, 
                    cap5_FQua = cap5_FQua, 
                    cap5_FPar = cap5_FPar, 
                    cap5_Vadd = cap5_Vadd, 
                    cap5_Vrmv = cap5_Vrmv, 
                    cap5_VMod = cap5_VMod, 
                    cap5_VSpe = cap5_VSpe, 
                    cap5_VQua = cap5_FQua, 
                    cap5_Vtyp = cap5_Vtyp)




    write.csv(d3, file = "concatData.csv",row.names=FALSE)
    
}


create_df_concatData <- function(){
  #Capitulo 4
  cap4_Fadd <- load_concat_data("CAP4_FineGrained.xlsx","g1:g20000")
  cap4_Frmv <- load_concat_data("CAP4_FineGrained.xlsx","i1:i20000")
  cap4_FRet <- load_concat_data("CAP4_FineGrained.xlsx","o1:o20000")
  cap4_FMod <- load_concat_data("CAP4_FineGrained.xlsx","p1:p20000")
  cap4_FSpe <- load_concat_data("CAP4_FineGrained.xlsx","q1:q20000")
  cap4_FQua <- load_concat_data("CAP4_FineGrained.xlsx","r1:r20000")
  cap4_FPar <- load_concat_data("CAP4_FineGrained.xlsx","s1:s20000")
  
  cap4_Vadd <- load_concat_data("CAP4_FineGrained.xlsx","h1:h20000")
  cap4_Vrmv <- load_concat_data("CAP4_FineGrained.xlsx","j1:j20000")
  cap4_VMod <- load_concat_data("CAP4_FineGrained.xlsx","k1:k20000")
  cap4_VSpe <- load_concat_data("CAP4_FineGrained.xlsx","l1:l20000")
  cap4_VQua <- load_concat_data("CAP4_FineGrained.xlsx","m1:m20000")
  cap4_Vtyp <- load_concat_data("CAP4_FineGrained.xlsx","n1:n20000")
  
  #Capitulo 5
  impact_point <- load_concat_data("Cap5_IMPACTO.xlsx","f1:f20000")
  d3 = data.frame(cap4_Fadd = cap4_Fadd, 
                  cap4_Frmv = cap4_Frmv,
                  cap4_FRet = cap4_FRet, 
                  cap4_FMod = cap4_FMod, 
                  cap4_FSpe = cap4_FSpe, 
                  cap4_FQua = cap4_FQua, 
                  cap4_FPar = cap4_FPar, 
                  cap4_Vadd = cap4_Vadd, 
                  cap4_Vrmv = cap4_Vrmv, 
                  cap4_VMod = cap4_VMod, 
                  cap4_VSpe = cap4_VSpe, 
                  cap4_VQua = cap4_VQua, 
                  cap4_Vtyp = cap4_Vtyp, 
                  impact_point = impact_point)
  
  make_test(d3)
  
}


regression_concatData <- function(){
  
  #Capitulo 4
  deps <- load_concat_data("CAP4_FineGrained.xlsx","x1:x20000")
  mp <- load_concat_data("CAP4_FineGrained.xlsx","y1:y20000")
  
  #Capitulo 5
  pip <- load_concat_data("Cap5_IMPACTO.xlsx","e1:e20000")
  ip <- load_concat_data("Cap5_IMPACTO.xlsx","f1:f20000")
  

  
  
  rmp <- c()
  rip <- c()
  
  
  for (i in 1:length(mp)){
    if(mp[i] != 0 ){
      rmp <- c(rmp, mp[i]/deps[i])
      
      if(pip[i] != 0){
        rip <- c(rip, ip[i]/pip[i])
      }
      if(pip[i] == 0){
        rip <- c(rip, 0)
      }
      
    }
    
  }
  n <- nls (rip ~ th0*rmp / (th1+rmp), start = list(th0=13,th1=21))
  print(summary(n))
  plot(rmp,rip, 
       xlab="Pontos de Manuten?o", ylab="Pontos de Impacto", pch=16)
  lines(rmp, predict(n), col = c("blue"))
  
  
}

mp_vs_ip  <- function(){
  #Capitulo 4
  functions <- load_concat_data("CAP4_FineGrained.xlsx","d1:d20000")
  variables <- load_concat_data("CAP4_FineGrained.xlsx","f1:f20000")
  dep <- load_concat_data("CAP4_FineGrained.xlsx","x1:x20000")
  depc <- load_concat_data("CAP4_FineGrained.xlsx","y1:y20000")

  #Capitulo 5
  i_p <- load_concat_data("Cap4_FineGrained.xlsx","z1:z20000")
  
  maintenance_pointsnot <-c()
  maintenance_points <-c()
  impact_points<-c()
  
  fn <- c()
  vn <- c()
  
  f <- c()
  v <- c()
  
  depsnot <-c()
  deps <-c()
  
  a= 0
  b=0
  for (i in 1:length(functions)){
    if(functions[i]+variables[i] > 0){
      if(i_p[i] == 0  && depc[i] > 0){
        maintenance_pointsnot <- c(maintenance_pointsnot, (functions[i]+variables[i])/depc[i])
        f <- c(f, (functions[i]/depc[i]))
        v <- c(v, (variables[i]/depc[i]))
        
        if( ( (functions[i]+variables[i])/depc[i])  < 2 ){
          a = a+1
        }
        
        if(((functions[i]+variables[i])/depc[i])  > 2){
          b = b+1
        }
        #impact_points<-c(impact_points, i_p[i])
        #depsnot <-c(depsnot, dep[i])
        
      }
      
      if(i_p[i] > 0 && depc[i] >0){
        maintenance_points <- c(maintenance_points, ((functions[i]+variables[i])/depc[i]))
        fn <- c(fn, (functions[i]/depc[i]))
        vn <- c(vn, (variables[i]/depc[i]))
       
        #impact_points<-c(impact_points, i_p[i])
        #deps <-c(deps, dep[i])
        
      }

    }
    
    
  }

  #hist(maintenance_points, density=80, breaks=100);
  
  
  
  #boxplot(maintenance_pointsnot, maintenance_points, outline = FALSE, names=c("Não Impacto","Impacto"),col="grey90")
  
  boxplot(f,fn, outline = FALSE, names=c("N? Impacto","Impacto"),col="grey90")
  
  #mp_vs_ip = data.frame(maintenance_points = maintenance_points, impact_points = impact_points)
  #write.csv(mp_vs_ip, file = "mp_vs_ip_not.csv",row.names=FALSE)
  #print(maintenance_points)

  #with(mp_vs_ip, scatter.smooth(maintenance_points, impact_points))
  ## or with dotted thick smoothed line results :
  #with(mp_vs_ip, scatter.smooth(maintenance_points, impact_points, lpars =list(col = "red", lwd = 1, lty = 1)))
  
  
  nimpacto <- summary(maintenance_pointsnot)
  impacto <- summary(maintenance_points)
  
  
  
  # Definition of vectors
  Sistema <- c("N? Impacto", "Impacto")
  Media <- c(nimpacto[4],impacto[4])
  Mediana <- c(nimpacto[3],impacto[3])
  Min <- c(nimpacto[1],impacto[1])
  Max <- c(nimpacto[6],impacto[6])
  Desvio <- c(sd(maintenance_pointsnot),sd(maintenance_points))
  
  # Create a data frame from the vectors
  result_df <- data.frame(Sistema, Media, Mediana, Min, Max, Desvio)
  print( result_df)

  
  #scatter(maintenance_points, impact_points, "", "Pontos de Manutenção","Pontos de Impacto" )
  
  #boxplot(maintenance_points, impact_points, names=c("Pontos de Manuten??o","Pontos de Impacto"),col="grey")
  
}


#cor_o2o_concatData()

#cor_maintenance_vs_impact_concatData()

#scatter_maintenance_vs_impact_concatData()

#create_df_concatData()

#regression_concatData()

#mp_vs_ip()
  
  
  # I divide the second screen in 2 columns :
  my_screen_step2=split.screen(c(1, 2))
  screen(my_screen_step2[1])
  hist(maintenance_points, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="" , xlab="Impacto", ylab="", breaks=600)
  screen(my_screen_step2[2])
  hist(maintenance_pointsnot, border=F , col=rgb(0.8,0.2,0.8,0.7) , main="" ,  xlab="N? Impacto", ylab="",breaks=600)



