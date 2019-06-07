library(readxl)

load_concat_data <- function(arq,ranges){
  result <- c()
  
  for (l in 1:length( excel_sheets(arq))){
    
    data_column <- read_excel(arq, sheet=l,na = "", range = (ranges))
    
    result <- c(result,na.omit(data_column[[1]]))
    
  }
  
  return ( result)
  
}



calc_pechanged <- function(pet, pec){
  ipp <- c()
  for (i in 1:length(pet)){
    if(pet[i] != 0){
      ipp <- c(ipp, pec[i]/pet[i])
    }
  }
  
  return(ipp)
}


remove_zero <- function(data){
  ip <- c()
  cont = 0
  print(length(data))
  for (i in 1:length(data)){
    if(data[i] != 0){
      ip <- c(ip, data[i])
      cont <- cont+1
    }
  }
  print(cont)
  return(ip)
}


#f/e == variavel
#d/c == funcao


func_var_wilcoxon <- function(){

  varc <- load_concat_data("CAP4_FineGrained.xlsx","f1:f20000")
  vart <- load_concat_data("CAP4_FineGrained.xlsx","e1:e20000")

  funcc <- load_concat_data("CAP4_FineGrained.xlsx","d1:d20000")
  funct <- load_concat_data("CAP4_FineGrained.xlsx","c1:c20000")


  func <- calc_pechanged (funct, funcc)
  var <- calc_pechanged(vart, varc)


  wilcox.test(func, var) 

  cliff.delta(var, func)
}

cat_kruskal <- function(){

  Fadd <- load_concat_data("CAP4_FineGrained.xlsx","g1:g20000")
  Frmv <- load_concat_data("CAP4_FineGrained.xlsx","i1:i20000")
  FRet <- load_concat_data("CAP4_FineGrained.xlsx","o1:o20000")
  FMod <- load_concat_data("CAP4_FineGrained.xlsx","p1:p20000")
  FSpe <- load_concat_data("CAP4_FineGrained.xlsx","q1:q20000")
  FQua <- load_concat_data("CAP4_FineGrained.xlsx","r1:r20000")
  FPar <- load_concat_data("CAP4_FineGrained.xlsx","s1:s20000")
  
  
  Vadd <- load_concat_data("CAP4_FineGrained.xlsx","h1:h20000")
  Vrmv <- load_concat_data("CAP4_FineGrained.xlsx","j1:j20000")
  VMod <- load_concat_data("CAP4_FineGrained.xlsx","k1:k20000")
  VSpe <- load_concat_data("CAP4_FineGrained.xlsx","l1:l20000")
  VQua <- load_concat_data("CAP4_FineGrained.xlsx","m1:m20000")
  Vtyp <- load_concat_data("CAP4_FineGrained.xlsx","n1:n20000")



  kruskal.test(Vadd, Vrmv, VMod, VSpe, VQua, Vtyp)
  kruskal.test(Fadd, Frmv, FMod, FSpe, FQua, FRet, FPar)
  #cliff.delta(var, func)
}


wilcox.test(Fadd, Frmv)











