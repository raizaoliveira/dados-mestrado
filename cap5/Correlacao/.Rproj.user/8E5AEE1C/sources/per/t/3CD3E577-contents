

make_test <- function(df)
{
	#df <-  read.csv(arq, na.strings = c("","NA"), header=TRUE)

	Function_Addition = cor.test(df$cap4_Fadd,df$impact_point, method = "spearman")
	Function_Exclusion = cor.test(df$cap4_Frmv,df$impact_point, method = "spearman")
	Function_Return_Type = cor.test(df$cap4_FRet,df$impact_point, method = "spearman")
	Function_Modifier = cor.test(df$cap4_FMod,df$impact_point, method = "spearman")
	Function_Specifier = cor.test(df$cap4_FSpe,df$impact_point, method = "spearman")
	Function_Qualifier = cor.test(df$cap4_FQua,df$impact_point, method = "spearman")
	Function_Parameter = cor.test(df$cap4_FPar,df$impact_point, method = "spearman")


	Variable_Addition = cor.test(df$cap4_Vadd,df$impact_point, method = "spearman")
	Variable_Exclusion = cor.test(df$cap4_Vrmv,df$impact_point, method = "spearman")
	Variable_Modifier = cor.test(df$cap4_VMod,df$impact_point, method = "spearman")
	Variable_Specifier = cor.test(df$cap4_VSpe,df$impact_point, method = "spearman")
	Variable_Qualifier = cor.test(df$cap4_VQua,df$impact_point, method = "spearman")
	Variable_Type = cor.test(df$cap4_Vtyp,df$impact_point, method = "spearman")


	 print(Variable_Specifier$rho)

  	df_corr = data.frame(
		Function_Addition = Function_Addition$p.value,
		Function_Exclusion = Function_Exclusion$p.value,
		Function_Return_Type = Function_Return_Type$p.value,
		Function_Modifier = Function_Modifier$p.value,
		Function_Specifier = Function_Specifier$p.value,
		Function_Qualifier = Function_Qualifier$p.value,
		Function_Parameter = Function_Parameter$p.value,

		Variable_Addition = Variable_Addition$p.value,
		Variable_Exclusion = Variable_Exclusion$p.value,
		Variable_Modifier = Variable_Modifier$p.value,
		Variable_Specifier = Variable_Specifier$p.value,
		Variable_Qualifier = Variable_Qualifier$p.value,
		Variable_Type = Variable_Type$p.value
	)
  	
  	df_rh0 = data.frame(
  	  Function_Addition = Function_Addition$rho,
  	  Function_Exclusion = Function_Exclusion$rho,
  	  Function_Return_Type = Function_Return_Type$rho,
  	  Function_Modifier = Function_Modifier$rho,
  	  Function_Specifier = Function_Specifier$rho,
  	  Function_Qualifier = Function_Qualifier$rho,
  	  Function_Parameter = Function_Parameter$rho,
  	  
  	  Variable_Addition = Variable_Addition$rho,
  	  Variable_Exclusion = Variable_Exclusion$rho,
  	  Variable_Modifier = Variable_Modifier$rho,
  	  Variable_Specifier = Variable_Specifier$rho,
  	  Variable_Qualifier = Variable_Qualifier$rho,
  	  Variable_Type = Variable_Type$rho
  	)

  

	write.csv(df_corr, file = "Results-PointImpacts.csv",row.names=FALSE)
	write.csv(df_rh0, file = "Results-PointImpactsRH0.csv",row.names=FALSE)

}

scatter <- function(x, y, tittle, x_lab, y_lab){

	plot(x, y, main=tittle, 
   xlab=x_lab, ylab=y_lab, pch=16)


}






