library(readxl)

load <- function(arq,ranges, l)
{
  data_column <- read_excel(arq, sheet=l,na = "", range = (ranges))
  return ( na.omit(data_column[[1]]))
  
}


calc_vip <- function(ip, vip){
  ipp <- c()
  for (i in 1:length(vip)){
    if(vip[i] != 0 && ip[i] != 0){


      ipp <- c(ipp, ip[i]/vip[i])
    }
  }
  print(ipp)
  return(ipp)
}



library(vioplot)
ipa <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Vim")
ipb <- load("Cap5_IMPACTO.xlsx","v1:v20000", "PaceMaker")
ipc <- load("Cap5_IMPACTO.xlsx","v1:v20000", "LibXML")
ipd <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Lighttpd")
ipe <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Collectd")
ipf <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Syslog")
ipg <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Ccache")
iph <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Gawk")
ipi <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Gzip")
ipj <- load("Cap5_IMPACTO.xlsx","v1:v20000", "LibNFC")
ipk <- load("Cap5_IMPACTO.xlsx","v1:v20000", "LibSSH")
ipl <- load("Cap5_IMPACTO.xlsx","v1:v20000", "librskafka")
ipm <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Mosquitto")
ipn <- load("Cap5_IMPACTO.xlsx","v1:v20000", "MPSolve")
ipo <- load("Cap5_IMPACTO.xlsx","v1:v20000", "OpenVPN")
ipq <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Ossec")
ipr <- load("Cap5_IMPACTO.xlsx","v1:v20000", "PianoBar")
ips <- load("Cap5_IMPACTO.xlsx","v1:v20000", "SilverSearch")
ipt <- load("Cap5_IMPACTO.xlsx","v1:v20000", "GNUPlot")
ipu <- load("Cap5_IMPACTO.xlsx","v1:v20000", "OpenSC")
ipv <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Sylpheed")
ipx <- load("Cap5_IMPACTO.xlsx","v1:v20000", "curl")
ipz <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Gnuchess")
ipy <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Totem")
ipw <- load("Cap5_IMPACTO.xlsx","v1:v20000", "m4")
ipaa <- load("Cap5_IMPACTO.xlsx","v1:v20000", "dia")
ipab <- load("Cap5_IMPACTO.xlsx","v1:v20000", "mapserver")
ipac <- load("Cap5_IMPACTO.xlsx","v1:v20000", "ethersex")
ipad <- load("Cap5_IMPACTO.xlsx","v1:v20000", "uwsgi")
ipae <- load("Cap5_IMPACTO.xlsx","v1:v20000", "Hexchat")


vpa <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Vim")
vpb <- load("Cap5_IMPACTO.xlsx","f1:f20000", "PaceMaker")
vpc <- load("Cap5_IMPACTO.xlsx","f1:f20000", "LibXML")
vpd <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Lighttpd")
vpe <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Collectd")
vpf <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Syslog")
vpg <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Ccache")
vph <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Gawk")
vpi <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Gzip")
vpj <- load("Cap5_IMPACTO.xlsx","f1:f20000", "LibNFC")
vpk <- load("Cap5_IMPACTO.xlsx","f1:f20000", "LibSSH")
vpl <- load("Cap5_IMPACTO.xlsx","f1:f20000", "librskafka")
vpm <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Mosquitto")
vpn <- load("Cap5_IMPACTO.xlsx","f1:f20000", "MPSolve")
vpo <- load("Cap5_IMPACTO.xlsx","f1:f20000", "OpenVPN")
vpq <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Ossec")
vpr <- load("Cap5_IMPACTO.xlsx","f1:f20000", "PianoBar")
vps <- load("Cap5_IMPACTO.xlsx","f1:f20000", "SilverSearch")
vpt <- load("Cap5_IMPACTO.xlsx","f1:f20000", "GNUPlot")
vpu <- load("Cap5_IMPACTO.xlsx","f1:f20000", "OpenSC")
vpv <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Sylpheed")
vpx <- load("Cap5_IMPACTO.xlsx","f1:f20000", "curl")
vpz <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Gnuchess")
vpy <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Totem")
vpw <- load("Cap5_IMPACTO.xlsx","f1:f20000", "m4")
vpaa <- load("Cap5_IMPACTO.xlsx","f1:f20000", "dia")
vpab <- load("Cap5_IMPACTO.xlsx","f1:f20000", "mapserver")
vpac <- load("Cap5_IMPACTO.xlsx","f1:f20000", "ethersex")
vpad <- load("Cap5_IMPACTO.xlsx","f1:f20000", "uwsgi")
vpae <- load("Cap5_IMPACTO.xlsx","f1:f20000", "Hexchat")

ip_a <- calc_vip(ipa, vpa)
ip_b <- calc_vip(ipb, vpb)
ip_c <- calc_vip(ipc, vpc)
ip_d <- calc_vip(ipd, vpd)
ip_e <- calc_vip(ipe, vpe)
ip_f <- calc_vip(ipf, vpf)
ip_g <- calc_vip(ipg, vpg)
ip_h <- calc_vip(iph, vph)
ip_i <- calc_vip(ipi, vpi)
ip_j <- calc_vip(ipj, vpj)
ip_k <- calc_vip(ipk, vpk)
ip_l <- calc_vip(ipl, vpl)
ip_m <- calc_vip(ipm, vpm)
ip_n <- calc_vip(ipn, vpn)
ip_o <- calc_vip(ipo, vpo)
ip_q <- calc_vip(ipq, vpq)
ip_r <- calc_vip(ipr, vpr)
ip_s <- calc_vip(ips, vps)
ip_t <- calc_vip(ipt, vpt)
ip_u <- calc_vip(ipu, vpu)
ip_v <- calc_vip(ipv, vpv)
ip_x <- calc_vip(ipx, vpx)
ip_z <- calc_vip(ipz, vpz)
ip_y <- calc_vip(ipy, vpy)
ip_w <- calc_vip(ipw, vpw)
ip_aa <- calc_vip(ipaa, vpaa)
ip_ab <- calc_vip(ipab, vpab)
ip_ac <- calc_vip(ipac, vpac)
ip_ad <- calc_vip(ipad, vpad)
ip_ae <- calc_vip(ipae, vpae)









  Vim = summary(ip_a)
  PaceMaker = summary(ip_b)
  LibXML = summary(ip_c)
  Lighttpd = summary(ip_d)
  Collectd = summary(ip_e)
  Syslog = summary(ip_f)
  Ccache = summary(ip_g)
  Gawk = summary(ip_h)
  Gzip = summary(ip_i)
  LibNFC = summary(ip_j)
  LibSSH = summary(ip_k)
  librskafka = summary(ip_l)
  Mosquitto = summary(ip_m)
  MPSolve = summary(ip_n)
  OpenVPN = summary(ip_o)
  Ossec = summary(ip_q)
  PianoBar = summary(ip_r)
  SilverSearch = summary(ip_s)
  GNUPlot = summary(ip_t)
  OpenSC = summary(ip_u)
  Sylpheed = summary(ip_v)
  Curl = summary(ip_x)
  Gnuchess = summary(ip_z)
  Totem = summary(ip_y)
  M4 = summary(ip_w)
  Dia = summary(ip_aa)
  MapServer = summary(ip_ab)
  Ethersex = summary(ip_ac)
  Uwsgi = summary(ip_ad)
  Hexchat = summary(ip_ae)




# Definition of vectors
Sistema <- c("Vim", "PaceMaker", "LibXML", "Lighttpd", "Collectd", "Syslog", "Ccache", "Gawk", "Gzip","LibNFC","LibSSH","librskafka","Mosquitto","MPSolve","OpenVPN","Ossec","PianoBar","SilverSearch","GNUPlot","OpenSC", "Sylpheed ","Curl" ,"Gnuchess" ,"Totem" ,"M4" ,"Dia" ,"MapServer","Ethersex" ,"Uwsgi" ,"Hexchat")
Media <- c(Vim[4],PaceMaker[4], LibXML[4], Lighttpd[4], Collectd[4], Syslog[4], Ccache[4], Gawk[4], Gzip[4],LibNFC[4],LibSSH[4],librskafka[4],Mosquitto[4],MPSolve[4],OpenVPN[4],Ossec[4],PianoBar[4],SilverSearch[4],GNUPlot[4],OpenSC[4],Sylpheed[4],Curl[4] ,Gnuchess[4] ,Totem[4] ,M4[4] ,Dia[4] ,MapServer[4],Ethersex[4] ,Uwsgi[4] ,Hexchat[4])
Mediana <- c(Vim[3],PaceMaker[3], LibXML[3], Lighttpd[3], Collectd[3], Syslog[3], Ccache[3], Gawk[3], Gzip[3],LibNFC[3],LibSSH[3],librskafka[3],Mosquitto[3],MPSolve[3],OpenVPN[3],Ossec[3],PianoBar[3],SilverSearch[3],GNUPlot[3],OpenSC[3],Sylpheed[3],Curl[3] ,Gnuchess[3] ,Totem[3] ,M4[3] ,Dia[3] ,MapServer[3],Ethersex[3] ,Uwsgi[3] ,Hexchat[3])
Min <- c(Vim[1],PaceMaker[1], LibXML[1], Lighttpd[1], Collectd[1], Syslog[1], Ccache[1], Gawk[1], Gzip[1],LibNFC[1],LibSSH[1],librskafka[1],Mosquitto[1],MPSolve[1],OpenVPN[1],Ossec[1],PianoBar[1],SilverSearch[1],GNUPlot[1],OpenSC[1],Sylpheed[1],Curl[1] ,Gnuchess[1] ,Totem[1] ,M4[1] ,Dia[1] ,MapServer[1],Ethersex[1] ,Uwsgi[1] ,Hexchat[1])
Max <- c(Vim[6],PaceMaker[6], LibXML[6], Lighttpd[6], Collectd[6], Syslog[6], Ccache[6], Gawk[6], Gzip[6],LibNFC[6],LibSSH[6],librskafka[6],Mosquitto[6],MPSolve[6],OpenVPN[6],Ossec[6],PianoBar[6],SilverSearch[6],GNUPlot[6],OpenSC[6],Sylpheed[6],Curl[6] ,Gnuchess[6] ,Totem[6] ,M4[6] ,Dia[6] ,MapServer[6],Ethersex[6] ,Uwsgi[6] ,Hexchat[6])
Desvio <- c(sd(ipa),sd(ipb), sd(ipc), sd(ipd), sd(ipe),sd(ipf),sd(ipg),sd(iph),sd(ipi),sd(ipj),sd(ipk),sd(ipl),sd(ipm),sd(ipn),sd(ipo),sd(ipq),sd(ipr),sd(ips),sd(ipt),sd(ipu),sd(ipv),sd(ipx),sd(ipz),sd(ipy),sd(ipw),sd(ipaa),sd(ipab),sd(ipac),sd(ipad),sd(ipae))

# Create a data frame from the vectors
impactPoints_df <- data.frame(Sistema, Media, Mediana, Min, Max, Desvio)
print(impactPoints_df)
write.csv(impactPoints_df, file = "SUMMARYIP.csv",row.names=FALSE)


  Vim = summary(vpa)
  PaceMaker = summary(vpb)
  LibXML = summary(vpc)
  Lighttpd = summary(vpd)
  Collectd = summary(vpe)
  Syslog = summary(vpf)
  Ccache = summary(vpg)
  Gawk = summary(vph)
  Gzip = summary(vpi)
  LibNFC = summary(vpj)
  LibSSH = summary(vpk)
  librskafka = summary(vpl)
  Mosquitto = summary(vpm)
  MPSolve = summary(vpn)
  OpenVPN = summary(vpo)
  Ossec = summary(vpq)
  PianoBar = summary(vpr)
  SilverSearch = summary(vps)
  GNUPlot = summary(vpt)
  OpenSC = summary(vpu)
  Sylpheed = summary(vpv)
  Curl = summary(vpx)
  Gnuchess = summary(vpz)
  Totem = summary(vpy)
  M4 = summary(vpw)
  Dia = summary(vpaa)
  MapServer = summary(vpab)
  Ethersex = summary(vpac)
  Uwsgi = summary(vpad)
  Hexchat = summary(vpae)




# Definition of vectors
Sistema <- c("Vim", "PaceMaker", "LibXML", "Lighttpd", "Collectd", "Syslog", "Ccache", "Gawk", "Gzip","LibNFC","LibSSH","librskafka","Mosquitto","MPSolve","OpenVPN","Ossec","PianoBar","SilverSearch","GNUPlot","OpenSC", "Sylpheed ","Curl" ,"Gnuchess" ,"Totem" ,"M4" ,"Dia" ,"MapServer","Ethersex" ,"Uwsgi" ,"Hexchat")
Media <- c(Vim[4],PaceMaker[4], LibXML[4], Lighttpd[4], Collectd[4], Syslog[4], Ccache[4], Gawk[4], Gzip[4],LibNFC[4],LibSSH[4],librskafka[4],Mosquitto[4],MPSolve[4],OpenVPN[4],Ossec[4],PianoBar[4],SilverSearch[4],GNUPlot[4],OpenSC[4],Sylpheed[4],Curl[4] ,Gnuchess[4] ,Totem[4] ,M4[4] ,Dia[4] ,MapServer[4],Ethersex[4] ,Uwsgi[4] ,Hexchat[4])
Mediana <- c(Vim[3],PaceMaker[3], LibXML[3], Lighttpd[3], Collectd[3], Syslog[3], Ccache[3], Gawk[3], Gzip[3],LibNFC[3],LibSSH[3],librskafka[3],Mosquitto[3],MPSolve[3],OpenVPN[3],Ossec[3],PianoBar[3],SilverSearch[3],GNUPlot[3],OpenSC[3],Sylpheed[3],Curl[3] ,Gnuchess[3] ,Totem[3] ,M4[3] ,Dia[3] ,MapServer[3],Ethersex[3] ,Uwsgi[3] ,Hexchat[3])
Min <- c(Vim[1],PaceMaker[1], LibXML[1], Lighttpd[1], Collectd[1], Syslog[1], Ccache[1], Gawk[1], Gzip[1],LibNFC[1],LibSSH[1],librskafka[1],Mosquitto[1],MPSolve[1],OpenVPN[1],Ossec[1],PianoBar[1],SilverSearch[1],GNUPlot[1],OpenSC[1],Sylpheed[1],Curl[1] ,Gnuchess[1] ,Totem[1] ,M4[1] ,Dia[1] ,MapServer[1],Ethersex[1] ,Uwsgi[1] ,Hexchat[1])
Max <- c(Vim[6],PaceMaker[6], LibXML[6], Lighttpd[6], Collectd[6], Syslog[6], Ccache[6], Gawk[6], Gzip[6],LibNFC[6],LibSSH[6],librskafka[6],Mosquitto[6],MPSolve[6],OpenVPN[6],Ossec[6],PianoBar[6],SilverSearch[6],GNUPlot[6],OpenSC[6],Sylpheed[6],Curl[6] ,Gnuchess[6] ,Totem[6] ,M4[6] ,Dia[6] ,MapServer[6],Ethersex[6] ,Uwsgi[6] ,Hexchat[6])
Desvio <- c(sd(vpa),sd(vpb), sd(vpc), sd(vpd), sd(vpe),sd(vpf),sd(vpg),sd(vph),sd(vpi),sd(vpj),sd(vpk),sd(vpl),sd(vpm),sd(vpn),sd(vpo),sd(vpq),sd(vpr),sd(vps),sd(vpt),sd(vpu),sd(vpv),sd(vpx),sd(vpz),sd(vpy),sd(vpw),sd(vpaa),sd(vpab),sd(vpac),sd(vpad),sd(vpae))

# Create a data frame from the vectors
impactVariabilities_df <- data.frame(Sistema, Media, Mediana, Min, Max, Desvio)
print(impactVariabilities_df)
write.csv(impactVariabilities_df, file = "SUMMARYVAR.csv",row.names=FALSE)

library(ggplot2)

C1 <- data.frame(value = ip_a, variable = "Vim")
C2 <- data.frame(value = ip_b, variable = "PaceMaker")
C3 <- data.frame(value = ip_c, variable = "LibXML")
C4 <- data.frame(value = ip_d, variable = "Lighttpd")
C5 <- data.frame(value = ip_e, variable = "Collectd")
C6 <- data.frame(value = ip_f, variable = "Syslog")
C7 <- data.frame(value = ip_g, variable = "CCache")
C8 <- data.frame(value = ip_h, variable = "Gawk")
C9 <- data.frame(value = ip_i, variable = "Gzip")
C10 <- data.frame(value = ip_j, variable = "LibNFC")
C11 <- data.frame(value = ip_k, variable = "LibSSH")
C12 <- data.frame(value = ip_l, variable = "LibrsKakfka")
C13 <- data.frame(value = ip_m, variable = "Mosquitto")
C14 <- data.frame(value = ip_n, variable = "MPSolve")
C15 <- data.frame(value = ip_o, variable = "OpenVPN")
C16 <- data.frame(value = ip_q, variable = "Ossec")
C17 <- data.frame(value = ip_r, variable = "PianoBar")
C18 <- data.frame(value = ip_s, variable = "SilverSearcher")
C19 <- data.frame(value = ip_t, variable = "GNUPlot")
C20 <- data.frame(value = ip_u, variable = "OpenSC")




dat <- rbind(C1 ,C2 ,C3 ,C4 ,C5 ,C6 ,C7 ,C8 ,C9 ,C10)
head(dat)

p <-  ggplot(dat, aes(x = variable, y = value, color=value)) +
  geom_violin(scale = "width", adjust = 1, width = 0.5, fill='#A4A4A4', color="darkred")  + theme(axis.text.x = element_text(face="plain", color="#000000", 
              size=10, angle=35, vjust=0.8), axis.text.y = element_text(size=12))  + 
              stat_summary(fun.y=mean, geom="point", shape=17, size=1,color="red") +scale_color_manual(values=c("red", "blue", "green"))+ geom_boxplot(width=0.1) + theme_minimal()

                                                                                                                                                                                                                                                                                               
 p<-p+labs(x="", y="", title=" ")
p


