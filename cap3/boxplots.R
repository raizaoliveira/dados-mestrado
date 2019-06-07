library(readxl)

load <- function(arq,ranges, l)
{
  data_column <- read_excel(arq, sheet=l,na = "", range = (ranges))
  return ( na.omit(data_column[[1]]))
  
}



lighttpdrdc <- data.frame(value = load("Book1.xlsx","a1:a4000", "rdc"), variable = "Lighttpd")
curlrdc <- data.frame(value = load("Book1.xlsx","b1:b20000", "rdc"), variable = "Curl")
gziprdc <- data.frame(value = load("Book1.xlsx","c1:c20000", "rdc"), variable = "Gzip")
openvpnrdc <- data.frame(value = load("Book1.xlsx","d1:d20000", "rdc"), variable = "OpenVPN")
hexchatrdc <- data.frame(value = load("Book1.xlsx","e1:e20000", "rdc"), variable = "Hexchat")
libsshrdc <- data.frame(value = load("Book1.xlsx","f1:f20000", "rdc"), variable = "LibSSH")


rdc <- rbind(lighttpdrdc, curlrdc, gziprdc, openvpnrdc, hexchatrdc, libsshrdc)
head(rdc)


lighttpdrdm <- data.frame(value = load("Book1.xlsx","a1:a4000", "rdm"), variable = "Lighttpd")
curlrdm <- data.frame(value = load("Book1.xlsx","b1:b20000", "rdm"), variable = "Curl")
gziprdm <- data.frame(value = load("Book1.xlsx","c1:c20000", "rdm"), variable = "Gzip")
openvpnrdm <- data.frame(value = load("Book1.xlsx","d1:d20000", "rdm"), variable = "OpenVPN")
hexchatrdm <- data.frame(value = load("Book1.xlsx","e1:e20000", "rdm"), variable = "Hexchat")
libsshrdm <- data.frame(value = load("Book1.xlsx","f1:f20000", "rdm"), variable = "LibSSH")


rdm <- rbind(lighttpdrdm, curlrdm, gziprdm, openvpnrdm, hexchatrdm, libsshrdm)
head(rdm)


lighttpdcdc <- data.frame(value = load("Book1.xlsx","a1:a4000", "cdc"), variable = "Lighttpd")
curlcdc <- data.frame(value = load("Book1.xlsx","b1:b20000", "cdc"), variable = "Curl")
gzipcdc <- data.frame(value = load("Book1.xlsx","c1:c20000", "cdc"), variable = "Gzip")
openvpncdc <- data.frame(value = load("Book1.xlsx","d1:d20000", "cdc"), variable = "OpenVPN")
hexchatcdc <- data.frame(value = load("Book1.xlsx","e1:e20000", "cdc"), variable = "Hexchat")
libsshcdc <- data.frame(value = load("Book1.xlsx","f1:f20000", "cdc"), variable = "LibSSH")


cdc <- rbind(lighttpdcdc, curlcdc, gzipcdc, openvpncdc, hexchatcdc, libsshcdc)
head(cdc)


lighttpdcdm <- data.frame(value = load("Book1.xlsx","a1:a4000", "cdm"), variable = "Lighttpd")
curlcdm <- data.frame(value = load("Book1.xlsx","b1:b20000", "cdm"), variable = "Curl")
gzipcdm <- data.frame(value = load("Book1.xlsx","c1:c20000", "cdm"), variable = "Gzip")
openvpncdm <- data.frame(value = load("Book1.xlsx","d1:d20000", "cdm"), variable = "OpenVPN")
hexchatcdm <- data.frame(value = load("Book1.xlsx","e1:e20000", "cdm"), variable = "Hexchat")
libsshcdm <- data.frame(value = load("Book1.xlsx","f1:f20000", "cdm"), variable = "LibSSH")


cdm <- rbind(lighttpdcdm, curlcdm, gzipcdm, openvpncdm, hexchatcdm, libsshcdm)
head(cdm)




library(ggplot2)


p <-  ggplot(cdm, aes(x = variable, y = value, color=value)) +
  geom_violin(scale = "width", adjust = 1, width = 0.5, fill='#A4A4A4', color="darkred", outlier.shape = NA)  + theme(axis.text.x = element_text(face="plain", color="#000000", 
                                                                                                                             size=10, angle=35, vjust=0.8), axis.text.y = element_text(size=12))  + 
  stat_summary(fun.y=mean, geom="point", shape=17, size=1,color="red") +scale_color_manual(values=c("red", "blue", "green"))+ geom_boxplot(width=0.1) + theme_minimal()


p<-p+labs(x="", y="", title=" ")
p

lhcdc <- summary(lighttpdrdc$value)
cucdc <- summary(curlrdc$value)
gzcdc <- summary(gziprdc$value) 
opcdc <- summary(openvpnrdc$value)
hecdc <- summary(hexchatrdc$value) 
lscdc <- summary(libsshrdc$value)


# Definition of vectors
Sistema <- c("Lighttpd", "Curl", "Gzip", "Openvpn", "Hexchat", "Libssh")
Media <- c(lhcdc[4],cucdc[4], gzcdc[4], opcdc[4],hecdc[4], lscdc[4])
Mediana <- c(lhcdc[3],cucdc[3], gzcdc[3], opcdc[3],hecdc[3], lscdc[3]) 
Min <- c(lhcdc[1],cucdc[1], gzcdc[1], opcdc[1],hecdc[1], lscdc[1]) 
Max <- c(lhcdc[6],cucdc[6], gzcdc[6], opcdc[6],hecdc[6], lscdc[6]) 
Desvio <- c(sd(lhcdc),sd(cucdc), sd(gzcdc), sd(opcdc), sd(hecdc),sd(lscdc))

# Create a data frame from the vectors
impactPoints_df <- data.frame(Sistema, Media, Mediana, Min, Max, Desvio)
print(impactPoints_df)
write.csv(impactPoints_df, file = "SUMMARYrdc.csv",row.names=FALSE)


