

library(readxl)
#[c] variabilities
#[d] dependencies



load <- function(arq,ranges, l)
{
  data_column <- read_excel(arq, sheet=l,na = "", range = (ranges))
  return ( na.omit(data_column[[1]]))
  
}

delta <-function(dat, h){

	fre  <- c()
	last = 1
	for (i in 1:length(dat)) {
		if (i > 1){
			if(abs(dat[i]-dat[i-1]) > 0){
			  fre <-c(fre, abs(difftime(h[i],h[last],tz="GMT",units="hours")))
			  last = i
			}
			
		}
	}
	return(na.omit(fre))

}


#load Variabilities
vimv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Vim"),load("Cap4-Coarse.xlsx","a1:a20000", "Vim"))
pacemakerv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "PaceMaker"),load("Cap4-Coarse.xlsx","a1:a20000", "PaceMaker"))
lixmlv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Libxml2"),load("Cap4-Coarse.xlsx","a1:a20000", "Libxml2"))
Lighttpdv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Lighttpd"),load("Cap4-Coarse.xlsx","a1:a20000", "Lighttpd"))
Collectdv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Collected"),load("Cap4-Coarse.xlsx","a1:a20000", "Collected"))
Syslogv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Syslog"),load("Cap4-Coarse.xlsx","a1:a20000", "Syslog"))
ccachev <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Ccache"),load("Cap4-Coarse.xlsx","a1:a20000", "Ccache"))
gawkv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Gawk"),load("Cap4-Coarse.xlsx","a1:a20000", "Gawk"))
gzipv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Gzip"),load("Cap4-Coarse.xlsx","a1:a20000", "Gzip"))
libnfcv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "LibNFC"),load("Cap4-Coarse.xlsx","a1:a20000", "LibNFC"))
Libsshv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "LibSSH"),load("Cap4-Coarse.xlsx","a1:a20000", "LibSSH")  )
librdkafkav <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "LibrsKafka"),load("Cap4-Coarse.xlsx","a1:a20000", "LibrsKafka")  )
Mosquittov <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Mosquitto"),load("Cap4-Coarse.xlsx","a1:a20000", "Mosquitto")  )
MPSolvev <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "MPSolve"),load("Cap4-Coarse.xlsx","a1:a20000", "LibNFC")  )
OpenVPNv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "OpenVPN"),load("Cap4-Coarse.xlsx","a1:a20000", "OpenVPN"))
Ossecv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Ossec"),load("Cap4-Coarse.xlsx","a1:a20000", "Ossec"))
PianoBarv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "PianoBar"),load("Cap4-Coarse.xlsx","a1:a20000", "PianoBar"))
SilverSearchv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "SilverSearcher"),load("Cap4-Coarse.xlsx","a1:a20000", "SilverSearcher"))
GNUPlotv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "GNUPlot"),load("Cap4-Coarse.xlsx","a1:a20000", "GNUPlot"))
OpenSCv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "OpenSC"),load("Cap4-Coarse.xlsx","a1:a20000", "OpenSC"))
Sylpheedv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Sylpheed"),load("Cap4-Coarse.xlsx","a1:a20000", "Sylpheed"))
Curlv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Curl"),load("Cap4-Coarse.xlsx","a1:a20000", "Curl"))
gnuchessv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "gnuchess"),load("Cap4-Coarse.xlsx","a1:a20000", "gnuchess"))
totemv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Totem"),load("Cap4-Coarse.xlsx","a1:a20000", "Totem"))
m4v <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "m4"),load("Cap4-Coarse.xlsx","a1:a20000", "m4"))
diav <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "dia"),load("Cap4-Coarse.xlsx","a1:a20000", "dia"))
mapserverv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "mapserver"),load("Cap4-Coarse.xlsx","a1:a20000", "mapserver"))
ethersexv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "ethersex"),load("Cap4-Coarse.xlsx","a1:a20000", "ethersex"))
uwsgiv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "uwsgi"),load("Cap4-Coarse.xlsx","a1:a20000", "uwsgi"))
Hexchatv <- delta(load("Cap4-Coarse.xlsx","c1:c20000", "Hexchat"),load("Cap4-Coarse.xlsx","a1:a20000", "Hexchat"))



  C1 <- data.frame(value = vimv, variable = "Vim")
  C2 <- data.frame(value = pacemakerv, variable = "PaceMaker")
  C3 <- data.frame(value = lixmlv, variable = "LibXML")
  C4 <- data.frame(value = Lighttpdv, variable = "Lighttpd")
  C5 <- data.frame(value = Collectdv, variable = "Collectd")
  C6 <- data.frame(value = Syslogv, variable = "Syslog")
  C7 <- data.frame(value = ccachev, variable = "CCache")
  C8 <- data.frame(value = gawkv, variable = "Gawk")
  C9 <- data.frame(value = gzipv, variable = "Gzip")
  C10 <- data.frame(value = libnfcv, variable = "LibNFC")
  C11 <- data.frame(value = Libsshv, variable = "LibSSH")
  C12 <- data.frame(value = librdkafkav, variable = "LibrsKakfka")
  C13 <- data.frame(value = Mosquittov, variable = "Mosquitto")
  C14 <- data.frame(value = MPSolvev, variable = "MPSolve")
  C15 <- data.frame(value = OpenVPNv, variable = "OpenVPN")
  C16 <- data.frame(value = Ossecv, variable = "Ossec")
  C17 <- data.frame(value = PianoBarv, variable = "PianoBar")
  C18 <- data.frame(value = SilverSearchv, variable = "SilverSearcher")
  C19 <- data.frame(value = GNUPlotv, variable = "GNUPlot")
  C20 <- data.frame(value = OpenSCv, variable = "OpenSC")
  C21 <- data.frame(value = diav, variable = "Dia")
  C22 <- data.frame(value = uwsgiv, variable = "Uwsgi")
  C23 <- data.frame(value = mapserverv, variable = "MapServer")
  C24 <- data.frame(value = Hexchatv, variable = "Hexchat")
  C25 <- data.frame(value = ethersexv, variable = "Ethersex")
  C26 <- data.frame(value = m4v, variable = "M4")
  C27 <- data.frame(value = totemv, variable = "Totem")
  C28 <- data.frame(value = gnuchessv, variable = "Gnuchess")
  C29 <- data.frame(value = Curlv, variable = "Curl")
  C30 <- data.frame(value = Sylpheedv, variable = "Sylpheed")

  datavar <- rbind(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20,C21,C22,C23,C24,C25,C26,C27,C28,C29, C30)
  head(datavar)


  allS <- c(vimv,pacemakerv,lixmlv,Lighttpdv,Collectdv,Syslogv,ccachev,gawkv ,gzipv,libnfcv,Libsshv,librdkafkav,Mosquittov,MPSolvev,OpenVPNv,Ossecv,PianoBarv,SilverSearchv,GNUPlotv,OpenSCv,Sylpheedv,Curlv,gnuchessv,totemv,m4v,diav,mapserverv,ethersexv,uwsgiv,Hexchatv)
  dataallvar <- data.frame(value = allS, variable = "Sistemas")
  head(dataallvar)

#load Dependencies
vimd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Vim"))
pacemakerd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "PaceMaker"))
lixmld <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "LibXML"))
Lighttpdd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Lighttpd"))
Collectdd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Collectd"))
Syslogd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Syslog"))
ccached <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Ccache"))
gawkd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Gawk"))
gzipd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Gzip"))
libnfcd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "LibNFC"))
Libsshd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "LibSSH"))
librdkafkad <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "librskafka"))
Mosquittod <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Mosquitto"))
MPSolved <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "MPSolve"))
OpenVPNd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "OpenVPN"))
Ossecd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Ossec"))
PianoBard <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "PianoBar"))
SilverSearchd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "SilverSearch"))
GNUPlotd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "GNUPlot"))
OpenSCd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "OpenSC"))
Sylpheedd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Sylpheed"))
Curld <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "curl"))
gnuchessd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Gnuchess"))
totemd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Totem"))
m4d <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "m4"))
diad <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "dia"))
mapserverd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "mapserver"))
ethersexd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "ethersex"))
uwsgid <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "uwsgi"))
Hexchatd <- delta(load("Cap5_IMPACTO.xlsx","d1:d20000", "Hexchat"))


	C1 <- data.frame(value = vimd, variable = "Vim")
	C2 <- data.frame(value = pacemakerd, variable = "PaceMaker")
	C3 <- data.frame(value = lixmld, variable = "LibXML")
	C4 <- data.frame(value = Lighttpdd, variable = "Lighttpd")
	C5 <- data.frame(value = Collectdd, variable = "Collectd")
	C6 <- data.frame(value = Syslogd, variable = "Syslog")
	C7 <- data.frame(value = ccached, variable = "CCache")
	C8 <- data.frame(value = gawkd, variable = "Gawk")
	C9 <- data.frame(value = gzipd, variable = "Gzip")
	C10 <- data.frame(value = libnfcd, variable = "LibNFC")
	C11 <- data.frame(value = Libsshd, variable = "LibSSH")
	C12 <- data.frame(value = librdkafkad, variable = "LibrsKakfka")
	C13 <- data.frame(value = Mosquittod, variable = "Mosquitto")
	C14 <- data.frame(value = MPSolved, variable = "MPSolve")
	C15 <- data.frame(value = OpenVPNd, variable = "OpenVPN")
	C16 <- data.frame(value = Ossecd, variable = "Ossec")
	C17 <- data.frame(value = PianoBard, variable = "PianoBar")
	C18 <- data.frame(value = SilverSearchd, variable = "SilverSearcher")
	C19 <- data.frame(value = GNUPlotd, variable = "GNUPlot")
	C20 <- data.frame(value = OpenSCd, variable = "OpenSC")
	C21 <- data.frame(value = diad, variable = "Dia")
	C22 <- data.frame(value = uwsgid, variable = "Uwsgi")
	C23 <- data.frame(value = mapserverd, variable = "MapServer")
	C24 <- data.frame(value = Hexchatd, variable = "Hexchat")
	C25 <- data.frame(value = ethersexd, variable = "Ethersex")
	C26 <- data.frame(value = m4d, variable = "M4")
	C27 <- data.frame(value = totemd, variable = "Totem")
	C28 <- data.frame(value = gnuchessd, variable = "Gnuchess")
	C29 <- data.frame(value = Curld, variable = "Curl")
	C30 <- data.frame(value = Sylpheedd, variable = "Sylpheed")

  datadep <- rbind(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20,C21,C22,C23,C24,C25,C26,C27,C28,C29, C30)
  head(datadep)


  allS <- c(vimd,pacemakerd,lixmld,Lighttpdd,Collectdd,Syslogd,ccached,gawkd,gzipd,libnfcd,Libsshd,librdkafkad,Mosquittod,MPSolved,OpenVPNd,Ossecd,PianoBard,SilverSearchd,GNUPlotd,OpenSCd,Sylpheedd,Curld,gnuchessd,totemd,m4d,diad,mapserverd,ethersexd,uwsgid,Hexchatd)
  dataalldep <- data.frame(value = allS, variable = "Sistemas")
  head(dataalldep)




#load Dependencies Total
vimk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Vim"),load("Cap4-Coarse.xlsx","a1:a20000", "Vim"))
pacemakerk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "PaceMaker"),load("Cap4-Coarse.xlsx","a1:a20000", "PaceMaker"))
lixmlk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Libxml2"),load("Cap4-Coarse.xlsx","a1:a20000", "Libxml2"))
Lighttpdk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Lighttpd"),load("Cap4-Coarse.xlsx","a1:a20000", "Lighttpd"))
Collectdk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Collected"),load("Cap4-Coarse.xlsx","a1:a20000", "Collected"))
Syslogk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Syslog"),load("Cap4-Coarse.xlsx","a1:a20000", "Syslog"))
ccachek <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Ccache"),load("Cap4-Coarse.xlsx","a1:a20000", "Ccache"))
gawkk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Gawk"),load("Cap4-Coarse.xlsx","a1:a20000", "Gawk"))
gzipk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Gzip"),load("Cap4-Coarse.xlsx","a1:a20000", "Gzip"))
libnfck <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "LibNFC"),load("Cap4-Coarse.xlsx","a1:a20000", "LibNFC"))
Libsshk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "LibSSH"),load("Cap4-Coarse.xlsx","a1:a20000", "LibSSH"))
librdkafkak <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "LibrsKafka"),load("Cap4-Coarse.xlsx","a1:a20000", "LibrsKafka"))
Mosquittok <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Mosquitto"),load("Cap4-Coarse.xlsx","a1:a20000", "Mosquitto"))
MPSolvek <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "MPSolve"),load("Cap4-Coarse.xlsx","a1:a20000", "MPSolve"))
OpenVPNk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "OpenVPN"),load("Cap4-Coarse.xlsx","a1:a20000", "OpenVPN"))
Osseck <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Ossec"),load("Cap4-Coarse.xlsx","a1:a20000", "Ossec"))
PianoBark <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "PianoBar"),load("Cap4-Coarse.xlsx","a1:a20000", "PianoBar"))
SilverSearchk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "SilverSearcher"),load("Cap4-Coarse.xlsx","a1:a20000", "SilverSearcher"))
GNUPlotk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "GNUPlot"),load("Cap4-Coarse.xlsx","a1:a20000", "GNUPlot"))
OpenSCk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "OpenSC"),load("Cap4-Coarse.xlsx","a1:a20000", "OpenSC"))
Sylpheedk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Sylpheed"),load("Cap4-Coarse.xlsx","a1:a20000", "Sylpheed"))
Curlk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Curl"),load("Cap4-Coarse.xlsx","a1:a20000", "Curl"))
gnuchessk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "gnuchess"),load("Cap4-Coarse.xlsx","a1:a20000", "gnuchess"))
totemk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Totem"),load("Cap4-Coarse.xlsx","a1:a20000", "Totem"))
m4k <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "m4"),load("Cap4-Coarse.xlsx","a1:a20000", "m4"))
diak <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "dia"),load("Cap4-Coarse.xlsx","a1:a20000", "dia"))
mapserverk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "mapserver"),load("Cap4-Coarse.xlsx","a1:a20000", "mapserver"))
ethersexk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "ethersex"),load("Cap4-Coarse.xlsx","a1:a20000", "ethersex"))
uwsgik <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "uwsgi"),load("Cap4-Coarse.xlsx","a1:a20000", "uwsgi"))
Hexchatk <- delta(load("Cap4-Coarse.xlsx","k1:k20000", "Hexchat"),load("Cap4-Coarse.xlsx","a1:a20000", "Hexchat"))


  C1 <- data.frame(value = vimk, variable = "Vim")
  C2 <- data.frame(value = pacemakerk, variable = "PaceMaker")
  C3 <- data.frame(value = lixmlk, variable = "LibXML")
  C4 <- data.frame(value = Lighttpdk, variable = "Lighttpd")
  C5 <- data.frame(value = Collectdk, variable = "Collectd")
  C6 <- data.frame(value = Syslogk, variable = "Syslog")
  C7 <- data.frame(value = ccachek, variable = "CCache")
  C8 <- data.frame(value = gawkk, variable = "Gawk")
  C9 <- data.frame(value = gzipk, variable = "Gzip")
  C10 <- data.frame(value = libnfck, variable = "LibNFC")
  C11 <- data.frame(value = Libsshk, variable = "LibSSH")
  C12 <- data.frame(value = librdkafkak, variable = "LibrsKakfka")
  C13 <- data.frame(value = Mosquittok, variable = "Mosquitto")
  C14 <- data.frame(value = MPSolvek, variable = "MPSolve")
  C15 <- data.frame(value = OpenVPNk, variable = "OpenVPN")
  C16 <- data.frame(value = Osseck, variable = "Ossec")
  C17 <- data.frame(value = PianoBark, variable = "PianoBar")
  C18 <- data.frame(value = SilverSearchk, variable = "SilverSearcher")
  C19 <- data.frame(value = GNUPlotk, variable = "GNUPlot")
  C20 <- data.frame(value = OpenSCk, variable = "OpenSC")
  C21 <- data.frame(value = diak, variable = "Dia")
  C22 <- data.frame(value = uwsgik, variable = "Uwsgi")
  C23 <- data.frame(value = mapserverk, variable = "MapServer")
  C24 <- data.frame(value = Hexchatk, variable = "Hexchat")
  C25 <- data.frame(value = ethersexk, variable = "Ethersex")
  C26 <- data.frame(value = m4k, variable = "M4")
  C27 <- data.frame(value = totemk, variable = "Totem")
  C28 <- data.frame(value = gnuchessk, variable = "Gnuchess")
  C29 <- data.frame(value = Curlk, variable = "Curl")
  C30 <- data.frame(value = Sylpheedk, variable = "Sylpheed")

  datadept <- rbind(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20,C21,C22,C23,C24,C25,C26,C27,C28,C29, C30)
  head(datadept)


  allS <- c(vimk,pacemakerk,lixmlk,Lighttpdk,Collectdk,Syslogk,ccachek,gawkk,gzipk,libnfck,Libsshk,librdkafkak,Mosquittok,MPSolvek,OpenVPNk,Osseck,PianoBark,SilverSearchk,GNUPlotk,OpenSCk,Sylpheedk,Curlk,gnuchessk,totemk,m4k,diak,mapserverk,ethersexk,uwsgik,Hexchatk)
  dataalldept <- data.frame(value = allS, variable = "Sistemas")
  head(dataalldept)
  
  
  
  violinplotnolog <-function(data){
    library(ggplot2)
    library(scales)
    
    
    
    plt_wool <- ggplot(subset(data, value > 0) , aes(x=variable,y=value)) + 
      geom_violin(aes(fill = variable)) +stat_summary(fun.data="mean_sdl", geom="crossbar", width=0.002 ) +
      theme_gray() + theme(axis.text.x=element_text(face="plain", color="#000000", 
                                                    size=10, angle=0, vjust=0.2), axis.text.y = element_text(size=12)) +
      theme(axis.title=element_text(size=12,face="bold")) +
      labs(x = "", y = "Total") +
      annotation_logticks(sides = "rl") +
      theme(panel.grid.minor = element_blank()) +
      guides(title.hjust=0.5) +
      theme(plot.margin=unit(c(0,1,0,0),"mm"))
    
    yp <- subset(data, value>0)             # Choosing only +ve values in col x
    sts <- boxplot.stats(yp$value)$stats  # Compute lower and upper whisker limits
    
    p1 = plt_wool + coord_cartesian(ylim = c(sts[2]/2,max(sts)*1.05))
    
    p1
    p1+geom_boxplot(width=0.06)
    
  }