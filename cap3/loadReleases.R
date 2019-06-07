
#Load Releases
  vimr <- rmvoutlier(freq(releasesdate$vim))
  PaceMakerr <- rmvoutlier(freq(releasesdate$pacemaker))
  LibXMLr <- rmvoutlier(freq(releasesdate$LibXML))
  Lighttpdr <- rmvoutlier(freq(releasesdate$lighttpd))
  Collectedr <- rmvoutlier(freq(releasesdate$collectd))
  Syslogr <- rmvoutlier(freq(releasesdate$Syslog))
  ccacher <- rmvoutlier(freq(releasesdate$ccache))
  Gzipr <- rmvoutlier(freq(releasesdate$Gzip))
  libnfcr <- rmvoutlier(freq(releasesdate$libnfc))
  Libsshr <- rmvoutlier(freq(releasesdate$Libssh))
  librskafkar <- rmvoutlier(freq(releasesdate$librdkafka))
  Mosquittor <- rmvoutlier(freq(releasesdate$mosquitto))
  MPSolver <- rmvoutlier(freq(releasesdate$MPSolve))
  OpenVPNr <- rmvoutlier(freq(releasesdate$OpenVPN))
  Ossecr <- rmvoutlier(freq(releasesdate$ossec))
  PianoBarr <- rmvoutlier(freq(releasesdate$pianobar))
  SilverSearchr <- rmvoutlier(freq(releasesdate$silversearch))
  GNUPlotr <- rmvoutlier(freq(releasesdate$GNUPlot))
  OpenSCr <- rmvoutlier(freq(releasesdate$OpenSC))
  Sylpheedr <- rmvoutlier(freq(releasesdate$sylpheed))
  Curlr <- rmvoutlier(freq(releasesdate$curl))
  totemr <- rmvoutlier(freq(releasesdate$totem))
  diar <- rmvoutlier(freq(releasesdate$dia))
  mapserverr <- rmvoutlier(freq(releasesdate$mapserver))
  uwsgir <- rmvoutlier(freq(releasesdate$uwsgi))
  Hexchatr <- rmvoutlier(freq(releasesdate$Hexchat))


  C1 <- data.frame(value = vimr, variable = "Vim")
  C2 <- data.frame(value = PaceMakerr, variable = "PaceMaker")
  C3 <- data.frame(value = LibXMLr, variable = "LibXML")
  C4 <- data.frame(value = Lighttpdr, variable = "Lighttpd")
  C5 <- data.frame(value = Collectedr, variable = "Collectd")
  C6 <- data.frame(value = Syslogr, variable = "Syslog")
  C7 <- data.frame(value = ccacher, variable = "CCache")
  C9 <- data.frame(value = Gzipr, variable = "Gzip")
  C10 <- data.frame(value = libnfcr, variable = "LibNFC")
  C11 <- data.frame(value = Libsshr, variable = "LibSSH")
  C12 <- data.frame(value = librskafkar, variable = "LibrsKakfka")
  C13 <- data.frame(value = Mosquittor, variable = "Mosquitto")
  C14 <- data.frame(value = MPSolver, variable = "MPSolve")
  C15 <- data.frame(value = OpenVPNr, variable = "OpenVPN")
  C16 <- data.frame(value = Ossecr, variable = "Ossec")
  C17 <- data.frame(value = PianoBarr, variable = "PianoBar")
  C18 <- data.frame(value = SilverSearchr, variable = "SilverSearcher")
  C19 <- data.frame(value = GNUPlotr, variable = "GNUPlot")
  C20 <- data.frame(value = OpenSCr, variable = "OpenSC")
  C21 <- data.frame(value = diar, variable = "Dia")
  C22 <- data.frame(value = uwsgir, variable = "Uwsgi")
  C23 <- data.frame(value = mapserverr, variable = "MapServer")
  C24 <- data.frame(value = Hexchatr, variable = "Hexchat")
  C27 <- data.frame(value = totemr, variable = "Totem")
  C29 <- data.frame(value = Curlr, variable = "Curl")
  C30 <- data.frame(value = Sylpheedr, variable = "Sylpheed")
  

  
  datarelease <- rbind(C7,C5,C29,C21,C19,C9,C24,C10,C12,C11,C3,C4,C23,C13,C14,C20,C15,C16,C2,C17,C18,C30,C6,C27,C22, C1)
  head(datarelease)

  
  
  allS <- c(vimr ,  PaceMakerr ,  LibXMLr ,  Lighttpdr ,  Collectedr,  Syslogr,  ccacher,  Gzipr ,  libnfcr,  Libsshr ,  librskafkar,  Mosquittor ,  MPSolver ,  OpenVPNr ,  Ossecr ,  PianoBarr ,  SilverSearchr ,  GNUPlotr ,  OpenSCr ,  Sylpheedr ,  Curlr ,  totemr ,  diar ,  mapserverr ,  uwsgir,  Hexchatr)
  dataallrelease <- data.frame(value = allS, variable = "Sistemas")
  head(dataallrelease)









