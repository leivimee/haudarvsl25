library(sf)
library(tidyverse)
library(units)
library(rjags)
library(runjags)


# loendusandmed
load("src/sood-20250314.RData")
# str(vaatlus)
# str(transekt)

# millised seirealad on antud perioodis loendusandmeteta
# seirealad
slsj <- st_read("src/PR0101_sj_n399_ar.shp", quiet=T)
# paaride arvud
agr <- read_csv2("src/PR0101_agregeeritud_seireandmed.csv") %>%
  bind_rows(
    read_excel("src/digimata.xlsx") %>% mutate(Programm="Haudelindude kooslused (madalsood ja rabad)")
  )

trslsji<-st_intersects(slsj, transekt)
slsjlisada<-slsj %>%
  st_drop_geometry() %>%
  slice(which(lengths(trslsji)==0)) %>%
  select(kkr_kood) %>%
  unlist() %>% unname()

viimane <- agr %>% 
  filter(`Seirekoha KKR` %in% slsjlisada) %>%
  group_by(`Seirekoha KKR`) %>%
  summarise(Seireaeg=max(Seireaeg)) %>%
  left_join(agr %>% select(`Seirekoha KKR`, Seireaeg, `Liik/takson (lad)`, `Mõõdetud arvväärtus`), by=c("Seirekoha KKR", "Seireaeg"))

viimane1<-viimane %>%
  group_by(`Liik/takson (lad)`) %>%
  summarise(loendus=sum(`Mõõdetud arvväärtus`)) %>%
  rename(liik=`Liik/takson (lad)`) %>%
  arrange(desc(loendus))

model_string <- "model{
  # Eelhinnangud
  psi ~ dunif(0,1)
  for(s in 1:nsites){
    N[s] ~ dbinom(psi, 100)
    #N[s] ~ dbinom(psi, 1.3*ncap[s])
  }
  for(o in 1:nobsost){
    sigma[o] ~ dunif(0, 1000)
  }

  for(s in 1:nsites){
    # Avastamisprotsess ribades 1:nD
    for(g in 1:nD){
      log(p[s,g]) <- -midpt[g] * midpt[g] / (2*sigma[obsost[s]]*sigma[obsost[s]])
      pi[s,g] <- delta[g] / B
      cp[s,g] <- p[s,g] * pi[s,g]
      cp.cond[s,g] <- cp[s,g] / pcap[s]
    }
    pcap[s] <- sum(cp[s,])
    # Vaatlusprotsess
    ncap[s] ~ dbin(pcap[s], N[s]) # Part 2 of HM
    Y[s,1:nD] ~ dmulti(cp.cond[s,1:nD], ncap[s])
  }

  Ntotal <- sum(N[])
  D <- Ntotal / A
  #(2*0.2*3768.805)
}"

effort1<-transekt %>% 
  mutate(len=drop_units(st_length(.))) %>%
  st_drop_geometry() %>% 
  filter(skeem=="sood")



slsplist<-list(
  "lyrtet" = "Lyrurus tetrix",
  "vanvan" = "Vanellus vanellus",
  "pluapr" = "Pluvialis apricaria",
  "numpha" = "Numenius phaeopus",
  "numarq" = "Numenius arquata",
  "trigla" = "Tringa glareola",
  "trineb" = "Tringa nebularia",
  "tritot" = "Tringa totanus",
  "limlim" = "Limosa limosa",
  "lanexc" = "Lanius excubitor",
  "lancol" = "Lanius collurio"
)


library(progress)
pb <- txtProgressBar(min = 1, max = length(slsplist), style = 3)

for(i in 1:length(slsplist)) {
  #i<-3
  splyhend<-names(slsplist)[i]
  spnimi <- unname(unlist(slsplist[i]))
  
  # kõik tr
  obsostbands0 <- vaatlus %>%
    st_drop_geometry() %>%
    filter(skeem=="sood") %>%
    filter(!is.na(liik) & !liik=="") %>%
    select(transekt,obsrvr) %>%
    distinct()
  
  # liigi esinemisega
  obsostbands11<-vaatlus %>%
    st_drop_geometry() %>%
    filter(skeem=="sood") %>%
    filter(!is.na(liik) & !liik=="") %>%
    filter(liik==splyhend) %>%
    filter(!is.na(hinnang) & hinnang>0) %>%
    filter(!is.na(band)) %>%
    group_by(transekt,obsrvr,band) %>%
    summarise(paare=sum(hinnang)) %>%
    pivot_wider(names_from="band", values_from="paare", values_fill=0) %>%
    ungroup() %>%
    select(transekt,obsrvr,`1`,`2`,`3`,`4`,`5`)
  
  obsostbands1 <- obsostbands0 %>% left_join(
    obsostbands11,
    by=c("transekt","obsrvr")
  ) %>% 
  mutate(across(3:7, ~replace_na(.x,0)))
  
  # gen obs x obsost kood
  obsost<-obsostbands1 %>%
    mutate(ncap=`1`+`2`+`3`+`4`+`5`) %>%
    #filter(ncap>1) %>%
    #filter(len>200) %>%
    group_by(obsrvr) %>%
    summarise(nseg=n()) %>%
    ungroup() %>%
    mutate(kood=1:nrow(.))
  
  # lisa kood
  obsostbands2 <- obsostbands1 %>%
    ungroup() %>%
    #mutate(ncap=`1`+`2`+`3`+`4`+`5`+`6`) %>%
    mutate(ncap=`1`+`2`+`3`+`4`+`5`) %>%
    #filter(ncap>1) %>%
    #filter(len>200) %>%
    left_join(obsost %>% select(obsrvr, kood), by=c("obsrvr")) %>%
    rename(obsostkood=kood)
  
  # läbitud transektid
  ytotlen<-effort1 %>% filter(transekt%in%obsostbands2$transekt) %>% summarise(len=sum(len)/1e3) %>% unlist() %>% unname()
  
  yi<-obsostbands2 %>%
    ungroup() %>%
    select(`1`,`2`,`3`,`4`,`5`)
    #select(`1`,`2`,`3`,`4`,`5`)
  
  #xg <- c(0,25,50,100,150,200,250)
  xg <- c(0,25,50,100,150,200)
  nD <- length(xg) - 1 # Number of intervals = length(xg)
  #midpt <- c(25/2, 25+25/2, 75, 125, 175, 225)
  midpt <- c(25/2, 25+25/2, 75, 125, 175)
  #delta <- c(25,25,50,50,50,50)
  delta <- c(25,25,50,50,50)
  #ncap = yi %>% mutate(N=`1`+`2`+`3`+`4`+`5`+`6`) %>% select(N) %>% unlist() %>% unname()
  ncap = yi %>% mutate(N=`1`+`2`+`3`+`4`+`5`) %>% select(N) %>% unlist() %>% unname()
  
  dataList <- list(
    #Y = as.matrix(yi[1:6]),
    Y = as.matrix(yi[1:5]),
    ncap = ncap,
    #nD = 6,
    nD = 5,
    midpt = midpt,
    delta = delta,
    nsites = nrow(yi),
    #B = 250,
    B=200,
    obsost = as.numeric(obsostbands2$obsostkood),
    nobsost = nrow(obsost),
    A = 2*0.2*ytotlen
  )
  
  posterior <- run.jags(
    model_string,
    n.chains = 3,
    data = dataList,
    monitor = c("Ntotal","D"),
    adapt = 1000,
    burnin = 5000,
    sample = 1e4
  )
  
  postssummary1<-tibble(liik=splyhend, ladnimi=spnimi, loendatud=sum(yi, na.rm=T), transekte=nrow(yi), km=ytotlen) %>% bind_cols( summary(posterior) )
  if(i==1) {
    postssummary<-postssummary1
  } else {
    postssummary <- postssummary %>%
      bind_rows(
        postssummary1
      )
  }
  
  setTxtProgressBar(pb, i)
}

# param jäi lisamata
postssummary$param <- rep(c("N","D"),length(slsplist))

# hinda vaatlusandmeteta elupaigad, st andmed on kogutud enne 2013 või ei ole veel digitud (2023, 2024)
slsjspparand<-postssummary %>% select(1:2) %>%
  distinct() %>%
  left_join(
    viimane1 %>% rename(ladnimi=liik)
  )

for(i in 1:nrow(slsjspparand)) {
  #i<-3
  # kust liiki leitud ? 
  slsjspleitud<-agr %>% filter(`Liik/takson (lad)`==slsjspparand$ladnimi[i] ) %>% select(`Seirekoha KKR`) %>% unlist() %>% unname()
  slsjspaladlisada1<-slsj %>% filter(kkr_kood %in% slsjlisada) %>% filter(kkr_kood %in% slsjspleitud) %>%
    mutate(A=drop_units(st_area(.))/1e6) %>%
    select(kkr_kood, nimi, A) %>%
    st_drop_geometry()
  if(i==1) {
    slsjspaladlisada<-tibble(liik=slsjspparand$liik[i]) %>% bind_cols( slsjspaladlisada1 )
    } else {
      slsjspaladlisada<-slsjspaladlisada %>% bind_rows( tibble(liik=slsjspparand$liik[i]) %>% bind_cols( slsjspaladlisada1) )
  }
}

slsjspdsarv<-slsjspparand %>%
  left_join(
    slsjspaladlisada,
    by="liik"
  ) %>%
  left_join(
    postssummary %>% filter(param=="D") %>% select(liik, Lower95, Median, Upper95),
    by="liik"
  ) %>%
  mutate(
    n1=round(A*Lower95,0),
    n2=round(A*Median,0),
    n3=round(A*Upper95,0)
  ) %>%
  group_by(liik,ladnimi) %>%
  summarise(loendus=loendus[1], A=sum(A), N1=sum(n1), N2=sum(n2), N3=sum(n3))


save(postssummary, slsjspdsarv, file="result/slsp-dist-20250315.RData")



