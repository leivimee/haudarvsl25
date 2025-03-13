library(tidyverse)
library(rtrim)
library(readxl)


agr <- read_csv2("src/PR0101_agregeeritud_seireandmed.csv") %>%
  bind_rows(
    read_excel("src/digimata.xlsx") %>% mutate(Programm="Haudelindude kooslused (madalsood ja rabad)")
  )

# kontroll
agr %>% filter(`Seirekoha nimi`=="Võlla raba") %>% filter(Seireaeg==2024) %>% select(Seireaeg, `Seirekoha KKR`, `Seirekoha nimi`, `Liik/takson (lad)`, `Mõõdetud arvväärtus`)

data1 <- agr

spstrata<-expand_grid(
  `Liik/takson (lad)`=c(
    "Pluvialis apricaria",
    "Lyrurus tetrix",
    "Vanellus vanellus",
    "Tringa glareola",
    "Gallinago gallinago",
    "Larus canus",
    "Tringa totanus",
    "Numenius phaeopus",
    "Bucephala clangula",
    "Anas platyrhynchos",
    "Limosa limosa",
    "Lanius collurio",
    "Tringa nebularia",
    "Turdus viscivorus",
    "Numenius arquata",
    "Anas crecca",
    "Calidris pugnax",
    "Lanius excubitor",
    "Chroicocephalus ridibundus",
    "Aythya fuligula",
    "Hydrocoloeus minutus",
    "Circus pygargus",
    "Larus argentatus",
    "Cygnus cygnus",
    "Circus aeruginosus",
    "Podiceps auritus",
    "Calidris alpina schinzii",
    "Charadrius dubius",
    "Spatula clypeata",
    "Chlidonias niger",
    "Lagopus lagopus",
    "Mareca penelope"
  ),
  Programm=c(
    "Haudelindude kooslused (madalsood ja rabad)"
    ),
  Periood=c("LT","ST")
)

excludecp<-tibble(
  "Pluvialis apricaria" = c(1988, 2003)
)

#ymin<-1980
#ymax<-2024



for(i in 1:nrow(spstrata)) {
  
  #i<-2
  spstrataperiodid<-spstrata$Periood[i]
  if(spstrataperiodid=="ST") {
    sprange<-data1 %>%
      filter(Seireaeg>=2024-12+1) %>%
      filter(!is.na(`Seirekoha KKR`)) %>%
      filter(Programm==spstrata$Programm[i]) %>%
      filter(`Liik/takson (lad)`==spstrata$`Liik/takson (lad)`[i]) %>%
      filter(`Mõõdetud arvväärtus`>0) %>%
      select(Seireaeg) %>%
      unlist %>% unname() %>%
      range()
    ymin<-sprange[1]
    ymax<-sprange[2]
    if(ymax<2024) {
      sprange<-data1 %>%
        filter(Seireaeg>=ymax-12+1) %>%
        filter(!is.na(`Seirekoha KKR`)) %>%
        filter(Programm==spstrata$Programm[i]) %>%
        filter(`Liik/takson (lad)`==spstrata$`Liik/takson (lad)`[i]) %>%
        filter(`Mõõdetud arvväärtus`>0) %>%
        select(Seireaeg) %>%
        unlist %>% unname() %>%
        range()
      ymin<-sprange[1]
    }
  } else {
    sprange<-data1 %>%
      filter(Seireaeg>=1980) %>%
      filter(!is.na(`Seirekoha KKR`)) %>%
      filter(Programm==spstrata$Programm[i]) %>%
      filter(`Liik/takson (lad)`==spstrata$`Liik/takson (lad)`[i]) %>%
      filter(`Mõõdetud arvväärtus`>0) %>%
      select(Seireaeg) %>%
      unlist %>% unname() %>%
      range()
    ymin<-sprange[1]
    ymax<-sprange[2]    
  }
  
  visitdata <- data1 %>%
    filter(!is.na(`Seirekoha KKR`)) %>%
    filter(Seireaeg>=ymin & Seireaeg<=ymax) %>%
    filter(Programm==spstrata$Programm[i]) %>%
    rename(site=`Seirekoha KKR`,year=Seireaeg,count=`Mõõdetud arvväärtus`) %>%
    select(`Liik/takson (lad)`,site,year,count)
  spdata<-visitdata %>%
    filter(`Liik/takson (lad)`==spstrata$`Liik/takson (lad)`[i]) %>%
    select(site,year,count) %>%
    filter(count>0)
  visitdata<-visitdata %>% filter(site %in% spdata$site) %>%
    select(site,year) %>%
    distinct()
  
  jldatawz<-expand_grid(year=ymin:ymax,site=unique(visitdata$site)) %>%
    left_join(
      visitdata %>% mutate(visit=1),
      by=c("site","year")
    ) %>%
    left_join(
      spdata,
      by=c("site","year")
    ) %>%
    mutate(
      count=replace(count, which(is.na(count)&visit==1), 0)
    ) %>%
    group_by(site) %>%
    arrange(-desc(year),.by_group = T) %>%
    select(site,year,count)
  
  
  
  #m1<-try( rtrim::trim(count~site+year, data=jldatawz, model=2, changepoints="all", overdisp=T, serialcor=T) )
  #m1<-try( rtrim::trim(count~site+year, data=jldatawz, model=1, overdisp=T, serialcor=F) )
  #if(inherits(m1,"try-error")) {
  #  m1<-try( rtrim::trim(count~site+year, data=jldatawz, model=1, overdisp=F, serialcor=F) )
  #  if(inherits(m1,"try-error")) {
  #    next;
  #  }
  #}
  #overall(m1)
  #totals(m1)
  #plot(index(m1))
  #plot(overall(m1))
  
  # cp
  sptimetotal<-jldatawz %>% group_by(year) %>% summarise(sites=length(unique(site[!is.na(count)])), total=sum(count,na.rm=T)) %>%
    ungroup() %>%
    mutate(
      total50=quantile(total, probs=0.3),
      cp=1:nrow(.),
      use=ifelse(total<total50 & !cp==nrow(.) & sites>=3, TRUE, FALSE)
    )
  spcp<-sptimetotal %>% filter(use==TRUE) %>% select(cp) %>% unlist() %>% unname()
  
  if(spstrataperiodid=="ST") {
    m1<-try( rtrim::trim(count~site+year, data=jldatawz, model=1, overdisp=T, serialcor=F) )
    if(inherits(m1,"try-error")) {
      m1<-try( rtrim::trim(count~site+year, data=jldatawz, model=1, overdisp=F, serialcor=F) )
      if(inherits(m1,"try-error")) {
        next;
      }
    }
  } else {
    m1<-try( rtrim::trim(count~site+year, data=jldatawz, model=2, changepoints=spcp, overdisp=T, serialcor=T) )
    if(inherits(m1,"try-error")) {
      m1<-try( rtrim::trim(count~site+year, data=jldatawz, model=2, changepoints=spcp, overdisp=F, serialcor=F) )
      if(inherits(m1,"try-error")) {
        m1<-try( rtrim::trim(count~site+year, data=jldatawz, model=1, overdisp=T, serialcor=F) )
        if(inherits(m1,"try-error")) {
          next;
        }
      }
    }
  }

  #overall(m1)
  #totals(m1)
  #plot(index(m1))
  #plot(overall(m1))
  
  if(i==1) {
    overalls<-tibble(
      Programm=spstrata$Programm[i],
      Liik=spstrata$`Liik/takson (lad)`[i],
      Periood=spstrataperiodid
    ) %>% 
      bind_cols(as_tibble(overall(m1)$slope))
    
    indices<-tibble(
      Programm=spstrata$Programm[i],
      Liik=spstrata$`Liik/takson (lad)`[i],
      Periood=spstrataperiodid
    ) %>% 
      bind_cols(as_tibble(index(m1, level = 0.95, which="imputed")))
    
    totals <- tibble(
      Programm=spstrata$Programm[i],
      Liik=spstrata$`Liik/takson (lad)`[i],
      Periood=spstrataperiodid
    ) %>% 
      bind_cols(totals(m1, which="imputed", level=0.95))
    
  } else {
    overalls<-overalls %>% 
      bind_rows(
        tibble(
          Programm=spstrata$Programm[i],
          Liik=spstrata$`Liik/takson (lad)`[i],
          Periood=spstrataperiodid
        ) %>% 
          bind_cols(as_tibble(overall(m1)$slope))
      )
    indices<-indices %>%
      bind_rows(
        tibble(
          Programm=spstrata$Programm[i],
          Liik=spstrata$`Liik/takson (lad)`[i],
          Periood=spstrataperiodid
        ) %>% 
          bind_cols(as_tibble(index(m1, level = 0.95, which="imputed")))
      )
    totals <- totals %>%
      bind_rows(
        tibble(
          Programm=spstrata$Programm[i],
          Liik=spstrata$`Liik/takson (lad)`[i],
          Periood=spstrataperiodid
        ) %>% 
          bind_cols(totals(m1, which="imputed", level=0.95))
      )
    
  }
  
}

save(overalls, indices, totals, file="result/spstrata-20250312-1.RData")




