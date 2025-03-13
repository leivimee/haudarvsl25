library(terra)
library(sf)
library(dplyr)
library(PresenceAbsence)
library(rjags)
library(runjags)
library(ggplot2)
library(ggdark)
#library(smoothr)
#library(stars)
library(tidyterra)

st_erase = function(x, y) st_difference(x, st_union(y))

#eesti
eesti<-st_read("ref/taust.gpkg", "eesti", quiet=T)
# seirealad
slsj <- st_read("src/PR0101_sj_n399_ar.shp", quiet=T)
# uurimata rabad
vslsj <- st_read("src/elup-vslsj.gpkg", "EL7110", quiet=T)
#jääksood
ammend<-st_read("ref/taust.gpkg", "ammend", quiet=T)
# paaride arvud
agr <- read_csv2("src/PR0101_agregeeritud_seireandmed.csv")

# loendusandmed
slvaatlus <- st_read("src/hlk-sood-2016-2023.gpkg", "vaatlused") %>% 
  mutate(aasta=as.integer(format(as_date(kuupäev, format="%d.%m.%Y"),"%Y"))) %>%
  filter(aasta>=2019)
sltransekt <- st_read("src/hlk-sood-2016-2023.gpkg", "loendusrajad") %>% 
  mutate(aasta=as.integer(format(as_date(kuupäev, format="%d.%m.%Y"),"%Y"))) %>%
  filter(aasta>=2019)


###############################
# pluapr
###############################
# kalibreerime elupaigamudeli
r <- rast("D:/sdm/soolind2023/pred/soolind-me-pluapr-v1.tif") %>%
  terra::aggregate(3) %>%
  terra::project("EPSG:3301")
names(r) <- "prob"
slspvtl<-slvaatlus %>%
  filter(liik=="pluapr")
# vaatlus
slspvtlr1<-terra::extract(r, slspvtl, cells=T, na.rm=T) %>%
  select(prob, cell) %>%
  distinct()
# territoorium
slspvtlr2<-terra::extract(r, st_buffer(slspvtl,300), cells=T, na.rm=T) %>%
  select(prob, cell) %>%
  distinct()
# transekt
slspvtlrt<-terra::extract(r, sltransekt, cells=T, na.rm=T) %>%
  select(prob, cell) %>%
  distinct()
# taust
samplebbox<-slspvtl %>% st_bbox() %>% st_as_sfc()
sltaust<-st_sample(samplebbox, nrow(slspvtlr1)) %>% st_as_sf()
slspvtlr00<-terra::extract(r, sltaust, cells=T, na.rm=T) %>%
  filter(!is.na(prob))
#slspvtlr0<-slspvtlrt %>% filter(cell%in%slspvtlr1$cell)

#slspvtlr0<-slspvtlr00 %>% filter(!cell %in% slspvtlr1$cell) %>% filter(!cell %in% slspvtlrt$cell) %>%
#  select(prob, cell) %>%
#  distinct()

#slspvtlr0<-slspvtlr00 %>% filter(!cell %in% slspvtlr1$cell) %>%
#  bind_rows(slspvtlrt %>% filter(cell%in%slspvtlr1$cell)) %>%
#  select(prob, cell) %>%
#  distinct()

# taust - territ. + transekt - territ.
slspvtlr0<-slspvtlr00 %>% filter(!cell %in% slspvtlr2$cell) %>%
  bind_rows(slspvtlrt %>% filter(!cell%in%slspvtlr2$cell)) %>%
  select(prob, cell) %>%
  distinct()

slspvalid<-slspvtlr1 %>% rename(predicted=prob, ID=cell) %>% mutate(observed=1) %>%
  bind_rows(
    slspvtlr0 %>% rename(predicted=prob, ID=cell) %>% mutate(observed=0)
  ) %>%
  select(ID, observed, predicted) %>%
  filter(!is.na(predicted))

# vali lävend vastavalt prognoosi väärtus = leidude vaatlussagedus
#slspthresh <- optimal.thresholds(slspvalid, opt.methods=6) %>% select(predicted) %>% unlist() %>% unname()
slspthresh <- optimal.thresholds(slspvalid, opt.methods=3) %>% select(predicted) %>% unlist() %>% unname()

rclmat <- c(0,slspthresh,NA, slspthresh,1,1) %>%
  matrix(ncol=3, byrow=TRUE)
rcl <- classify(r, rclmat, include.lowest=TRUE)

# rcl summa seirealades
slsjelup<-terra::extract(rcl, slsj, fun=sum, na.rm=T) %>%
  mutate(prob=replace_na(prob,0), elup_ha=prob*90*90/1e4) %>% rename(cellsum=prob) %>%
  bind_cols(
    slsj %>% st_drop_geometry() %>% select(kkr_kood,nimi)
  )

slspviimane<-agr %>% filter(`Liik/takson (lad)`=="Pluvialis apricaria") %>% 
  select(`Seirekoha KKR`, Seireaeg,`Mõõdetud arvväärtus`) %>%
  group_by(`Seirekoha KKR`) %>%
  slice_max(order_by=Seireaeg, n=1)

slsjelup<-slsjelup %>%
  left_join(
    slspviimane %>% rename(loendus=Seireaeg, kkr_kood=`Seirekoha KKR`, paare=`Mõõdetud arvväärtus`),
    by="kkr_kood"
  ) %>%
  mutate(paare=replace_na(paare,0))

slsjelup %>%
  filter(elup_ha>1 & paare>1) %>%
  ggplot()+
  geom_point(aes(x=elup_ha, y=paare), color="black")+
  #dark_theme_minimal()
  theme_minimal()


# Vektorda elupaigad, silu ja prognoosi neile tõenäoline arvukus

#rasp <- st_as_sf(stars::st_as_stars(rcl), as_points = FALSE, merge = TRUE)
rasp<-rcl %>%
  as.polygons() %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  mutate(elup_ha=drop_units(st_area(.))/1e4)
elupha<-rasp$elup_ha

# silumine
#raspsm<-smooth(st_as_sf(rasp), method = "ksmooth")



# MCMC simulatsioonid

slsjelup1 <- slsjelup %>%
  filter(elup_ha>1 & paare>1)

dataList <- list(N = nrow(slsjelup1), y = slsjelup1$paare, x = slsjelup1$elup_ha)

notrun<-T
if(notrun==T) {
  # lineaarne mudel
  model_string1 <- "model{
    for (i in 1:N) {
      y[i] ~ dnorm(beta0 + beta1 * x[i], sigma^-2)
    }
  
    beta0 ~ dnorm(0, 100^-2)
    beta1 ~ dnorm(0, 100^-2)
    sigma ~ dunif(0, 10)
  }"
  
  posterior <- run.jags(
    model_string1,
    n.chains = 1,
    data = dataList,
    monitor = c("beta0", "beta1", "sigma"),
    adapt = 1000,
    burnin = 5000,
    sample = 1e4
  )
  
  # prognoos
  post <- as.mcmc(posterior)
  postpred<-function(x,prob=0.5) {
    lp <- post[ , "beta0"] +  elupha * post[ , "beta1"]
    y <- rnorm(3e3, lp, post[, "sigma"])
    q <- quantile(y, probs=prob)
    return(q)
  }
  slsppostpred <- tibble(
    elup_ha=elupha,
    n1=unlist(lapply(elupha, function(x)unlist(postpred(x,0.025)) )),
    n2=unlist(lapply(elupha, function(x)unlist(postpred(x,0.5)) )),
    n3=unlist(lapply(elupha, function(x)unlist(postpred(x,0.975)) ))
  )
  
  slsppostpred %>% filter(elup_ha>=10) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3))
  slsppostpred %>% filter(elup_ha>=100) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3))
  
  slsjelup<-slsjelup %>%
    mutate(
      n1=unlist(lapply(elup_ha, function(x)unlist(postpred(x,0.025)) )),
      n2=unlist(lapply(elup_ha, function(x)unlist(postpred(x,0.5)) )),
      n3=unlist(lapply(elup_ha, function(x)unlist(postpred(x,0.975)) ))
    )
  
  slsjelup %>%
    ggplot()+
    geom_point(aes(x=n2,y=paare))+
    theme_minimal()
}

slsjelup<-slsjelup %>%
  mutate(
    n1=unlist(lapply(elup_ha, function(x)unlist(postpred(x,0.025)) )),
    n2=unlist(lapply(elup_ha, function(x)unlist(postpred(x,0.5)) )),
    n3=unlist(lapply(elup_ha, function(x)unlist(postpred(x,0.975)) ))
  )

slsjelup %>%
  ggplot()+
  geom_point(aes(x=n2,y=paare))+
  theme_minimal()

slsppostpred %>% filter(elup_ha>=10) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3))
slsppostpred %>% filter(elup_ha>=100) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3))

#rasp <- rasp %>% bind_cols(slsppostpred %>% select(2:4))

slsjout <- st_intersection(rasp, vslsj) %>%
  group_by(elup_id) %>%
  summarise(elup_ha=sum(elup_ha), do_union=T)
  
slsppostpred <- tibble(
  elup_ha=slsjout$elup_ha,
  n1=unlist(lapply(slsjout$elup_ha, function(x)unlist(postpred(x,0.025)) )),
  n2=unlist(lapply(slsjout$elup_ha, function(x)unlist(postpred(x,0.5)) )),
  n3=unlist(lapply(slsjout$elup_ha, function(x)unlist(postpred(x,0.975)) ))
)

slsjout <- slsjout %>% bind_cols(slsppostpred %>% select(2:4))

slsjout %>% st_drop_geometry() %>% filter(elup_ha>=1) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3))
slsjout %>% st_drop_geometry() %>% filter(elup_ha>=10) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3))
slsjout %>% st_drop_geometry() %>% filter(elup_ha>=100) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3))
slsjout %>% st_drop_geometry() %>% filter(kaardiobj_ha>=100) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3))


# Väljund

p<-slsjout %>% 
  filter(elup_ha>=10) %>% 
  ggplot()+
  geom_sf(data=eesti, fill=NA, color="grey")+
  geom_sf(data=slsj, fill="lightgrey", color=NA, linewidth=0.2)+
  geom_sf(fill="brown1", color="brown1")+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.text=element_text(size=6), axis.text=element_text(size=6))
ggsave("fig/slsj-pluapr-uurimata.png", p, width=4.5, height=3, bg="white")

p<-ggplot() +
  geom_spatraster(data = r, aes(fill=prob))+
  #geom_spatraster(data = r1, aes(fill=anttri))+
  #geom_sf(data=eesti, fill=NA, color="#cccccc")+
  scale_fill_distiller(palette="Spectral", direction = -1, na.value=NA)+
  #scale_fill_manual(breaks=c(1,2,3,4), labels=c("<0,02","0,02-0,04","0,04-0,08",">0,08"), values=c("#2b83ba","#81c0ab","#f3fabb","#e37b61","#d7191c"), na.value=NA)+
  #scale_fill_manual(breaks=c(1,2,3,4), labels=c("<0,04","0,04-0,06","0,06-0,12",">0,12"), values=c("white","#81c0ab","#f0dda6","#d7191c"), na.value=NA)+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.text=element_text(size=6), axis.text=element_text(size=6))
ggsave("fig/slsj-pluapr-sdm.png", p, width=4.5, height=3, bg="white")

p<-ggplot() +
  geom_spatraster(data = as.factor(rcl), aes(fill=prob))+
  #geom_spatraster(data = r1, aes(fill=anttri))+
  geom_sf(data=eesti, fill=NA, color="#cccccc")+
  scale_fill_manual(breaks=c(1), values="brown1", na.value=NA)+
  #scale_fill_distiller(palette="Spectral", direction = -1, na.value=NA)+
  #scale_fill_manual(breaks=c(1,2,3,4), labels=c("<0,02","0,02-0,04","0,04-0,08",">0,08"), values=c("#2b83ba","#81c0ab","#f3fabb","#e37b61","#d7191c"), na.value=NA)+
  #scale_fill_manual(breaks=c(1,2,3,4), labels=c("<0,04","0,04-0,06","0,06-0,12",">0,12"), values=c("white","#81c0ab","#f0dda6","#d7191c"), na.value=NA)+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.text=element_text(size=6), axis.text=element_text(size=6))
ggsave("fig/slsj-pluapr-elup.png", p, width=4.5, height=3, bg="white")

#slspvalid <- tibble(liik="pluapr", `Liik/takson (lad)`="Pluvialis apricaria") %>% bind_cols(slspvalid)
#slsjout <- tibble(liik="pluapr", `Liik/takson (lad)`="Pluvialis apricaria") %>% 
#  bind_cols(slsjout %>% select(elup_id, elup_ha, n1, n2, n3,kaardiobj_ha, kood, kkt))

p<-slsjelup %>%
  filter(elup_ha>1 & paare>1) %>%
  ggplot()+
  #geom_density_2d_filled(aes(x=elup_ha,y=paare))
  geom_point(aes(x=elup_ha, y=paare), color="black")+
  #dark_theme_minimal()
  theme_minimal()+
  theme(legend.title = element_blank(), legend.text=element_text(size=6), axis.text=element_text(size=6))
ggsave("fig/slsj-pluapr-eluphapaare.png", p, width=4.5, height=3, bg="white")

png("fig/slsj-pluapr-pacalib.png", width=12, height=12, units="cm", res=300, pointsize=8)
par(mfrow=c(2,2))
#png("figure/slelup-cal-hist.png",width=6, height=6, units="cm", res=300, pointsize=6)
slspvalid %>%
  presence.absence.hist(which.model=1,ylab="vaatluste arv kontrollvalimis\nnumber of plots", xlab="prognoos\npredicted probability",main="", N.bars=20, legend.text = c("esineb","ei esine"))
#dev.off()
#png("figure/slelup-cal-cal.png",width=6, height=6, units="cm", res=300, pointsize=6)
slspvalid %>%
  calibration.plot(which.model=1, ylab="esinemiste suhe kontrollvalimis \nobserved occurence", xlab="prognoos \npredicted probability", main="", N.bins=10)
#dev.off()
#png("figure/slelup-cal-roc.png",width=6, height=6, units="cm", res=300, pointsize=6)
slspvalid %>%
  auc.roc.plot(which.model=1, ylab="tundlikkus (tõeste esinemiste sagedus)\nsensitivity", xlab="ekslikkus (valede esinemiste sagedus)\n1-specificity",main="")
#dev.off()
#png("figure/slelup-cal-thr.png",width=6, height=6, units="cm", res=300, pointsize=6)
slspvalid %>%
  error.threshold.plot(which.model=1, ylab="statistiku väärtus\naccuracy measures", xlab="valitud esinemislävend\nthreshold", main="" )
dev.off()

save(slspvalid, slspthresh, slsjout, slsjelup, file="result/slsp-pluapr.RData")

slspelupvarv <- tibble(liik="pluapr") %>% 
  bind_cols(slsjout %>% st_drop_geometry() %>% filter(elup_ha>=1) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3)))











# final


slsplist<-list(
  "pluapr" = "Pluvialis apricaria",
  "numpha" = "Numenius phaeopus",
  "trigla" = "Tringa glareola",
  "trineb" = "Tringa nebularia",
  "limlim" = "Limosa limosa",
  "lanexc" = "Lanius excubitor",
  "numarq" = "Numenius arquata"
  )




for(i in 4:length(slsplist)) {
  
  # i<-3
  splyhend<-names(slsplist)[i]
  spnimi <- slsplist[i]

  # kalibreerime elupaigamudeli
  r1 <- rast(paste0("D:/sdm/soolind2023/pred/soolind-me-",splyhend,"-v1.tif")) %>%
    terra::aggregate(3) %>%
    terra::project("EPSG:3301")
  # maski kaevandusalad välja
  ram<-rasterize(ammend,r, vals=1)
  r<-mask(r1,ram,maskvalues=1,updatevalue=0)
  names(r) <- "prob"
  slspvtl<-slvaatlus %>%
    filter(liik==splyhend)
  # vaatlus
  slspvtlr1<-terra::extract(r, slspvtl, cells=T, na.rm=T) %>%
    select(prob, cell) %>%
    distinct()
  # territoorium
  slspvtlr2<-terra::extract(r, st_buffer(slspvtl,300), cells=T, na.rm=T) %>%
    select(prob, cell) %>%
    distinct()
  # transekt
  slspvtlrt<-terra::extract(r, sltransekt, cells=T, na.rm=T) %>%
    select(prob, cell) %>%
    distinct()
  # taust
  samplebbox<-slspvtl %>% st_bbox() %>% st_as_sfc()
  sltaust<-st_sample(samplebbox, nrow(slspvtlr1)) %>% st_as_sf()
  slspvtlr00<-terra::extract(r, sltaust, cells=T, na.rm=T) %>%
    filter(!is.na(prob))
  #slspvtlr0<-slspvtlrt %>% filter(cell%in%slspvtlr1$cell)
  

  
  # taust - territ. + transekt - territ.
  slspvtlr0<-slspvtlr00 %>% filter(!cell %in% slspvtlr2$cell) %>%
    bind_rows(slspvtlrt %>% filter(!cell%in%slspvtlr2$cell)) %>%
    select(prob, cell) %>%
    distinct()
  
  slspvalid<-slspvtlr1 %>% rename(predicted=prob, ID=cell) %>% mutate(observed=1) %>%
    bind_rows(
      slspvtlr0 %>% rename(predicted=prob, ID=cell) %>% mutate(observed=0)
    ) %>%
    select(ID, observed, predicted) %>%
    filter(!is.na(predicted))
  
  # vali lävend vastavalt prognoosi väärtus = leidude vaatlussagedus
  # optimal.thresholds(slspvalid)
  slspthresh <- optimal.thresholds(slspvalid, opt.methods=3) %>% select(predicted) %>% unlist() %>% unname()
  
  rclmat <- c(0,slspthresh,NA, slspthresh,1,1) %>%
    matrix(ncol=3, byrow=TRUE)
  rcl <- classify(r, rclmat, include.lowest=TRUE)
  
  # rcl summa seirealades
  slsjelup<-terra::extract(rcl, slsj, fun=sum, na.rm=T) %>%
    mutate(prob=replace_na(prob,0), elup_ha=prob*90*90/1e4) %>% rename(cellsum=prob) %>%
    bind_cols(
      slsj %>% st_drop_geometry() %>% select(kkr_kood,nimi)
    )
  
  slspviimane<-agr %>% filter(`Liik/takson (lad)`==spnimi) %>% 
    select(`Seirekoha KKR`, Seireaeg,`Mõõdetud arvväärtus`) %>%
    group_by(`Seirekoha KKR`) %>%
    slice_max(order_by=Seireaeg, n=1)
  
  slsjelup<-slsjelup %>%
    left_join(
      slspviimane %>% rename(loendus=Seireaeg, kkr_kood=`Seirekoha KKR`, paare=`Mõõdetud arvväärtus`),
      by="kkr_kood"
    ) %>%
    mutate(paare=replace_na(paare,0))
  
  p<-slsjelup %>%
    filter(elup_ha>1 & paare>1) %>%
    ggplot()+
    geom_point(aes(x=elup_ha, y=paare), color="black")+
    #dark_theme_minimal()
    theme_minimal()
  
  
  # Vektorda elupaigad ja prognoosi neile tõenäoline arvukus
  
  #rasp <- st_as_sf(stars::st_as_stars(rcl), as_points = FALSE, merge = TRUE)
  rasp<-rcl %>%
    as.polygons() %>%
    st_as_sf() %>%
    st_cast("POLYGON") %>%
    mutate(elup_ha=drop_units(st_area(.))/1e4)
  elupha<-rasp$elup_ha

  
  # MCMC simulatsioonid
  
  slsjelup1 <- slsjelup %>%
    filter(elup_ha>1 & paare>1)
  
  # hinda eelhinnangud
  spm1<-lm(paare~elup_ha, data=slsjelup1)
  
  dataList <- list(N = nrow(slsjelup1), y = slsjelup1$paare, x = slsjelup1$elup_ha, mu0=unname(coef(spm1)[1]), mu1=unname(coef(spm1)[2]), a=1, b=1 )
  
  
  # lin. mudel
  model_string <- "model{ 
  ## vaatlus
  for (i in 1:N){
     y[i] ~ dnorm(beta0 + beta1*x[i], invsigma2)
  }
  ## eelhinnang
  beta0 ~ dnorm(mu0, 1e-2)
  beta1 ~ dnorm(mu1, 1e-2)
  invsigma2 ~ dgamma(a, b)
  sigma <- sqrt(pow(invsigma2, -1))
}"
  
  posterior <- run.jags(
    model_string,
    n.chains = 1,
    data = dataList,
    monitor = c("beta0", "beta1", "sigma"),
    adapt = 1000,
    burnin = 5000,
    sample = 1e4
  )
  
  # prognoos
  post <- as.mcmc(posterior)
  postpred<-function(x,prob=0.5) {
    lp <- post[ , "beta0"] +  x * post[ , "beta1"]
    y <- rnorm(1e3, lp, post[, "sigma"])
    q <- quantile(y, probs=prob)
    # arvukus ei saa minna miinusesse
    if(q<0) { q<-0 }
    return(q)
  }
  
  slsjelup<-slsjelup %>%
    mutate(
      n1=unlist(lapply(elup_ha, function(x)unlist(postpred(x,0.025)) )),
      n2=unlist(lapply(elup_ha, function(x)unlist(postpred(x,0.5)) )),
      n3=unlist(lapply(elup_ha, function(x)unlist(postpred(x,0.975)) ))
    )
  p<-slsjelup %>%
    ggplot()+
    geom_point(aes(x=n2,y=paare))+
    theme_minimal()
  
  slsppostpred <- tibble(
    elup_ha=elupha,
    n1=unlist(lapply(elupha, function(x)unlist(postpred(x,0.025)) )),
    n2=unlist(lapply(elupha, function(x)unlist(postpred(x,0.5)) )),
    n3=unlist(lapply(elupha, function(x)unlist(postpred(x,0.975)) ))
  )
  #slsppostpred %>% filter(elup_ha>=10) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3))
  #slsppostpred %>% filter(elup_ha>=100) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3))
  
  #rasp <- rasp %>% bind_cols(slsppostpred %>% select(2:4))
  
  if(splyhend%in%c("pluapr","numpha")) {
    slsjout <- st_intersection(rasp, vslsj) %>%
      group_by(elup_id) %>%
      summarise(elup_ha=sum(elup_ha), do_union=T)
    slsppostpred <- tibble(
      elup_ha=slsjout$elup_ha,
      n1=unlist(lapply(slsjout$elup_ha, function(x)unlist(postpred(x,0.025)) )),
      n2=unlist(lapply(slsjout$elup_ha, function(x)unlist(postpred(x,0.5)) )),
      n3=unlist(lapply(slsjout$elup_ha, function(x)unlist(postpred(x,0.975)) ))
    )
    slsjout <- slsjout %>% bind_cols(slsppostpred %>% select(2:4))
  } else {
    # eemalda kattuvusega
    rslsji<-st_intersects(rasp, slsj)
    slsjout <- rasp %>% 
      slice(which(lengths(rslsji)==0)) %>%
      mutate(elup_id=1:nrow(.)) %>% select(elup_id, elup_ha)
    slsppostpred <- tibble(
      elup_ha=slsjout$elup_ha,
      n1=unlist(lapply(slsjout$elup_ha, function(x)unlist(postpred(x,0.025)) )),
      n2=unlist(lapply(slsjout$elup_ha, function(x)unlist(postpred(x,0.5)) )),
      n3=unlist(lapply(slsjout$elup_ha, function(x)unlist(postpred(x,0.975)) ))
    )
    slsjout <- slsjout %>% bind_cols(slsppostpred %>% select(2:4))
  }
  
  
  
  
  # Väljund
  
  p<-slsjout %>% 
    filter(elup_ha>=1) %>% 
    ggplot()+
    geom_sf(data=eesti, fill=NA, color="grey")+
    geom_sf(data=slsj, fill="lightgrey", color=NA, linewidth=0.2)+
    geom_sf(fill="brown1", color="brown1")+
    theme_minimal()+
    theme(legend.title = element_blank(), legend.text=element_text(size=6), axis.text=element_text(size=6))
  ggsave(paste0("fig/slsj-",splyhend,"-uurimata.png"), p, width=4.5, height=3, bg="white")
  
  if(!file.exists(paste0("fig/slsj-",splyhend,"-sdm.png"))) {
    p<-ggplot() +
      geom_spatraster(data = r, aes(fill=prob))+
      #geom_spatraster(data = r1, aes(fill=anttri))+
      #geom_sf(data=eesti, fill=NA, color="#cccccc")+
      scale_fill_distiller(palette="Spectral", direction = -1, na.value=NA)+
      #scale_fill_manual(breaks=c(1,2,3,4), labels=c("<0,02","0,02-0,04","0,04-0,08",">0,08"), values=c("#2b83ba","#81c0ab","#f3fabb","#e37b61","#d7191c"), na.value=NA)+
      #scale_fill_manual(breaks=c(1,2,3,4), labels=c("<0,04","0,04-0,06","0,06-0,12",">0,12"), values=c("white","#81c0ab","#f0dda6","#d7191c"), na.value=NA)+
      theme_minimal()+
      theme(legend.title = element_blank(), legend.text=element_text(size=6), axis.text=element_text(size=6))
    ggsave(paste0("fig/slsj-",splyhend,"-sdm.png"), p, width=4.5, height=3, bg="white")
  }
  
  p<-ggplot() +
    geom_spatraster(data = as.factor(rcl), aes(fill=prob))+
    #geom_spatraster(data = r1, aes(fill=anttri))+
    geom_sf(data=eesti, fill=NA, color="#cccccc")+
    #geom_sf(data=rasp, fill="brown1", color="brown1")+
    #geom_sf(data=slsj, fill="grey", color="grey")+
    scale_fill_manual(breaks=c(1), values="brown1", na.value=NA)+
    #scale_fill_distiller(palette="Spectral", direction = -1, na.value=NA)+
    #scale_fill_manual(breaks=c(1,2,3,4), labels=c("<0,02","0,02-0,04","0,04-0,08",">0,08"), values=c("#2b83ba","#81c0ab","#f3fabb","#e37b61","#d7191c"), na.value=NA)+
    #scale_fill_manual(breaks=c(1,2,3,4), labels=c("<0,04","0,04-0,06","0,06-0,12",">0,12"), values=c("white","#81c0ab","#f0dda6","#d7191c"), na.value=NA)+
    theme_minimal()+
    theme(legend.title = element_blank(), legend.text=element_text(size=6), axis.text=element_text(size=6), legend.position = "none")
  ggsave(paste0("fig/slsj-",splyhend,"-elup.png"), p, width=4.5, height=3, bg="white")
  
  #slspvalid <- tibble(liik="pluapr", `Liik/takson (lad)`="Pluvialis apricaria") %>% bind_cols(slspvalid)
  #slsjout <- tibble(liik="pluapr", `Liik/takson (lad)`="Pluvialis apricaria") %>% 
  #  bind_cols(slsjout %>% select(elup_id, elup_ha, n1, n2, n3,kaardiobj_ha, kood, kkt))
  
  p<-slsjelup %>%
    filter(elup_ha>1 & paare>1) %>%
    ggplot()+
    #geom_density_2d_filled(aes(x=elup_ha,y=paare))
    geom_point(aes(x=elup_ha, y=paare), color="black")+
    #dark_theme_minimal()
    theme_minimal()+
    theme(legend.title = element_blank(), legend.text=element_text(size=6), axis.text=element_text(size=6))
  ggsave(paste0("fig/slsj-",splyhend,"-eluphapaare.png"), p, width=4.5, height=3, bg="white")
  
  png(paste0("fig/slsj-",splyhend,"-pacalib.png"), width=12, height=12, units="cm", res=300, pointsize=8)
  par(mfrow=c(2,2))
  #png("figure/slelup-cal-hist.png",width=6, height=6, units="cm", res=300, pointsize=6)
  slspvalid %>%
    presence.absence.hist(which.model=1,ylab="vaatluste arv kontrollvalimis\nnumber of plots", xlab="prognoos\npredicted probability",main="", N.bars=20, legend.text = c("esineb","ei esine"))
  #dev.off()
  #png("figure/slelup-cal-cal.png",width=6, height=6, units="cm", res=300, pointsize=6)
  slspvalid %>%
    calibration.plot(which.model=1, ylab="esinemiste suhe kontrollvalimis \nobserved occurence", xlab="prognoos \npredicted probability", main="", N.bins=10)
  #dev.off()
  #png("figure/slelup-cal-roc.png",width=6, height=6, units="cm", res=300, pointsize=6)
  slspvalid %>%
    auc.roc.plot(which.model=1, ylab="tundlikkus (tõeste esinemiste sagedus)\nsensitivity", xlab="ekslikkus (valede esinemiste sagedus)\n1-specificity",main="")
  #dev.off()
  #png("figure/slelup-cal-thr.png",width=6, height=6, units="cm", res=300, pointsize=6)
  slspvalid %>%
    error.threshold.plot(which.model=1, ylab="statistiku väärtus\naccuracy measures", xlab="valitud esinemislävend\nthreshold", main="" )
  dev.off()
  
  save(slspvalid, slspthresh, slsjout, slsjelup, slspthresh, file=paste0("result/slsp-",splyhend,".RData"))
  
  if(i==1) {
    slspelupvarv <- tibble(liik=splyhend, eluphalavend=1) %>% 
        bind_cols(slsjout %>% st_drop_geometry() %>% filter(elup_ha>=1) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3)))
    slspelupvarv <- slspelupvarv %>% bind_rows(
      tibble(liik=splyhend, eluphalavend=10) %>% 
        bind_cols(slsjout %>% st_drop_geometry() %>% filter(elup_ha>=10) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3)))
      )
    slspelupvarv <- slspelupvarv %>% bind_rows(
      tibble(liik=splyhend, eluphalavend=100) %>% 
        bind_cols(slsjout %>% st_drop_geometry() %>% filter(elup_ha>=100) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3)))
    )
  } else {
    slspelupvarv <- slspelupvarv %>% bind_rows(
      tibble(liik=splyhend, eluphalavend=1) %>% 
        bind_cols(slsjout %>% st_drop_geometry() %>% filter(elup_ha>=1) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3)))
    )
    slspelupvarv <- slspelupvarv %>% bind_rows(
      tibble(liik=splyhend, eluphalavend=10) %>% 
        bind_cols(slsjout %>% st_drop_geometry() %>% filter(elup_ha>=10) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3)))
    )
    slspelupvarv <- slspelupvarv %>% bind_rows(
      tibble(liik=splyhend, eluphalavend=100) %>% 
        bind_cols(slsjout %>% st_drop_geometry() %>% filter(elup_ha>=100) %>% summarise(N1=sum(n1), N2=sum(n2), N3=sum(n3)))
    )
  }

  
  
}

save(slspelupvarv, file="result/slspelupvarv-1-10-100ha-20250313.Rdata")
