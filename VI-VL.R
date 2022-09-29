require(openxlsx)
require(tidyverse)
require(plotly)
require(cartogram)

opstine<- read.xlsx("sifarnici-mesta-opstine-okruzi-delatnosti.xlsx", sheet = "Opština")
VLautomig<- read.csv("VL-AutoMig.csv")
VLautomig <- left_join(VLautomig, opstine, by=c("Code"="OpstinaSifra"))


VLpop<- read.csv("VL-pop.csv")

VLpop<- VLpop %>% gather("X1991","X2002","X2011", key="Godina", value = "pop")
VLpop$Godina <- gsub("X", replacement = "", VLpop$Godina)
VLpop$Godina <- as.integer(VLpop$Godina)

VLautomig$Code <- as.character(VLautomig$Code)
VLautomig<- left_join(VLautomig,VLpop, by=c("Code"="Code", "Godina"="Godina"))
VLautomig$pop <- as.integer(VLautomig$pop)

load("srd")
srd@data$okrug <- gsub(as.character(srd@data$QNAME), pattern = " okrug", replacement = '')
srd@data$okrug[2] <- "Beogradski"
srd@data$area <- raster::area(srd)/1000000
VLautomig <- VLautomig %>% arrange(-Doseljeno)
srd$Weights<- left_join(srd@data,VLautomig %>% group_by(Okrug, Godina) %>% summarise(Weights=sum(pop*Doseljeno/100,na.rm = T)) %>% filter(Godina==1991), by=c("QNAME"="Okrug"))$Weights
srd$Doseljeno1991<- left_join(srd@data,VLautomig %>% filter(Godina==1991)%>% group_by(Okrug) %>% summarise(Doseljeno=sum(pop*Doseljeno,na.rm = T)/sum(pop,na.rm = T),popup=paste(Opština, round(Doseljeno,1), collapse = "\n")) , by=c("QNAME"="Okrug"))$Doseljeno
srd$Doseljeno2002<- left_join(srd@data,VLautomig %>% filter(Godina==2002)%>%group_by(Okrug) %>% summarise(Doseljeno=sum(pop*Doseljeno,na.rm = T)/sum(pop,na.rm = T),popup=paste(Opština, round(Doseljeno,1), collapse = "\n")) , by=c("QNAME"="Okrug"))$Doseljeno
srd$Doseljeno2011<- left_join(srd@data,VLautomig %>% filter(Godina==2011)%>%group_by(Okrug) %>% summarise(Doseljeno=sum(pop*Doseljeno,na.rm = T)/sum(pop,na.rm = T), popup=paste(Opština, round(Doseljeno,1), collapse = "\n")) , by=c("QNAME"="Okrug"))$Doseljeno
srd$Popup1991<- left_join(srd@data,VLautomig %>% filter(Godina==1991)%>% group_by(Okrug) %>% summarise(popup=paste0(gsub(trimws(OpstinaNazivLat), pattern =  "Beograd-|Beograd -",replacement = "")," ", format(Doseljeno,nsmall=1, digits=2, decimal.mark=","),"%", collapse = "\n")) , by=c("QNAME"="Okrug"))$popup
srd$Popup2002<- left_join(srd@data,VLautomig %>% filter(Godina==2002)%>%group_by(Okrug) %>% summarise(popup=paste0(gsub(trimws(OpstinaNazivLat), pattern =  "Beograd-|Beograd -",replacement = "")," ", format(Doseljeno,nsmall=1, digits=2, decimal.mark=","),"%", collapse = "\n")) , by=c("QNAME"="Okrug"))$popup
srd$Popup2011<- left_join(srd@data,VLautomig %>% filter(Godina==2011)%>%group_by(Okrug) %>% summarise(popup=paste0(gsub(trimws(OpstinaNazivLat), pattern =  "Beograd-|Beograd -",replacement = "")," ", format(Doseljeno,nsmall=1, digits=2, decimal.mark=","),"%" , collapse = "\n")) , by=c("QNAME"="Okrug"))$popup

srd$Weights<- srd$Weights * srd@data$area 
serb_proj <- "+proj=tmerc +lat_0=0 +lon_0=21 +k=0.9999 +x_0=7500000 +y_0=0 +ellps=bessel +towgs84=574.027,170.175,401.545,4.88786,-0.66524,-13.24673,6.89 +units=m"
srd<- spTransform( srd,serb_proj ) 
srd_ncount <- cartogram_dorling(srd, "Weights")
require(plotly)
srd_sf<- sf::st_as_sf(srd)
srd_ncount_sf<- sf::st_as_sf(srd_ncount)
a<- rbind(
  cbind(srd_ncount_sf %>% mutate(Doseljeno=Doseljeno1991, Popup=Popup1991), Godina=1991),
  cbind(srd_ncount_sf%>% mutate(Doseljeno=Doseljeno2002, Popup=Popup2002), Godina=2002),
  cbind(srd_ncount_sf%>% mutate(Doseljeno=Doseljeno2011, Popup=Popup2011), Godina=2011)
)

plot_ly() %>% 
  add_sf(
    data = a, 
    color = ~rev(Doseljeno),
    split = ~QNAME,
    frame =~Godina,
    size= ~Doseljeno,
    colors="RdBu",
    text = ~paste0("<b>",gsub(QNAME, pattern = 'ki okrug', replacement = "ka oblast"),"</b>:\n",Popup), 
    hoverinfo = "text", 
    hoveron = "fills",   alpha=1,
    stroke=I("grey90")
  ) %>%  layout(showlegend = FALSE) %>%
  animation_opts(frame = 1000) %>% animation_button(label = "Pokreni") %>% colorbar(title="Doseljeno\nstanovništvo (%)" )

