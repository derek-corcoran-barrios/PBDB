library(tidyverse)
library(lubridate)
library(scales)
library(Cairo)
library(sf)
library(rgdal)




All <- read_csv("https://paleobiodb.org/data1.2/occs/list.csv?datainfo&rowcount&interval=rhaetian,sinemurian&show=ecospace,coords,refattr,abund,attr,crmod,geo,ent,entbane,ident,lith,loc,paleoloc,phylo,prot,ref,rem,strat,stratext,time&lngmin=-37.68&lngmax=71.67&latmin=11.91&latmax=60", skip = 22) %>% mutate(Binomial = paste(genus, species_name))

All2 <- All %>% filter(phylum %in% c("Annelida", "Arthropoda", "Brachiopoda", "Bryozoa", "Chlorophyta", "Chordata", "Cnidaria", "Cyanobacteria",
                                     "Echinodermata", "Foraminifera", "Haptophyta", "Hemichordata", 
                                     "Mollusca", "Nematoda", "Porifera", 
                                     "Problematica", "Retaria", "Rhodophyta") | is.na(phylum)) %>% 
  filter(phylum %in% c("Annelida", "Brachiopoda", "Bryozoa", "Chlorophyta", "Cnidaria", "Cyanobacteria", "Mollusca", "Nematoda", "Porifera", 
                                     "Problematica", "Retaria", "Rhodophyta") | 
           is.na(phylum) | 
           (phylum == "Arthropoda" & class %in% c("Ostracoda", "Malacostraca", "Thylacocephala") | is.na(class)) | 
           (phylum == "Chordata" & class %in% c("Reptilia", "Actinopteri", "Chondrichthyes", "Osteichthyes", "Conodonta", "Actinopterygii", 
                                            "Coelacanthimorpha", "Echinodermata", "Foraminifera", "Haptophyta", "Hemichordata", 
                                            "Mollusca", "Nematoda", "Porifera", 
                                            "Problematica", "Retaria", "Rhodophyta") | is.na(class))) %>%  filter(order != "Pterosauria" | is.na(order))
        

All3 <- All2 %>% filter(environment %in% c("marine indet.", "open shallow subtidal", "deep subtidal ramp","reef, buildup or bioherm", "coarse channel fill", "delta plain", "estuary/bay", "carbonate indet.", "lagoonal/restricted shallow subtidal", "basinal (carbonate)","offshore ramp", "lagoonal", "shallow subtidal indet.", "sand shoal", "perireef or subreef", "offshore shelf", "offshore","tar", "basinal (siliciclastic)", "coastal indet.", "deep subtidal shelf", "foreshore", "marginal marine indet.", "deep subtidal indet.", "offshore indet.","paralic indet.", "\"channel\"", "basinal (siliceous)", "transition zone/lower shoreface", "fine channel fill", "deltaic indet.", "shoreface", "peritidal", "slope", "basin reef", "prodelta", "submarine fan") | is.na(environment)) 

Summ <- All %>% group_by(phylum) %>% summarise(n = n()) %>% arrange(desc(n))
Summ2 <- All2 %>% group_by(phylum) %>% summarise(n = n()) %>% arrange(desc(n))
Summ3 <- All3 %>% group_by(phylum) %>% summarise(n = n()) %>% arrange(desc(n))
cbind(Summ2, Summ3)

ages <- data.frame(Age = c("Rhaetian", "Hettangian", "Sinemurian"), Max_lim = c(208.5,201.3,199.3), Min_lim = c(201.3,199.3,190.8)) %>% mutate(duration = Max_lim - Min_lim)

Rahetian <- All3 %>% filter(min_ma >= ages$Min_lim[1] & max_ma <= ages$Max_lim[1]| 
                              min_ma >= ages$Min_lim[1] & max_ma > ages$Max_lim[1] & min_ma < ages$Max_lim[1]|
                              min_ma < ages$Min_lim[1] & max_ma <= ages$Max_lim[1] & max_ma > ages$Min_lim[1]|
                              min_ma < ages$Min_lim[1] & max_ma > ages$Max_lim[1])

Hettangian <-All3 %>% filter(min_ma >= ages$Min_lim[2] & max_ma <= ages$Max_lim[2]| 
                               min_ma >= ages$Min_lim[2] & max_ma > ages$Max_lim[2] & min_ma < ages$Max_lim[2]|
                               min_ma < ages$Min_lim[2] & max_ma <= ages$Max_lim[2] & max_ma > ages$Min_lim[2]|
                               min_ma < ages$Min_lim[2] & max_ma > ages$Max_lim[2])

Sinemurian <-All3 %>% filter(min_ma >= ages$Min_lim[3] & max_ma <= ages$Max_lim[3]| 
                               min_ma >= ages$Min_lim[3] & max_ma > ages$Max_lim[3] & min_ma < ages$Max_lim[3]|
                               min_ma < ages$Min_lim[3] & max_ma <= ages$Max_lim[3] & max_ma > ages$Min_lim[3]|
                               min_ma < ages$Min_lim[3] & max_ma > ages$Max_lim[3])

#broom::tidy(lm(lng ~ paleolng, data = Sinemurian))
#broom::tidy(lm(lat ~ paleolat, data = Sinemurian))
#broom::tidy(lm(lng ~ paleolng, data = Hettangian))
#broom::tidy(lm(lat ~ paleolat, data = Hettangian))
#broom::tidy(lm(lng ~ paleolng, data = Rahetian))
#broom::tidy(lm(lat ~ paleolat, data = Rahetian))



newBBoxSinemurian <- data.frame(xmin = (-22.4688*0.852) -10.7, xmax = (92.5524*0.852) -10.7, ymin =(-32.6446*0.401) +34 ,ymax =(60.134*0.401) +34 )
newBBoxHettangian <- data.frame(xmin = (-25.5355 *0.832) -9.97, xmax = (82.4529 *0.832) -9.97, ymin =(-28.5603*0.588) +28.7 ,ymax =(53.1572*0.588) + 28.7)
newBBoxRahetian <- data.frame(xmin = (-22.8228*1.02) -14.4, xmax = (84.3805*1.02) -14.4, ymin =(-25.9298*0.171) +42.1 ,ymax =(45.6189*0.171) + 42.1)

newBBox <- bind_rows(newBBoxSinemurian, newBBoxHettangian, newBBoxRahetian)

####
Poligono_Sinemuriano <- read_table2("Poligono.Corrdenada.paleoThetys.Sinemuriano.txt", 
                                                           col_names = FALSE, col_types = cols(X3 = col_skip()))

Poligono_Sinemuriano <- st_sfc(st_polygon(x = list(as.matrix(Poligono_Sinemuriano[-nrow(Poligono_Sinemuriano),c(2,1)]))))
Poligono_Sinemuriano <- st_sf(data.frame(Epoca="Sinemuriano", geom=Poligono_Sinemuriano), crs = 4326)

SinemurianNA <- Sinemurian[is.na(Sinemurian$paleolat),]
SinemurianNOTNA <- Sinemurian[!is.na(Sinemurian$paleolat),]

ModeloSinlat <- lm(paleolat ~ lat, data = SinemurianNOTNA)
SinemurianNA$paleolat <- predict(ModeloSinlat, SinemurianNA)

ModeloSinlng <- lm(paleolng ~ lng, data = SinemurianNOTNA)
SinemurianNA$paleolng <- predict(ModeloSinlng, SinemurianNA)

Sinemurian <- bind_rows(SinemurianNA,SinemurianNOTNA)


Sinemurian_points_paleo <- st_as_sf(Sinemurian[!is.na(Sinemurian$paleolat),], coords = c("paleolng","paleolat"), crs = 4326)
Sinemurian_points_paleo2 <- st_intersection(Sinemurian_points_paleo, Poligono_Sinemuriano)

length(unique(Sinemurian_points_paleo2$Binomial))
#length(unique(Hettangian$Binomial))
#length(unique(Rahetian$Binomial))


ggplot() + geom_sf(data = Poligono_Sinemuriano) + geom_sf(data = Sinemurian_points_paleo2, color = "red", alpha = 0.5)

###


Poligono_Rhaetian <- read_table2("Poligono.Corrdenada.paleoThetys.Rhaetian.txt", 
                                                        col_names = FALSE, col_types = cols(X3 = col_skip()))


Poligono_Rhaetian <- st_sfc(st_polygon(x = list(as.matrix(Poligono_Rhaetian[-nrow(Poligono_Rhaetian),c(2,1)]))))
Poligono_Rhaetian <- st_sf(data.frame(Epoca="Rhaetian", geom=Poligono_Rhaetian), crs = 4326)


RahetianNA <- Rahetian[is.na(Rahetian$paleolat),]
RahetianNOTNA <- Rahetian[!is.na(Rahetian$paleolat),]

ModeloSinlat <- lm(paleolat ~ lat, data = RahetianNOTNA)
RahetianNA$paleolat <- predict(ModeloSinlat, RahetianNA)

ModeloSinlng <- lm(paleolng ~ lng, data = RahetianNOTNA)
RahetianNA$paleolng <- predict(ModeloSinlng, RahetianNA)

Rahetian <- bind_rows(RahetianNA,RahetianNOTNA)


Rahetian_points_paleo <- st_as_sf(Rahetian[!is.na(Rahetian$paleolat),], coords = c("paleolng","paleolat"), crs = 4326)
Rahetian_points_paleo2 <- st_intersection(Rahetian_points_paleo, st_buffer(Poligono_Rhaetian, 0))

ggplot() + geom_sf(data = Poligono_Rhaetian) + geom_sf(data = Rahetian_points_paleo2, color = "red", alpha = 0.5)


length(unique(Rahetian_points_paleo2$Binomial))
#####


Poligono_Hettangian <- read_table2("Poligono.Corrdenada.paleoThetys.Hettangian.txt", 
                                                          col_names = FALSE, col_types = cols(X3 = col_skip()))

Poligono_Hettangian <- st_sfc(st_polygon(x = list(as.matrix(Poligono_Hettangian[-nrow(Poligono_Hettangian),c(2,1)]))))
Poligono_Hettangian <- st_sf(data.frame(Epoca="Hettangian", geom=Poligono_Hettangian), crs = 4326)


HettangianNA <- Hettangian[is.na(Hettangian$paleolat),]
HettangianNOTNA <- Hettangian[!is.na(Hettangian$paleolat),]

ModeloSinlat <- lm(paleolat ~ lat, data = HettangianNOTNA)
HettangianNA$paleolat <- predict(ModeloSinlat, HettangianNA)

ModeloSinlng <- lm(paleolng ~ lng, data = HettangianNOTNA)
HettangianNA$paleolng <- predict(ModeloSinlng, HettangianNA)

Hettangian <- bind_rows(HettangianNA,HettangianNOTNA)


Hettangian_points_paleo <- st_as_sf(Hettangian[!is.na(Hettangian$paleolat),], coords = c("paleolng","paleolat"), crs = 4326)
Hettangian_points_paleo2 <- st_intersection(Hettangian_points_paleo, Poligono_Hettangian)



ggplot() + geom_sf(data = Poligono_Hettangian) + geom_sf(data = Hettangian_points_paleo2, color = "red", alpha = 0.5)

length(unique(Sinemurian_points_paleo2$Binomial))
length(unique(Hettangian_points_paleo2$Binomial))
length(unique(Rahetian_points_paleo2$Binomial))

write_csv(as.data.frame(Sinemurian_points_paleo2), "Sinemurian_points_paleo2.csv")
write_csv(as.data.frame(Hettangian_points_paleo2), "Hettangian_points_paleo2.csv")
write_csv(as.data.frame(Rahetian_points_paleo2), "Rahetian_points_paleo2.csv")


