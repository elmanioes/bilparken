---
  title: "Audi"
author: "MV"
date: "6/8/2020"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Norgesbilparken 

# Carga de librerias:
  

library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(tidyverse)
library(raster)
library(sp)
library(rgdal)
library(readr)


# Data nedlasting:
# 01960: Registrerte kj�ret�y, etter statistikkvariabel og �r
# https://www.ssb.no/bilreg

veh_matric_1950_2020 <- read_delim("C:/mio/Big/estudios_mv/veh_matric_1950_2020.csv", 
                                   ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                   trim_ws = TRUE)
View(veh_matric_1950_2020)

# Norsk kj�ret�yregistrering grafen (1950-2020)

registreret <- veh_matric_1950_2020
registreret$vehic <- as.numeric(registreret$vehic)

registreret %>% filter (Typer == "Kj�ret�y i alt") %>% 
  dplyr::select(�r,vehic)%>%
  slice(seq(1,80,10),66:70)%>%
  spread(�r, vehic)%>% #transponer la tabla
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  row_spec(row = 0, background="green", bold=T, color="white")

registreret %>% filter (Typer == "Kj�ret�y i alt") %>% 
  ggplot (aes(x=�r, y=vehic/1000)) + 
  labs(title = "Registreret i Norge", x="�r", y= "kj�ret�y (i tusen)")+
  geom_line(alpha = 0.5, size = 1.5, color = "blue")+
  scale_y_continuous(breaks=seq(0,6000,by=1000))+
  scale_x_continuous(breaks=seq(1950,2020,by=10))


# Registrert i Norge etter kj�ret�ytype

# Graf

registreret %>% filter (Typer != "Kj�ret�y i alt") %>%
  group_by (Typer, �r ) %>%
  mutate_all(~replace(., is.na(.), 0))%>%
  #summarize(sum_veh = sum(vehic)) %>%
  ggplot (aes(x=�r, y=vehic, color = Typer)) + 
  labs(title = "Registreret i Norge per typer", x="�r", y= "kj�ret�y")+
  geom_line(alpha = 0.5, size = 1.5)+
  scale_y_continuous(breaks=seq(0,3000000,by=500000))+
  scale_x_continuous(breaks=seq(1950,2025,by=10))

# Tabell

Total_2019 <- registreret %>% 
  filter (Typer != "Kj�ret�y i alt" & �r == 2019)%>%
  summarise(sum(vehic, na.rm = TRUE ))

registreret %>% filter (Typer != "Kj�ret�y i alt" & �r == 2019) %>%
  group_by (Typer, �r ) %>%
  arrange (desc(vehic)) %>%
  # summarise(total(sum(vehic, na.rm = TRUE))) %>%
  # mutate(prosent = paste0(round(100*vehic / total ,2),"%"))
  #mutate(prosent = paste0(round(100*vehic /sum(vehic, na.rm = TRUE),2),"%")) %>%
  mutate(prosent = paste0(round(100*vehic / Total_2019,2),"%")) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  column_spec(column = 3, width = "2cm", background = "orange", color = "black") %>%
  row_spec(row = 0, background="green", bold=T, color="white")

# Total Registrert etter kommuner i 2019 i Norge

# Data nedlasting
# 11823: Registrerte kj�ret�y, etter region, drivstofftype, statistikkvariabel og �r
# https://www.ssb.no/bilreg

library(readxl)
comb_kom_2019 <- read_excel("C:/mio/Big/estudios_mv/comb_kom_2019.xlsx", 
                            sheet = "Hoja1")
View(comb_kom_2019)

# Tabell

kommune <- comb_kom_2019

kommune %>% 
  group_by(Kom) %>% 
  summarize(t2019 = sum(tall)) %>%
  arrange(desc(t2019)) %>%
  slice(1:10) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  row_spec(row = 0, background="green", bold=T, color="white") 



# Drivstofftype - 2019

kommune %>% 
  #filter(ar == 2019)%>%
  group_by(drivstofftype) %>% 
  summarize(total_anual = sum(tall))%>%
  ggplot(aes(reorder(drivstofftype,-total_anual),total_anual))+
  geom_bar(stat="identity" , fill = "blue")+
  labs(title = "Drivstofftype - �r 2019", x="Drivstoff", y= "kj�ret�y")+
  geom_text(aes(label = total_anual,  vjust=-1 ), size = 3)+
  theme(axis.text.x = element_text(angle = 30, hjust=1, size = 9))+
  scale_y_continuous(limit = c(0,2500000), breaks=seq(0,2500000,by=500000))


# Diesel Kjoretoy

kommune %>% 
  filter(drivstofftype == "Diesel" | drivstofftype == "Diesel hybrid, ladbar" |drivstofftype == "Diesel hybrid, ikke ladbar" )%>%
  group_by(kj�ret�y) %>% 
  summarize(total_anual = sum(tall))%>%
  ggplot(aes(reorder(kj�ret�y ,-total_anual),total_anual))+
  geom_bar(stat="identity" , fill = "blue")+
  labs(title = "Diesel Kj�ret�y  - �r 2019", x="Kj�ret�y typer", y= "kj�ret�y")+
  geom_text(aes(label = total_anual,vjust=-1 ), size = 3)+
  theme(axis.text.x = element_text(angle = 30, hjust=1, size = 9))+
  scale_y_continuous(limit = c(0,1500000), breaks=seq(0,1500000,by=250000))

# Bilmarker
# Data nedlasting:
# 08581: Registrerte person- og varebiler, etter bilmerke, statistikkvariabel og �r
# https://www.ssb.no/bilreg


marker_bil_2019 <- read_delim("C:/mio/Big/estudios_mv/marker_bil_2019.csv", 
                              ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                              trim_ws = TRUE)
View(marker_bil_2019)

#Bilmerker

# Audi i Norge

marker <- marker_bil_2019
colnames(marker)

names(marker) <- c("bilmerke", "alder", "personbiler2019", "varebiler2019")

# Tabell

marker %>%
  group_by(bilmerke)%>%
  summarise(total_bil= sum(personbiler2019))%>%
  filter(total_bil > 100000) %>%
  mutate(rank = rank(-total_bil))%>% 
  arrange(desc(total_bil)) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  row_spec(row = 0, background="green", bold=T, color="white") %>%
  row_spec(row = 7, background="yellow", bold=T, color="black")


# Graf

marker %>%
  group_by(bilmerke)%>%
  summarise (tot_merke = sum(personbiler2019)) %>% 
  filter (tot_merke >20000)%>% 
  arrange(desc(tot_merke)) %>%
  ggplot(aes(reorder(bilmerke,-tot_merke),tot_merke, fill = bilmerke == "Audi"))+
  scale_fill_manual(values = c("blue", "red"))+
  geom_bar(stat="identity") +
  guides(fill=FALSE)+
  labs(title = "Marker Kj�ret�y  - �r 2019", x="Merker", y= "kj�ret�y")+
  theme(axis.text.x = element_text(angle = 30, hjust=1, size = 9))

# Alder p� personbiler i Norge (2019)

marker$alder <- as.factor(marker$alder)
#levels(marker$alder)
# Para cambiar este orden usamos la funcion "factor".
marker$alder = factor(marker$alder, levels=c("Under 4 �r", "4 - 7 �r", "8 - 11 �r", "12 - 15 �r", "16 - 20 �r", "Over 20 �r"))
#levels(marker$alder)

# Audi og alle andre merker


marker %>%
  dplyr::select(-varebiler2019)%>%
  group_by(alder, bilmerke)%>%
  summarise(total_bil= sum(personbiler2019))%>%
  filter(total_bil > 12000) %>%
  ggplot(aes(x= alder, y = total_bil, color = bilmerke, size= bilmerke == "Audi"))+
  #scale_color_manual(values = c("blue", "red"))+
  geom_point() +
  guides(size=FALSE)+
  labs(title = "Bilparkens Alder  - �r 2019", x="Alder", y= "kj�ret�y")+
  theme(axis.text.x = element_text(angle = 30, hjust=1, size = 9))
#scale_y_continuous(limit = c(0,100000), breaks=seq(0,100000,by=10000))
#+geom_jitter() 

# Audi-biler registrert i Norge fra perioden solgt mellom 2009 og 2015

marker %>%
  filter (bilmerke =="Audi")%>%
  group_by(alder)%>%
  dplyr::select(alder,personbiler2019) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10)%>%
  row_spec(row = 0, background="green", bold=T, color="white") %>%
  row_spec(row = 2:3, background="yellow", bold=T, color="black") 

marker %>%
  filter (bilmerke =="Audi")%>%
  group_by(alder)%>%
  dplyr::select(alder,personbiler2019) %>%
  ggplot(aes(alder, personbiler2019, size = 15,
             color = (alder == "4 - 7 �r"  | alder == "8 - 11 �r")))+
  geom_point() +
  #scale_color_manual(values = c("blue", "red"))+
  scale_colour_manual("gases", labels = c("ok", "problem"),values = c("green","red"))+ 
  guides(size=FALSE)+
  labs(title = "Audis Alder - �r 2019", x="Alder", y= "biler", subtitle = "problemas de gases")

# Estudio de vehiculos per capita ?????

kommuner_fylker <- read_excel("C:/mio/Big/estudios_mv/kommuner_fylker.xlsx")
# View(kommuner_fylker)

# 11342: Areal og befolkning, etter region, statistikkvariabel og �r

Folkemengde_2020 <- read_excel("C:/mio/Big/estudios_mv/Folkemengde_2020.xlsx")
# View(Folkemengde_2020)

# Unir_tablas

kf_befolk <- merge(Folkemengde_2020, kommuner_fylker, by.x="kodekomm", by.y= "Kommunenummer", all.x=TRUE)

kf_befolk_bilpark <- merge (kommune, kf_befolk, by.x="Kom", by.y="Kommunar", all.x = TRUE)

# coches por habitante

kf_befolk_bilpark %>% 
  group_by(Kom) %>% 
  summarize(K2019= sum(tall),
            Kpc= round(sum(tall/Befolkning_2020),2),
            Bef_k= mean(Befolkning_2020)) %>%
  dplyr::select(Kom,K2019,Bef_k, Kpc)%>%
  arrange(desc(Kpc)) %>%
  slice(1:10) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  row_spec(row = 0, background="green", bold=T, color="white") 

# Coches por habitante en poblaciones grandes

kf_befolk_bilpark %>% 
  group_by(Kom) %>% 
  summarize(K2019= sum(tall),
            Kpc= round(sum(tall/Befolkning_2020),2),
            Bef_k= mean(Befolkning_2020)) %>%
  filter(Bef_k > 100000) %>%
  dplyr::select(Kom,K2019,Bef_k, Kpc)%>%
  arrange(desc(Kpc)) %>%
  slice(1:10) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  row_spec(row = 0, background="green", bold=T, color="white")

# Archivos conjunto bueno
# write.csv(kf_befolk_bilpark, "C:/mio/Big/estudios_mv/tabla1.csv")

# Coches por habitante por Fylke

befolk_F <- kf_befolk %>% group_by(Fylke) %>% summarize (bef_f = sum(Befolkning_2020))

befolk_Norge <- sum(befolk_F$bef_f) # comprobacion poblacion de noruega en 2019 (5367580 innbyggere)

## vehiculos per capita per capita

kf_befolk_bilpark %>% 
  group_by(Fylke) %>% 
  left_join(befolk_F)%>%
  summarize(K2019= sum(tall),
            Bef_k= mean(bef_f),
            Kpc= round(K2019/Bef_k,2)) %>%
  #filter(Bef_k > 100000) %>%
  dplyr::select(Fylke,K2019,Bef_k, Kpc)%>%
  arrange(desc(Kpc)) %>%
  slice(1:11) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  row_spec(row = 0, background="green", bold=T, color="white")

#Presentacion con datos geogr�ficos

library(sf)

gdb_norge_F <-  st_read("C:/mio/Big/SIG/Norge/FGDB/Basisdata_0000_Norge_25833_Fylker_FGDB.gdb/a0000000e.gdbtable")

gdb_norge_F$objid_f <- as.factor(gdb_norge_F$objid)

# Preparacion datos gdb (no aparecen el nombre de comunas)

fylke <- kf_befolk_bilpark %>% drop_na() %>% distinct(Fylke)

fylke_datos <- kf_befolk_bilpark %>% 
  group_by(Fylke) %>% 
  summarize( K2019 = sum(tall))%>%
  drop_na() %>%
  left_join(befolk_F, by = "Fylke")%>% 
  mutate(Kpc= K2019/bef_f)


gdb_norge_F$Fylke  <- as.factor(c("Vestfold og Telemark","Agder","Viken","Rogaland","Nordland","M�re og Romsdal","Vestland" ,"Oslo" , "Tr�ndelag - Tr��ndelage", "Troms og Finnmark - Romsa ja Finnm�rku - Tromssa ja Finmarkku" , "Innlandet"))


gdb_norge_datos <- gdb_norge_F %>% 
  left_join(fylke_datos)

ggplot(data = gdb_norge_datos)+
  geom_sf(aes(fill = Fylke), show.legend = FALSE)+
  facet_wrap(~ Fylke)


# Graficos pro kommuner

gdb_norge_K <-  st_read("C:/mio/Big/SIG/Norge/FGDB/Basisdata_0000_Norge_25833_Kommuner_FGDB.gdb/a0000000e.gdbtable")

class(gdb_norge_K)

ggplot(data = gdb_norge_K)+
  geom_sf(aes(fill = kommunenummer), show.legend = FALSE)


# Repasemos las cifras de vehiculos por c�pita por Kommune

kf_befolk_bilpark %>% 
  group_by(Kom) %>% 
  summarize(K2019= sum(tall),
            Kpc= round(sum(tall/Befolkning_2020),2),
            Bef_k= mean(Befolkning_2020)) %>%
  dplyr::select(Kom,K2019,Bef_k, Kpc)%>%
  arrange(desc(Kpc)) %>%
  slice(1:10) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  row_spec(row = 0, background="green", bold=T, color="white")

# Vamos a analizar la kommuna con m�s vehiculos

kf_befolk_bilpark %>%
  filter(Kom =="�seral")%>%
  group_by(kj�ret�y) %>% 
  summarize(K2019= sum(tall),
            Kpc= round(sum(tall/Befolkning_2020),2),
            Bef_k= mean(Befolkning_2020)) %>%
  mutate(prosent = paste0(round(100*K2019 / sum(K2019),2),"%")) %>%
  arrange(desc(K2019)) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  row_spec(row = 0, background="green", bold=T, color="white")


# Conclusi�n: En esta poblacion hay muchos tractores y muchas motonieves

# Datos por Kommunas

colnames(gdb_norge_K)

kf_befolk_bilpark <- kf_befolk_bilpark %>% 
  mutate(kodekomm = as.character(kodekomm))

kommuner_datos <- kf_befolk_bilpark %>%
  group_by(Kom, kj�ret�y) %>%
  dplyr::select(Kom,kodekomm, kj�ret�y, tall, Befolkning_2020  )%>%
  rename(kommunenummer = kodekomm)

gdb_norge_datos_K <- gdb_norge_K %>% 
  left_join(kommuner_datos)

# Estadisticas sobre mapas

# Calcularemos donde existen m�s motos de nieve (en n�mero) y porcentaje sobre las existentes en Noruega

kf_befolk_bilpark %>%
  group_by(Kom) %>%
  filter(kj�ret�y == "Beltemotorsykler")%>%
  summarize(K2019= sum(tall),
            Kpc= round(sum(tall/Befolkning_2020),2),
            Bef_k= mean(Befolkning_2020)) %>%
  mutate(prosent = paste0(round(100*K2019 / sum(K2019),2),"%")) %>%
  arrange(desc(K2019)) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  row_spec(row = 0, background="green", bold=T, color="white")

# Calcularemos m�s motos de nieve per capita

kf_befolk_bilpark %>%
  group_by(Kom) %>%
  filter(kj�ret�y == "Beltemotorsykler")%>%
  summarize(K2019= sum(tall),
            Kpc= round(sum(tall/Befolkning_2020),2),
            Bef_k= mean(Befolkning_2020)) %>%
  mutate(prosent = paste0(round(100*K2019 / sum(K2019),2),"%")) %>%
  arrange(desc(Kpc)) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  row_spec(row = 0, background="green", bold=T, color="white")

#Graficos

#Komunner con mas de 1000 motosdenieve

ggplot(gdb_norge_datos_K %>% filter(kj�ret�y == "Beltemotorsykler") %>% filter(tall > 1000)) +   geom_sf(aes(fill = tall)) 

# Kommuner con m�s de 1000 traktorer

ggplot(gdb_norge_datos_K %>% filter(kj�ret�y == "Traktorer") %>% filter(tall > 1000)) +   geom_sf(aes(fill = tall)) 


# Por porcentaje sobre el total de vehiculos
# Prepararemos una tabla ancha por komunas donde calcularemos el porcentaje que hay de cada vehiculo

k_ancha <- kf_befolk_bilpark %>%
  group_by(Kom, kj�ret�y)%>%
  summarize(kj = sum(tall))%>%
  spread(key   = kj�ret�y, #key son los nombres de columnas
         value = kj) #los valores con que se llenan las celdas %>% 


## comprobacion de valores NA

row.has.na <- apply(k_ancha, 1, function(x){any(is.na(x))})
sum(row.has.na) # No existen NA

# Vamos a incluir una columna de totales de vehiculos por kommuna

k_ancha <- as.data.frame(k_ancha) %>% 
  mutate(tot_K = rowSums(.[2:13]))

# Porcentajes del turismos, traktores y motos de nieve sobre el total de vehiculos de la comuna

k_ancha$pre_pb = round(k_ancha$Personbiler/k_ancha$tot_K*100,1)
k_ancha$pre_tr = round(k_ancha$Traktorer/k_ancha$tot_K*100,1)
k_ancha$pre_bm = round(k_ancha$Beltemotorsykler/k_ancha$tot_K*100,1)

# Poblaciones con mayor porcentaje de traktores sobre el total de vehiculos en la kommuna

k_ancha %>% dplyr::select(Kom, pre_tr )%>% 
  arrange(desc(pre_tr)) %>% 
  slice(1:10) %>% 
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  row_spec(row = 0, background="green", bold=T, color="white")


# Poblaciones con mayor porcentaje de motos de nieve

k_ancha %>% dplyr::select(Kom, pre_bm )%>% 
  arrange(desc(pre_bm)) %>% 
  slice(1:10) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center", font_size = 10) %>%
  row_spec(row = 0, background="green", bold=T, color="white")

# Carga de ficheros .shape de QGIS

kommunerV <- shapefile("C:/mio/Big/SIG/Norge/FGDB/Basisdata_0000_Norge_25833_Kommuner_FGDB.gdb/kommuner.shp")

fylkerV <- shapefile("C:/mio/Big/SIG/Norge/FGDB/Basisdata_0000_Norge_25833_Kommuner_FGDB.gdb/fylker.shp")

# informacion componente espacial

geometry(kommunerV)
geometry(fylkerV)

# Tablas de datos

#Poblacion_area_densidad_K

Folkemengde_2020 <- read_excel("C:/mio/Big/estudios_mv/Folkemengde_2020.xlsx")
View(Folkemengde_2020)

# Relacion Komunner y Fylker

kommuner_fylker <- read_excel("C:/mio/Big/estudios_mv/kommuner_fylker.xlsx")
View(kommuner_fylker)

# drivstoff y kjoretoy_K
comb_kom_2019 <- read_excel("C:/mio/Big/estudios_mv/comb_kom_2019.xlsx", 
                            sheet = "Hoja1")
View(comb_kom_2019)

# Unir_tablas

kf_befolk <- merge(Folkemengde_2020, kommuner_fylker, by.x="kodekomm", by.y= "Kommunenummer", all.x=TRUE)

# Comprobaciones de perdida de datos

nrow(distinct(kf_befolk,kodekomm)) # numero de valores distintos 356

nrow(distinct(comb_kom_2019,kod)) # codigos 359

# vamos a separar la columna Kom en 2 dejando solo el codigo despues de "-" 

comb_kom_2019 <- separate(data = comb_kom_2019, col = kod, into = c("letra", "kodekomm"), sep = "\\-")

nrow(distinct(comb_kom_2019,kodekomm)) # codigos 359

comb_kom_2019$kodekomm <- as.character(comb_kom_2019$kodekomm)

kf_befolk_bilpark <- merge (comb_kom_2019, kf_befolk, by="kodekomm")

nrow(distinct(kf_befolk_bilpark,kodekomm)) # numero de valores distintos 355

# tabla de datosV

datosV <- kf_befolk_bilpark %>%
  group_by(Kom, kj�ret�y) %>%
  rename(kommunenum = kodekomm)

length(unique(datosV$kommunenum)) # numero de valores distintos 355 !!!!!

# Vamos a comprobar que observaciones tienen NA

datosV_con_NA <- datosV[!complete.cases(datosV),]

# Existen registros de vehiculos con la denominaci�n "Delte kommuner og uoppgitt".

# Para eliminar todos los registros con NA: datosV <- na.omit(datosV)

# Vehiculos totales por kommuna

kj_tab <- datosV %>% group_by(Kom, kommunenum, Fylke, Befolkning_2020) %>% summarize (total_kj = sum(tall))

Kj_K_V <- merge (kommunerV,
                 kj_tab,
                 by="kommunenum")

# Poblaciones con m�s de 20.000 vehiculos

over20M <- subset(Kj_K_V, subset = total_kj>20000)

plot (kommunerV, axes=TRUE)
plot (over20M, col ="darkred", add=TRUE)

# Vehiculos per capita

# Creamos la variable: densidad de vehiculos per capita

Kj_K_V$kjpc = Kj_K_V$total_kj/Kj_K_V$Befolkning_2020

hist(Kj_K_V$kjpc)

# Conversion a factor

Kj_K_V$kjpc_f <- case_when(
  Kj_K_V$kjpc <= 0.5 ~ "< 0.5",
  Kj_K_V$kjpc <= 1 ~ "0.5 - 1.0",
  Kj_K_V$kjpc <= 1.5 ~ "1.0 - 1.5", 
  Kj_K_V$kjpc > 1.5 ~ " > 1.5")

table(Kj_K_V$kjpc_f)
Kj_K_V$kjpc_f <- as.factor(Kj_K_V$kjpc_f)
levels( Kj_K_V$kjpc_f )    # a, b, c

Kj_K_V$kjpc_f <- factor(Kj_K_V$kjpc_f, 
                        levels= levels(Kj_K_V$kjpc_f)[c(2,3,4,1)])

plot(Kj_K_V$kjpc_f, main = "Gr�fico de barras",
     xlab = "Densidad", ylab = "Frecuencia")

table(Kj_K_V$kjpc_f)

# Grafica 

plot (kommunerV, axes=TRUE)
plot (Kj_K_V,
      col = c("green","yellow","orange","red")[Kj_K_V$kjpc_f],
      add=TRUE)
legend ("bottomleft", inset=.05,title = expression("Densidad (veh/pers)"), legend = c("< 0.5","0.5 - 1.0","1.0 - 1.5", " > 1.5" ), fill = c("green","yellow","orange","red"), Kj_K_V$kjpc_f, cex=0.8, box.col = NA)


# Plano comunas con menos y m�s coches por habitante

mindre <- subset(Kj_K_V,kjpc < 0.5 )
mer <- subset(Kj_K_V,kjpc > 1.5 )

par(mfrow = c(1,2))
plot (kommunerV, axes=TRUE, main = "menos vehiculos por habitante", cex.main =0.5)
plot (mindre, col = "green", add=TRUE)

plot (kommunerV, axes=TRUE, main = "mas vehiculos por habitante", cex.main=0.5)
plot (mer, col = "red", add=TRUE)

# Fylke: Vestland

KjpcVestland <- subset(Kj_K_V,Fylke == "Vestland")

plot (KjpcVestland,
      col = c("green","yellow","orange","red")[KjpcVestland$kjpc_f],
      axes=TRUE, main = "Vestland")
legend ("bottomleft", inset=.05,title = expression("Densidad (veh/pers)"), legend = c("< 0.5","0.5 - 1.0","1.0 - 1.5", " > 1.5" ), fill = c("green","yellow","orange","red"), KjpcVestland$kjpc_f, cex=0.8, box.col = NA)

# Fylke: Viken

KjpcViken <- subset(Kj_K_V, Fylke == "Viken")

plot (KjpcViken,
      col = c("green","yellow","orange","red")[KjpcViken$kjpc_f],
      axes=TRUE, main = "Viken")
legend ("bottomleft", inset=.05,title = expression("Densidad (veh/pers)"), legend = c("< 0.5","0.5 - 1.0","1.0 - 1.5", " > 1.5" ), fill = c("green","yellow","orange","red"), KjpcViken$kjpc_f, cex=0.8, box.col = NA)

# Fylke: Troms og Finnmark

KjpcTFN <- subset(Kj_K_V, Fylke == "Troms og Finnmark - Romsa ja Finnm�rku - Tromssa ja Finmarkku" | Fylke == "Nordland")

plot (KjpcTFN,
      col = c("green","yellow","orange","red")[KjpcTFN$kjpc_f],
      axes=TRUE, main = "Troms, Finnmark y Nordland")
legend ("bottomright", inset=.05,title = expression("Densidad (veh/pers)"), legend = c("< 0.5","0.5 - 1.0","1.0 - 1.5", " > 1.5" ), fill = c("green","yellow","orange","red"), KjpcTFN$kjpc_f, cex=0.8, box.col = NA)

# Fylke: Innlandet

KjpcInn <- subset(Kj_K_V, Fylke == "Tr�ndelag - Tr��ndelage")

plot (KjpcInn,
      col = c("green","yellow","orange","red")[KjpcInn$kjpc_f],
      axes=TRUE, main = "Tr�ndelag")
legend ("bottomright", inset=.05,title = expression("Densidad (veh/pers)"), legend = c("< 0.5","0.5 - 1.0","1.0 - 1.5", " > 1.5" ), fill = c("green","yellow","orange","red"), KjpcInn$kjpc_f, cex=0.8, box.col = NA)

# Por tipo de vehiculos: Beltemotorsykler

# Beltemotorsykler i Finnmark

kj_BM_K <- datosV %>% filter (kj�ret�y == "Beltemotorsykler") %>%    group_by(Kom, kommunenum, Fylke, Befolkning_2020) %>% summarize (total_bm = sum(tall))

Kj_BM_K_V <- merge (kommunerV,
                    kj_BM_K,
                    by="kommunenum")


BM_TFN <- subset(Kj_BM_K_V, Fylke == "Troms og Finnmark - Romsa ja Finnm�rku - Tromssa ja Finmarkku")

BM_TFN1000 <- subset(Kj_BM_K_V, Fylke == "Troms og Finnmark - Romsa ja Finnm�rku - Tromssa ja Finmarkku" & total_bm > 1000)

plot(BM_TFN, axes = TRUE, main = "Beltemotorsykler i Troms, Finnmark")
text(BM_TFN1000, BM_TFN1000$total_bm, col = "red", cex = 0.5)


# Traktorer i Finnmark

kj_Trak_K <- datosV %>% filter (kj�ret�y == "Traktorer") %>%    group_by(Kom, kommunenum, Fylke, Befolkning_2020) %>% summarize (total_tr = sum(tall))

Kj_Trak_K_V <- merge (kommunerV,
                      kj_Trak_K,
                      by="kommunenum")


TR_TFN <- subset(Kj_Trak_K_V, Fylke == "Troms og Finnmark - Romsa ja Finnm�rku - Tromssa ja Finmarkku")

TR_TFN1000 <- subset(Kj_Trak_K_V, Fylke == "Troms og Finnmark - Romsa ja Finnm�rku - Tromssa ja Finmarkku" & total_tr > 1000)

plot(TR_TFN, axes = TRUE, main = "Traktorer i Troms, Finnmark")
text(TR_TFN1000, TR_TFN1000$total_tr, col = "red", cex = 0.5)





