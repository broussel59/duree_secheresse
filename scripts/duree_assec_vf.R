library(tidyverse)
library(data.table)
library(dplyr)
library (sf)

onde <- fread("D:/R/ONDE_page_web/durée assec/raw_data/onde_france_2021.csv")

head(onde)

onde_vf <- onde %>%
  
  dplyr::select("departement"= "<CdDepartement>","code_station"="<CdSiteHydro>","date"="<DtRealObservation>","typologie nationale"= "<LbRsObservationNat>","X"="<CoordXSiteHydro>", "Y"="<CoordYSiteHydro>") %>%
   mutate(date = as.Date(date, format = "%d/%m/%Y"))



duree_assec <- onde_vf %>%
    arrange(code_station, date,departement,X,Y) %>%
    mutate(date_assec = date %>%
           (function(x) {x[`typologie nationale` != "Assec"] <- NA 
           x})) %>%
    group_by(code_station,departement,X,Y) %>%
    mutate( duree_assec = difftime(date_assec,lag(date_assec),
      units = "days") %>%
      as.numeric() %>%
         (function(x) {x[is.na(x)]  <-  0
        cumsum(x)})) %>%
  
  summarise(duree_assec = max(duree_assec))

duree_assec_spat <- sf :: st_as_sf(
  duree_assec,
  coords = c("X", "Y"),
  crs = 2154,
  remove = FALSE)


st_write(duree_assec_spat,
     "output/duree_assec_2021.gpkg")

test<-1

