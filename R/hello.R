#libraries needed:
#data.table
#DBI

#function assumes datetime in logs is in UTC

require(tidyverse)

theme_set(theme_classic()+
            theme(text=element_text(size=20),
                  axis.text=element_text(colour="black"),
                  legend.position="top",
                  legend.key.width=unit(3, "cm")))

read_tb = function(tbdb_file) {

  con = DBI::dbConnect(RSQLite::SQLite(), dbname = tbdb_file)

  #To see all available tables:
  #print(DBI::dbListObjects(con))

  detections = data.table::setDT(DBI::dbReadTable(con, 'TagReadings'))[,
                                                                       .(serial=tbrSerialNo,
                                                                         ID=tagID,
                                                                         epo = secTimestampUTC,
                                                                         Data=tagData,
                                                                         frac=usTimestamp/1000000,
                                                                         SNR=tagSNR
                                                                       )][,
                                                                          dt_utc := as.POSIXct(epo, origin = '1970-01-01', tz = 'UTC')]


  sensors = data.table::setDT(DBI::dbReadTable(con, 'TbrSensorReadings'))[,
                                                                          .(serial=tbrSerialNo,
                                                                            dt_sec_utc = secTimestampUTC,
                                                                            temperature = (temperature - 50) / 10,
                                                                            noise=ambientNoise,
                                                                            peak=ambientNoisePeak)
  ][, dt_utc := as.POSIXct(dt_sec_utc, origin = '1970-01-01', tz = 'UTC')]

  DBI::dbDisconnect(con)

  dets<- detections %>%
    mutate(dt=with_tz(dt_utc, "Europe/Oslo")) %>%
    dplyr::select(dt, dt_utc, epo, frac, serial, ID, Data) %>%
    mutate(dti=round_date(dt, "10 mins")) %>%
    left_join(sensors %>%
                as_tibble %>%
                mutate(dt=with_tz(dt_utc, "Europe/Oslo")) %>%
                dplyr::select(dt, serial, temperature, noise) %>%
                mutate(dti=round_date(dt, "10 mins")) %>%
                dplyr::select(-dt),
              by=c("dti", "serial")) %>%
    dplyr::select(-dti)
  list(d=dets)

}

library(googlesheets4); require(tidyverse); require(sf)

gs4_deauth()
rec<-read_sheet('https://docs.google.com/spreadsheets/d/18mUpHQkSBs5PKN2XqoOZEUXXjzUTJwyDx7qvU-0kgqs/edit#gid=1414140311') %>%
  as_tibble %>%
  dplyr::filter(!is.na(lon)) %>%
  st_as_sf(., coords=c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(32633) %>%
  as(., "Spatial") %>%
  as_tibble %>%
  dplyr::rename(lon=coords.x1, lat=coords.x2)

1
gs4_deauth()

meta<-read_sheet('https://docs.google.com/spreadsheets/d/1ht4dk480HDm5a6Eop1eGKOnQNeGXJnKb7FnFBc9dMGA/edit#gid=0')
m<-meta
meta<-meta %>%
  mutate(ID=as.numeric(ID)) %>%
  distinct(Vendor, ID, n_ID) %>%
  mutate(n_ID=n_ID-1) %>%
  mutate(ID2=case_when(n_ID==0 ~ NA_real_,
                       n_ID>0 ~ ID+1)) %>%
  mutate(ID3=case_when(n_ID>=2 ~ ID+2,
                       T~NA_real_)) %>%
  mutate(ID4=case_when(n_ID==3 ~ ID+3,
                       T~NA_real_)) %>%
  mutate(ID3=case_when(n_ID>=2 ~ ID+2,
                       T~NA_real_)) %>%
  mutate(oid=ID) %>%
  dplyr::select(-n_ID) %>%
  gather(key, value, -oid, -Vendor) %>%
  dplyr::rename(ID=oid) %>%
  right_join(meta %>% mutate(ID=as.numeric(ID))) %>%
  mutate(key=case_when(grepl("-AT", Transmitter) &
                         value-ID==0 ~ "temp",
                       grepl("A-LP", Transmitter) ~ "accel",
                       grepl("-AT", Transmitter) &
                         value-ID==1 ~ "accel",
                       grepl("-DT", Transmitter) &
                         value-ID==0 ~ "depth",
                       grepl("-DT", Transmitter) &
                         value-ID==1 ~ "temp",
                       grepl("MT-", Transmitter) &
                         value-ID==0 ~ "not eaten",
                       grepl("MT-", Transmitter) &
                         value-ID==1 ~ "eaten",
                       grepl("MT-", Transmitter) &
                         value-ID==2 ~ "temp",
                       grepl("MT-", Transmitter) &
                         value-ID==3 ~ "temp2",
                       grepl("-T", Transmitter) &
                         value-ID==0 ~ "temp",
                       grepl("-P", Transmitter) &
                         value-ID==0 ~ "not eaten",
                       grepl("-P", Transmitter) &
                         value-ID==1 ~ "eaten",
                       grepl("-ADT", Transmitter) &
                         value-ID==0 ~ "temp",
                       grepl("-ADT", Transmitter) &
                         value-ID==1 ~ "accel",
                       grepl("-ADT", Transmitter) &
                         value-ID==2 ~ "depth",
                       grepl("-DAT", Transmitter) &
                         value-ID==0 ~ "depth",
                       grepl("-DAT", Transmitter) &
                         value-ID==1 ~ "accel",
                       grepl("-DAT", Transmitter) &
                         value-ID==2 ~ "temp",
                       grepl("-R", Transmitter) &
                         value-ID==0 ~ "range",
                       grepl("-D", Transmitter) &
                         value-ID==0 ~ "depth",
                       !grepl("-", Transmitter)~"ID")) %>%
  dplyr::filter(!is.na(key) | Vendor=="Vemco") %>%
  dplyr::rename(sensor=key, oid=ID, ID=value)

# get coordinates right for receivers (UTM 33)
# get daily receiver locations to account for moves and additions
require(lubridate)
receiver_locations<-seq.Date(as.Date("2020-01-01"),
                             as.Date(Sys.Date()), by="day") %>%
  as_tibble %>%
  expand_grid(., rec) %>%
  mutate(end=case_when(end=="" |
                         is.na(end)~ Sys.Date(), T~dmy(end))) %>%
  dplyr::filter(value>dmy(start) & value<end) %>%
  dplyr::select(value, Receiver, Station, Habitat, depth, sync, lon, lat) %>%
  dplyr::rename(dti=value)

f<-function(x) {
  x %>%
    left_join(meta %>%
                mutate(ID=as.integer(ID)) %>%
                dplyr::select(ID, oid, dmy, sensor, Spp, TL, Angler,
                              fate, fatedate,
                              Project, Transmitter, "Capture site", "Release Site"),
              by="ID")}

