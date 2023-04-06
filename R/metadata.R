#'
#' functions to get receiver and tagging metadata into session
#'
#' @param detections is an object in your R session created by read_tb
#' @export

meta<-gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1ht4dk480HDm5a6Eop1eGKOnQNeGXJnKb7FnFBc9dMGA/edit#gid=0')

meta<-meta %>%
  dplyr::mutate(ID=as.numeric(ID)) %>%
  dplyr:distinct(Vendor, ID, n_ID) %>%
  dplyr:mutate(n_ID=n_ID-1) %>%
  dplyr:mutate(ID2=dplyr:case_when(n_ID==0 ~ NA_real_,
                                   n_ID>0 ~ ID+1)) %>%
  dplyr: mutate(ID3=dplyr:case_when(n_ID>=2 ~ ID+2,
                                    T~NA_real_)) %>%
  dplyr:mutate(ID4=dplyr:case_when(n_ID==3 ~ ID+3,
                                   T~NA_real_)) %>%
  dplyr:mutate(ID3=dplyr:case_when(n_ID>=2 ~ ID+2,
                                   T~NA_real_)) %>%
  dplyr:mutate(oid=ID) %>%
  dplyr::select(-n_ID) %>%
  tidyr::gather(key, value, -oid, -Vendor) %>%
  dplyr::rename(ID=oid) %>%
  dplyr:right_join(meta %>% dplyr:mutate(ID=as.numeric(ID))) %>%
  dplyr:mutate(key=case_when(grepl("-AT", Transmitter) &
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

receiver_locations<-seq.Date(as.Date("2020-01-01"),
                             as.Date(Sys.Date()), by="day") %>%
  tidyr::as_tibble %>%
  tidyr::expand_grid(., rec) %>%
  dplyr:mutate(end=dplyr::case_when(end=="" |
                                      is.na(end)~ Sys.Date(), T~dmy(end))) %>%
  dplyr::filter(value>lubridate::dmy(start) & value<end) %>%
  dplyr::select(value, Receiver, Station, Habitat, depth, sync, lon, lat) %>%
  dplyr::rename(dti=value)

create_detections<-function(detections) {
  detections %>%
    dplyr::left_join(meta %>%
                       dplyr::mutate(ID=as.integer(ID)) %>%
                       dplyr::select(ID, oid, dmy, sensor, Spp, TL, Angler,
                                     fate, fatedate,
                                     Project, Transmitter, "Capture site", "Release Site"),
                     by="ID")}
