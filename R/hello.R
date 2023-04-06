#libraries needed:
#data.table
#DBI

#function assumes datetime in logs is in UTC

usethis::use_package("usethis")
usethis::use_package("ggplot2")
usethis::use_package("magrittr")
usethis::use_package("lubridate")
usethis::use_package("googlesheets4")
usethis::use_package("sf")
usethis::use_package("dplyr")
usethis::use_package("data.table")
usethis::use_package("RSQLite")

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
    mutate(dt=lubridate::with_tz(dt_utc, "Europe/Oslo")) %>%
    dplyr::select(dt, dt_utc, epo, frac, serial, ID, Data) %>%
    dplyr::mutate(dti=round_date(dt, "10 mins")) %>%
    dplyr::left_join(sensors %>%
                       as_tibble %>%
                       dplyr::mutate(dt=dplyr::with_tz(dt_utc, "Europe/Oslo")) %>%
                       dplyr::select(dt, serial, temperature, noise) %>%
                       dplyr::mutate(dti=round_date(dt, "10 mins")) %>%
                       dplyr::select(-dt),
                     by=c("dti", "serial")) %>%
    dplyr::select(-dti)
  list(d=dets)

}
