#'
#' function to read in and manipulate Thelma DB
#'
#' @param tbdb_file is the name of the thelma database
#' @export

read_tb = function(tbdb_file) {
  tbrSerialNo <- tagID <- epo <- frac <- usTimestamp <- tagSNR <- NULL
  temperature <- tbrSerialNo <- ambientNoise <- ambientNoisePeak <- secTimestampUTC  <- NULL
  tagData <- dt_utc <- dt_sec_utc  <- NULL


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
   dplyr::mutate(dt=lubridate::with_tz(.data$dt_utc, "Europe/Oslo")) %>%
    dplyr::select(.data$dt, .data$dt_utc, .data$epo, .data$frac,
                  .data$serial, .data$ID, .data$Data) %>%
    dplyr::mutate(dti=lubridate::round_date(.data$dt, "10 mins")) %>%
    dplyr::left_join(sensors %>%
                       tidyr::as_tibble %>%
                       dplyr::mutate(dt=lubridate::with_tz(.data$dt_utc, "Europe/Oslo")) %>%
                       dplyr::select(.data$dt, .data$serial, .data$temperature, .data$noise) %>%
                       dplyr::mutate(dti=lubridate::round_date(.data$dt, "10 mins")) %>%
                       dplyr::select(-.data$dt),
                     by=c("dti", "serial")) %>%
    dplyr::select(-.data$dti)
  list(d=dets)

}


