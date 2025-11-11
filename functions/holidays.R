
# https://www.thelocal.dk/20220103/what-public-holidays-does-denmark-have-in-2022
holidays <- function(date){
 fcase(
  date %within% interval(as.POSIXct("2022-01-01", tz = "Europe/Copenhagen"), as.POSIXct("2022-01-03 23:59", tz = "Europe/Copenhagen")) |
    date %within% interval(as.POSIXct("2022-12-22", tz = "Europe/Copenhagen"), as.POSIXct("2023-01-01 23:59", tz = "Europe/Copenhagen")), "Christmas holiday",
  date %within% interval(as.POSIXct("2022-02-19", tz = "Europe/Copenhagen"), as.POSIXct("2022-02-27 23:59", tz = "Europe/Copenhagen")), "Winter holiday",
  date %within% interval(as.POSIXct("2022-04-09", tz = "Europe/Copenhagen"), as.POSIXct("2022-04-18 23:59", tz = "Europe/Copenhagen")), "Easter holiday",
  date %within% interval(as.POSIXct("2022-06-25", tz = "Europe/Copenhagen"), as.POSIXct("2022-08-07 23:59", tz = "Europe/Copenhagen")), "Summer holiday",
  date %within% interval(as.POSIXct("2022-10-15", tz = "Europe/Copenhagen"), as.POSIXct("2022-10-23 23:59", tz = "Europe/Copenhagen")), "Autumn holiday",
  date %within% interval(as.POSIXct("2022-05-26", tz = "Europe/Copenhagen"), as.POSIXct("2022-05-29 23:59", tz = "Europe/Copenhagen")), "\nAscension holiday",
  date %within% interval(as.POSIXct("2022-05-13", tz = "Europe/Copenhagen"), as.POSIXct("2022-05-15 23:59", tz = "Europe/Copenhagen")), "Great prayer holiday",
  date %within% interval(as.POSIXct("2022-06-04", tz = "Europe/Copenhagen"), as.POSIXct("2022-06-06 23:59", tz = "Europe/Copenhagen")), "\n\nWhitsunday holiday",
  default = "No holiday"
)
}
