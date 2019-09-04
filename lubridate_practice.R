library(lubridate)

ymd(19931123)
mdy(11231993)
dmy(23111993)

mytimepoint <- ymd_hm("1993-11-23 11:23", tz = "Europe/Prague")
mytimepoint
class(mytimepoint)
year(mytimepoint)
month(mytimepoint)
day(mytimepoint)
hour(mytimepoint)
minute(mytimepoint)

wday(mytimepoint)
wday(mytimepoint, label = T, abbr = F)

with_tz(mytimepoint, tz = "Europe/London")
mytimepoint

time1 = ymd_hm("1993-09-23 11:23", tz = "Europe/Prague")
time2 = ymd_hm("1995-11-02 15:23", tz = "Europe/Prague")

myinterval <- interval(time1, time2)
myinterval
class(myinterval)

# Lubridate Exercise ####
df <- tibble::tribble(
  ~date, ~time, ~measurement,
  "1998-11-11", "22:04:05", 11.04,
  "1983-01-23", "04:09:45", 11.37,
  "1982-09-04", "11:09:56", 12.73,
  "1945_05-09", "23:15:12", 8.63,
  "1982-12-24", "14:16:34", 10.09,
  "1974-12-03", "08:08:23", 9.69,
  "1987-12-10", "21:16:14", 9.22
)
# Or make 3 vectors and combine
a <- c("1983-01-01","1999-03-05","1987/04/09",19990225)
a <- ymd(a, tz = "CET")
a
b <- c("22 4 5", "04:09;45","11:9:56","23,15,16")
b <- hms(b)
b
f <- rnorm(4,10)
f <- round(f, 2)
f

df$date <- ymd(df$date, tz = "CET")
df$time <- hms(df$time)
str(df)

date_time_measurement <- cbind.data.frame(
  date = a
  , time = b
  , measurement = f
)
date_time_measurement
df
str(date_time_measurement)
str(df)

minutes(7)
minutes(2.5)
dminutes(3)
dminutes(3.5)

minutes(2) + seconds(5)
minutes(2) + seconds(75)
as.duration(minutes(2) + seconds(75))

leap_year(2009:2014)
ymd(20140101) + years(1)
ymd(20140101) + dyears(1)
leap_year(2016)
ymd(20160101) + years(1)
ymd(20160101) + dyears(1)

x <- ymd_hm("2014-04-12 23:12", tz = "CET")
x
minute(x) <- 7
x
with_tz(x, tz = "Europe/London")

y <- ymd_hm("2015-04-12 23:07", tz = "CET")
interval(x, y)
y-x
