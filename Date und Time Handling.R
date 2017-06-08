########## DATE & DATETIME HANDLING
# Date = Kalendartag
# POSIXct = datetime
library(ggplot2)
library(lubridate)

datetime1 <- dmy_hm("27 Aug 2017 14:00")
datetime2 <- dmy_hm("27 Aug 2018 14:00")

#### read_csv 
# versucht automatisch zu erkennen
library(readr)
releases <- read_csv("~/Documents/Data/rversions.csv")
str(releases)


#### Operatoren
today()                  # Heute: Datum
now()                    # Jetzt: DateTime
range(releases$date)
max(releases$date)

#### Extrahieren
## Date
day(datetime1)          
month(datetime1)        
year(datetime1)         
##Time 
hour(datetime1)
minute(datetime1)
second(datetime1)
# Weiteres
wday(datetime1, week_start=1, label=T)         # Tag in der Woche 
quarter(datetime1)


### Tabeln
# => häufigkeiten zu bestimmten zeitpunkten
wday(releases$datetime, label = TRUE) %>% table()
year(releases$date) %>% table()

### Filtern
filter(releases, datetime1 > ymd("2000-1-1"))
wday(releases$date) %in% 2:6   

# difftime: feinere einstellungsmöglichkeiten: "secs", "mins", "hours", "days"
diff <- difftime(now(), mdy("July 20, 1969"), units = "days")


#### Period & Durations
# Period   = menschliche Interpretation - z.B. zwei Wochen Frist
# Duration = rechnet in sekunden => ddays(1) statt days(1)
# seconds(), minutes(), hours(), days(), months(), years()
datetime1 <- dmy_hm("27 Aug 2018 14:00")
datetime1 + days(1)
class(now() - datetime1)   # ergibt difftime => today() - date geht nicht - inkompatibel
days(2)                    # "2d 0H 0M 0S"
ddays(2)                   # "172800s (~2 days)"

#### Intervals
# %--%
# int_start(), int_end() and int_length()
# %within%
interval <- datetime1 %--% datetime2  # ...oder
interval <- interval(datetime1, datetime2)
int_start(interval); int_end(interval)  
int_length(interval)
(datetime1 + days(1)) %within% interval
# Konvertieren zu 
as.period(interval)
(d <- as.duration(interval))
seconds_to_period(86400)

#### Plotting
ggplot(releases, aes(x = datetime1, y = type)) +
  geom_line(aes(group = 1, color = as.factor(major)))  +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  xlim(as.Date("2000-01-01"), as.Date("2016-01-01"))

#### Anytime
# Versucht aus jedem String ein Datum zu parsen
library(anytime) 
sep_10_2009 <- c("September 10 2009", "2009-09-10", "10 Sep 2009", "09-10-2009")
anytime("sep_10_2009 10:00")

#### Einfaches Dates & Datetimes parsen
# Setzt sich zusammmen aus
# ymd, dmy, mdy, ydm 
# _h, _hm, _hms
ymd("2010 September 20th")
dmy("02.01.2010")
mdy_hm("Sep, 12th 2010 14:00")
# Normal
as.POSIXct("2010-10-01 12:12:00")
as.POSIXct("2010-10-01 12:12:00", tz = "UTC") # oder z.B. America/Los_Angeles


#### Parsen: Reihenfolge spezifzieren
# ?parse_date_time gibt referenz über buchstaben
# z.B. d = weekday, I = 12 stundenzeit ; p = pm/am
x <- "Monday June 1st 2010 at 4pm"
parse_date_time(x, orders = "amdyIp")

# Verschiedene Formate im selben Datensatz
# => können mehrere Formate angegeben werden
two_orders <- c("October 7, 2001", "October 13, 2002", "April 13, 2003", 
                "17 April 2005", "23 April 2017")
parse_date_time(two_orders, orders = c("mdy", "dmy"))
# ...oder
short_dates <- c("11 December 1282", "May 1372", "1253")
dates <- parse_date_time(short_dates, orders = c("dOmY", "OmY", "Y"))



#### Gut für plotten
releases$wday <- wday(releases$datetime, label = T)
ggplot(releases, aes(x=wday)) + geom_bar() + facet_wrap(~ type, ncol = 1, scale = "free_y")

