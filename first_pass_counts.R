#Here we do the first pass of counts of adverse events.
#Are the internet comments true? Did the COVID-19 vaccine really cause more adverse events than all other vaccines?

#Others things that we have learnt in this analysis.
#VAERS was established in 1990. https://vaers.hhs.gov/about.html
#VAX_DATE has errors. So we use RECVDATE in this first past.

library(data.table)
library(ggplot2)
library(scales)

options( scipen = 999 )

#Load the data if you are starting fresh
load("raw_vaers.RData")

vax[,unique(VAX_TYPE)]
vax[,length(unique(VAERS_ID))]
#1179569
nrow(vax)
#1564690
##So it looks like some people get multiple vaccines in one day. Then they have an adverse event.


vax[,covid_vax := ifelse(VAX_TYPE == "COVID19", "COVID19", "ALL_OTHERS")]
vax[,list("num_adverse_events"=length(unique(VAERS_ID))), by="covid_vax"]

#    covid_vax num_adverse_events
#1: ALL_OTHERS             716145
#2:    COVID19             464789
464789/(716145+464789)
#0.3935775
#COVID19 vaccines comprise 39% of all reported vaccine adverse reactions in the US since 1990.

f.vax[,covid_vax := ifelse(VAX_TYPE == "COVID19", "COVID19", "ALL_OTHERS")]
f.vax[,list("num_adverse_events"=length(unique(VAERS_ID))), by="covid_vax"]
#1: ALL_OTHERS             100627
#2:    COVID19             136120
136120/(136120+100627)
#0.5749598
#COVID19 vaccines comprise 57% of all reported vaccine adverse reactions outside of the US since 1990.

#We know the COVID19 vaccines were recently released. 
#How spread out were the other adverse reactions?
#Perhaps we just didn't get many reports in earlier years?

data.vax <- merge(x=data, y=vax, by="VAERS_ID")
data.vax[,VAX_DATE := as.Date(VAX_DATE, format="%m/%d/%Y")]
data.vax[,ONSET_DATE := as.Date(ONSET_DATE, format="%m/%d/%Y")]
data.vax[,RECVDATE := as.Date(RECVDATE, format="%m/%d/%Y")]

data.vax[,vax_year := year(VAX_DATE)]
data.vax[,recv_year := year(RECVDATE)]
data.vax[,vax_month := month(VAX_DATE)]
data.vax[,recv_month := month(RECVDATE)]
data.vax[,a_year := fcoalesce(vax_year, recv_year)]
data.vax[,a_month := fcoalesce(vax_month, recv_month)]

data.vax[covid_vax == "ALL_OTHERS",list("num_adverse_events"=length(unique(VAERS_ID))), by="recv_year"][order(recv_year),]
#A ramp up in adverse events. But no spikes in the past 10 years.
#    recv_year num_adverse_events
# 1:      1990               2102
# 2:      1991               9933
# 3:      1992              10692
# 4:      1993              10147
# 5:      1994              10193
# 6:      1995              10001
# 7:      1996              10771
# 8:      1997              11006
# 9:      1998               9949
#10:      1999              12123
#11:      2000              14104
#12:      2001              13359
#13:      2002              14074
#14:      2003              16757
#15:      2004              15324
#16:      2005              15581
#17:      2006              17313
#18:      2007              28226
#19:      2008              29766
#20:      2009              32786
#21:      2010              31582
#22:      2011              25408
#23:      2012              26668
#24:      2013              29736
#25:      2014              34340
#26:      2015              44422
#27:      2016              45706
#28:      2017              38911
#29:      2018              49137
#30:      2019              48443
#31:      2020              39275
#32:      2021               8310
#    recv_year num_adverse_events



#Now let's do the non-domestic.
f.data.vax <- merge(x=f.data, y=f.vax, by="VAERS_ID")
f.data.vax[,VAX_DATE := as.Date(VAX_DATE, format="%m/%d/%Y")]
f.data.vax[,ONSET_DATE := as.Date(ONSET_DATE, format="%m/%d/%Y")]
f.data.vax[,RECVDATE := as.Date(RECVDATE, format="%m/%d/%Y")]

f.data.vax[,vax_year := year(VAX_DATE)]
f.data.vax[,recv_year := year(RECVDATE)]
f.data.vax[,vax_month := month(VAX_DATE)]
f.data.vax[,recv_month := month(RECVDATE)]
f.data.vax[,a_year := fcoalesce(vax_year, recv_year)]
f.data.vax[,a_month := fcoalesce(vax_month, recv_month)]

f.data.vax[covid_vax == "ALL_OTHERS",list("num_adverse_events"=length(unique(VAERS_ID))), by="recv_year"][order(recv_year),]
#    recv_year num_adverse_events
# 1:      1990                 49
# 2:      1991                 59
# 3:      1992                125
# 4:      1993                161
# 5:      1994                161
# 6:      1995                272
# 7:      1996                418
# 8:      1997                602
# 9:      1998                833
#10:      1999                755
#11:      2000               1007
#12:      2001               1274
#13:      2002               1257
#14:      2003               1323
#15:      2004               1185
#16:      2005               1866
#17:      2006               2017
#18:      2007               2587
#19:      2008               3677
#20:      2009               4256
#21:      2010               4994
#22:      2011               5718
#23:      2012               5439
#24:      2013               6509
#25:      2014               6868
#26:      2015               7511
#27:      2016               7991
#28:      2017               7443
#29:      2018               8673
#30:      2019               9569
#31:      2020               4933
#32:      2021               1095
#    recv_year num_adverse_events


#Now let's make some charts to really drive the point home
by.year <- data.vax[,list("num_adverse_events"=length(unique(VAERS_ID))), by=c("recv_year", "covid_vax")][order(recv_year),]
the.chart <- ggplot(by.year) + geom_line(aes(x=recv_year, y=num_adverse_events, color=covid_vax)) 
the.chart <- the.chart + ggtitle("VAERS US Adverse Events\nt.me/covfefenewsnetwork")
the.chart <- the.chart + ylab("Total Aderse Events Reported Per Year") + xlab("Years")
the.chart <- the.chart + scale_y_continuous(label=comma, limits=c(0,464789))
the.chart <- the.chart + labs(color="Vaccine Type")
the.chart <- the.chart + theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=16,face="bold"))
ggsave("us_chart.png", the.chart, dpi="retina")
#by.year.vax <- dcast(by.year, recv_year ~ covid_vax, value.var="num_adverse_events")
#the.chart <- ggplot(by.year.vax) + geom_line(aes(x=recv_year, y=ALL_OTHERS), color="blue") + geom_line(aes(x=recv_year, y=COVID19), color="red")

by.year <- f.data.vax[,list("num_adverse_events"=length(unique(VAERS_ID))), by=c("recv_year", "covid_vax")][order(recv_year),]
the.chart <- ggplot(by.year) + geom_line(aes(x=recv_year, y=num_adverse_events, color=covid_vax)) 
the.chart <- the.chart + ggtitle("VAERS Non-US Adverse Events\nt.me/covfefenewsnetwork")
the.chart <- the.chart + ylab("Total Aderse Events Reported Per Year") + xlab("Years")
the.chart <- the.chart + scale_y_continuous(label=comma, limits=c(0,136120))
the.chart <- the.chart + labs(color="Vaccine Type")
the.chart <- the.chart + theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=16,face="bold"))
ggsave("non-us_chart.png", the.chart, dpi="retina")


