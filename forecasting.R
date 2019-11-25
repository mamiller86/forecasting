#### Forecasting Sponsorship
#### Mikaela Miller
#### November 2019

library(DBI)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(fpp2)
library(zoo)
library(pracma)
library(corrplot)
library(rio)
library(tidyr)
library(Hmisc)


# Endless campaigning for general needs usually doesn’t serve as a catalyst for spiking generosity. 
# You’ve got to create urgency with a deadline, 
# and craft a tie-in between your recipients’ needs (in the form of a story) and your new fundraising “season.”  

wc_dates = data.frame(date=as.Date(c('2017-12-27', '2018-03-02', '2018-05-31', '2018-08-28', '2018-12-28',
                              '2019-04-01', '2019-04-29', '2019-06-26')),
                      quantity=c(30000,30000,300000,39946,48002,47512,3190,79580))
wc_dates$month = as.Date(paste0(substring(wc_dates$date, 1,8), "01"))

### concert dates
tca_dates = import("Tasks_Events_191125.xlsx")
tca_artists = import("tca_artist_influence.csv")
tca_dates$month = as.Date(paste0(substring(tca_dates$`Planned Completion Date`, 1, 8), "01"))
tca_dates = merge(tca_dates, tca_artists, by.x="Task Name", by.y="Task.Name", all.x=T)
tca_dates = tca_dates %>% group_by(month, `Task Name`) %>% summarise(influence = sum(tca_spotifyListeners, na.rm=T))
tca_counts = spread(tca_dates, key="Task Name", value="influence")
tca_counts[is.na(tca_counts)==T] <- 0
tca_counts$concertReach = log(apply(tca_counts[,c(2:13)], 1, function(x) sum(x, na.rm=TRUE)))
tca_counts$lag1mo_concerts = lag(tca_counts$concertReach, +1)
####
# tca_artists = names(tca_counts)[-c(1,14:15)]
# tca_spotifyListeners = c(298958,671369,0,7,0,233424,1527870,676121,557172,2765,26,642386)
# tca_influence = data.frame(`Task Name` =tca_artists, tca_spotifyListeners)
# write.csv(tca_influence, "tca_artist_influence.csv", row.names=FALSE)

# tca_dates = merge(tca_dates, tca_influence, by.x="Task Name", by.y="Task.Name", all.x=T)
#econ variables
econ <- import("econ_indicators_BLS.xlsx") 
econ= econ%>% filter(Month>='2014-12-31') %>% mutate(month=as.Date(Month))


## open database connection

con <- dbConnect(odbc::odbc(), dsn="R_ODBC")

spon_dat <- dbGetQuery(con, "select distinct
      s.ShipTopersonid as personid,
      s.sponsorshipid,
      s.standingorderid,
      s.allocationstartdate,
      s.sponsorshipfactid,
      s.startdate,
      s.initialpaymentdate,
      s.childid,
      cd.marketingsourcegroup,
      cd.campaignid,
      cd.marketingsource,
      bd.BudgetMarketingSourceGroup,
      p.paidsponsor,
      case when cd.marketingsourcegroup in ('Affiliate','Search Engine','Web-Other','Banner Ads','Banner Campaigns','Gaming','Health','ISP/Portal','Network','Shopping' ) 
              then 'Internet - Advertising'
           when cd.marketingsourcegroup in ('Advocate Center','Base','Internet-Base','Internet','Internet FB','SponsorThon') 
              then 'Internet - Base'
           when cd.marketingsourcegroup in ('Magazine','Newspaper') 
                    then 'Magazine/Newspaper'
           when cd.marketingsourcegroup in ('Rented Email List','Rented List') 
                    then 'Rented List'
           when cd.marketingsourcegroup in ('Broadcast','Broadcast','Cable','Radio','Radio FB','Satellite','Spot','Syndicated','TV-Other') 
                    then 'TV/Radio'
           when cd.marketingsourcegroup in ('Social Media','Social Network') 
                then 'Social Media'
           when cd.marketingsourcegroup ='Face to Face' and bd.budgetmarketingsourcegroup='Door to Door' 
                then 'Door to Door'
           when cd.marketingsourcegroup ='Face to Face' 
                then 'Face to Face'
           when cd.marketingsourcegroup ='House List' and bd.budgetmarketingsourcegroup='Child Waiting' 
                then 'Waiting Child'
           when cd.marketingsourcegroup not in ('None','House List','Inbound Call','Event Marketing') 
                then 'Other Misc'
           else cd.marketingsourcegroup 
      end as sponsorship_source
     ,s.pricepoint
     ,row_number() over (partition by s.standingorderid order by s.sponsorshipfactid desc) as ranking
--into test.newspn
from edw_prod.dbo.SponsorshipFact s
LEFT JOIN edw_prod..CampaignDim cd ON s.campaignsk=cd.campaignsk
LEFT JOIN edw_prod..BudgetDim bd ON cd.BudgetGroupId = bd.BudgetGroupId
left join edw_prod.dbo.vw_SponsorshipPaidSponsors p on p.sponsorshipfactid=s.sponsorshipfactid
where 
  s.allocationstartdate >= '2012-10-01' 
--and s.allocationstartdate < '2019-11-01'
and s.PreviousSponsorshipId=1
and s.sponsorshipallocationsk in (2,5)
and s.reallocated='No'
and p.paidsponsor='Yes'")

spon_dat = spon_dat %>% filter(ranking==1)

### Visualize new sponsorship data
spon_dat$month = as.Date(cut(spon_dat$allocationstartdate, breaks="month"))
spon_dat$source = ifelse(spon_dat$sponsorship_source %in% c("Door to Door", "Inbound Call", "Other Misc", 
                                                            "TV/Radio", "Magazine/Newspaper", 
                                                            "Social Media", "Rented List"), "Other", 
                         spon_dat$sponsorship_source)
spon_dat_sum = spon_dat %>% group_by(month,source) %>% summarise(new_spon = n())
monthly_totals_check =  spon_dat %>% group_by(month) %>% summarise(new_spon = n())

library(scales)
library(zoo)


format_quarters <- function(x) {
  x <- as.yearqtr(x)
  year <- as.integer(x)
  quart <- as.integer(format(x, "%q"))
  
  paste(c("Jan","Apr","Jul","Oct")[quart], 
        year)
}

ggplot(spon_dat_sum, aes(x=(month), y=new_spon, color=source))+
  geom_point()+geom_line()+
  scale_x_date("Year and Month",
               breaks = date_breaks("3 months"),
               labels = format_quarters) +
  ylab("New Sponsorships")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, vjust=0.5))+
  facet_grid(rows=vars(source))
 
################ Discontinues

all_disc <- dbGetQuery(con, "select sf.ShipToPersonId
             , vdso.DiscontinueDate as AllocationDiscontinueDate
                , sf.modifieddate
                , sf.standingorderid
                , sf.sponsorshipid
                , sf.PreviousSponsorshipId
                , vdso.allocationstartdate
                , sf.pricepoint
              --into test.discont1
              from (select StandingOrderId
                          ,SponsorshipFactId
                                  ,DiscontinueDate  
                                  ,startAllocationdate as AllocationStartDate
--                    from temp.vwDiscontinuedSponsorshipStandingOrders
                      from edw_prod.dbo.vwDiscontinuedSponsorshipStandingOrders
                       where DiscontinueDate >= '2012-10-01'
                         and DiscontinueDate < '2019-11-01') vdso
              join edw_prod.dbo.SponsorshipFact sf
                 on vdso.StandingOrderId = sf.StandingOrderId
                and vdso.SponsorshipFactId = sf.SponsorshipFactId
        JOIN (select standingorderid
                          from edw_prod.dbo.StandingOrderSponsorshipRevenue
                              where StandingOrderGiving > 0) so 
                   ON sf.StandingOrderId=so.StandingOrderId
         where sf.SponsorshipAllocationSk in (2,5)")

all_disc$diff = difftime(all_disc$AllocationDiscontinueDate, all_disc$allocationstartdate, units="days")
disc = all_disc %>%  filter(!(PreviousSponsorshipId==1 & diff <=14))

disc$month = as.Date(cut(disc$AllocationDiscontinueDate, breaks="month"))

disc_sum = disc %>% group_by(month) %>% summarise(discontinues = n())



ggplot(disc_sum, aes(x=(month), y=discontinues))+
  geom_point()+geom_line()+
  scale_x_date("Year and Month",
               breaks = date_breaks("3 months"),
               labels = format_quarters) +
  ylab("Discontinued Sponsorships")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, vjust=0.5))
  #facet_grid(rows=vars(source))

net = merge(monthly_totals_check, disc_sum, by="month")
net$net = with(net, new_spon-discontinues)
ggplot(net, aes(x=(month), y=net))+
  geom_point()+geom_line()+
  scale_x_date("Year and Month",
               breaks = date_breaks("3 months"),
               labels = format_quarters) +
  ylab("Net New Sponsorships")+
  geom_vline(data=wc_dates, aes(xintercept=month), color="violet")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, vjust=0.5))+
  ggtitle("Net New Sponsorships")


monthly_totals_check$var = "New Sponsorships"
names(monthly_totals_check)[2] = "value"
names(disc_sum)[2] = "value"
disc_sum$var = "Discontinued Sponsorships"
both = rbind(monthly_totals_check, disc_sum)

ggplot(both, aes(x=(month), y=value, color=var))+
  geom_point()+geom_line()+
  scale_x_date("Year and Month",
               breaks = date_breaks("3 months"),
               labels = format_quarters) +
  scale_color_discrete(name = "Key")+
  ylab("# of Sponsorships")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, vjust=0.5))+
  ggtitle("New and Discontinued Sponsorships")
#facet_grid(rows=vars(source))


##################################
### Forecasting with Holt Winters

### 1) Forecasting new sponsorships
### 2) Forecasting discontinue### 2) Forecasting discontinue
### 3) Forecasting net new sponsorships
net$wc_appeal = ifelse(net$month %in% wc_dates$month, 1, 0)
net$month = as.Date(net$month, format="%Y-%m-%d")
data = merge(net, econ, by="month", all.x=T)
data = merge(data, tca_counts, by="month", all.x=T)
data = data %>% filter(year(month)<=2019 & year(month)>=2015)
data[is.na(data)]<-0
fc_ts =  ts(data[,-1], start = c(2015, 1), frequency = 12)
tail(fc_ts)


# Create an ACF plot of the volume data
# new spon
plot(ggAcf(fc_ts[,1])) # no apparent seasonality
# disc spon
plot(ggAcf(fc_ts[,2])) # no apparent seasonality in discontinues
# net new spon
plot(ggAcf(fc_ts[,3])) # no apparent seasonality in discontinues
###


# ### Split into test and train
train_new = subset(fc_ts[,1], end = length(fc_ts[,1]) - 3)
train_net = subset(fc_ts[,3], end = length(fc_ts[,3]) - 3)


fc_hw <- hw(train_new, seasonal = "additive", h = 12)
summary(fc_hw)

# Check if residuals look like white noise
checkresiduals(fc_hw)

# Plot forecasts - Holt Winters is decent
autoplot(fc_hw)+ylab("# Sponsorships")+theme_bw()+ggtitle("Forecasts of New Sponsorships: \nHolt-Winters' additive method")
accuracy(fc_hw, fc_ts[,1])


##############################
### ARIMA model
table(net$wc_appeal)

arima_train = window(fc_ts, start=c(2015,10), end=c(2019, 05))

xreg <- cbind(wc_appeal = arima_train[, "wc_appeal"],
                    S.P.500.Avg.Close =arima_train[,"S.P.500.Avg.Close"],
                    TotalEmployees = arima_train[,"Total...of.Employees..1000s."],
                    Concerts = arima_train[,"concertReach"])
# Rec_prob = arima_train[,"Rec_prob"])
arima_test = window(fc_ts, start=c(2019,06), end=c(2019, 10))

xreg_test <- cbind(wc_appeal = arima_test[, "wc_appeal"],
                   S.P.500.Avg.Close =arima_test[,"S.P.500.Avg.Close"],
                   TotalEmployees = arima_test[,"Total...of.Employees..1000s."],
                   Concerts = arima_test[,"concertReach"])
# Rec_prob = arima_train[,"Rec_prob"])


############### Total Volume ARIMA
fit.arima <- auto.arima(arima_train[,"new_spon"], xreg = xreg) 

# 
# xpred = (model_data %>% filter(yr %in% c(2016, 2017,2018)) %>% group_by(mo) %>% 
#            summarize(S.P.500.Avg.Close = mean(S.P.500.Avg.Close),
#                      Dow.Jones.Avg.Close = mean(Dow.Jones.Avg.Close), 
#                      U3.Unemp.Rate = mean(U3.Unemp.Rate),
#                      TotalEmployees = mean(Total...of.Employees..1000s.),
#                      Hires = mean(Hires..1000s.),
#                      YoYNetChange = mean(YoYNetDeltaJobs),
#                      YC = mean(Yield.Curve),
#                      HPI = mean(HPI.Seasonal),
#                      CCI = mean(ConsumerConfidIndex),
#                      ATN.Merger = 1, 
#                      NovDecJan.Adjustment = mean(NovDecJan.Adjustment, na.rm=T)))
# xpred_full = rbind(as.matrix(dplyr::select(xpred, -mo)), as.matrix(dplyr::select(xpred, -mo)))
# xpred_test = xpred %>% filter(mo<=6) %>% select(-mo)
# # xpred_test$Rec_prob = rprob$Rec_prob[157:162]
# xpred_test = as.matrix(xpred_test)


# Forecast fit year ahead
fc_2019_tot = forecast(fit.arima, xreg=xreg_test)
fc_2019_tot %>% autoplot()
fc_2019_tot %>% checkresiduals()

test = window(fc_ts, start=c(2019,06), end=c(2019, 10))
accuracy(fc_2019_tot, data$new_spon[54:58])

fc_2019_tot_full = forecast(fit.arima, xreg = xpred_full)
