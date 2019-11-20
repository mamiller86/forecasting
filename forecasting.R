#### Forecasting Sponsorship
#### Mikaela Miller
#### November 2019

library(DBI)
library(dplyr)

con <- dbConnect(odbc::odbc(), dsn="R_ODBC")

spon_dat <- dbGetQuery(con, "select distinct
      s.ShipTopersonid as personid,
      s.sponsorshipid,
      s.allocationstartdate,
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
from edw_prod.dbo.SponsorshipFact s
LEFT JOIN edw_prod..CampaignDim cd ON s.campaignsk=cd.campaignsk
LEFT JOIN edw_prod..BudgetDim bd ON cd.BudgetGroupId = bd.BudgetGroupId
left join edw_prod.dbo.vw_SponsorshipPaidSponsors p on p.sponsorshipfactid=s.sponsorshipfactid
where 
s.allocationstartdate >= '2016-10-01'
and s.PreviousSponsorshipId=1
and s.sponsorshipallocationsk in (2,5)
and s.reallocated='No'
and p.paidsponsor='Yes'
order by s.allocationstartdate")
