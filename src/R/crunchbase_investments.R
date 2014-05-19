#########
### In this analysis, I would like to take a closer look at geographic investment patterns
### across different industries
#########

rm(list=ls())
setwd("~/Github/Crunchbase_Network")

options(stringsAsFactors=F)

#### The following data was obtained from CrunchBase Data Exports
investments <- read.csv("~/Github/Crunchbase_Network/Data/investments.csv")

library(plyr)
library(dplyr)
colnames(investments)
filter(investments, company_name %in% c("Quid", "Palantir Technologies"))

### First, we need to account for the fact that each row reflects the total investment,
### not the (approximate) investment for a given investor
num_firms <- ddply(investments,
                   .(company_name, funded_at, raised_amount_usd),
                   nrow)
colnames(num_firms)[4] <- "nfirms"
num_firms <- transform(num_firms, raised_per_firm = raised_amount_usd/nfirms)
investments <- merge(investments, num_firms)

count(investments$raised_per_firm>0)
### looks like we have quite a few cases where the amount invested is unknown
investments <- filter(investments, raised_per_firm>0)

#### let's take a look at what industries we are dealing with
count(investments$company_category_code)

### remove nonsense/useless categories
investments <- filter(investments,
                      !(company_category_code %in% c("", "6/28/2005", "other")))

count(investments$company_category_code)
### let's fix that typo
investments[which(investments$company_category_code=="public_relation"),
            "company_category_code"] <- "public_relations"
count(investments$company_category_code)

### Now let's take a look at regions
count(investments$investor_region)
### Remove useless regions
investments <- filter(investments, !(investor_region %in% c("unknown", "TBD")))
count(investments$company_region)
investments <- filter(investments, !(company_region %in% c("unknown", "TBD")))
### There are still quite a few typos, but they shouldn't affect results too much
investments <- transform(investments,
                         investor_region = toupper(investor_region),
                         company_region = toupper(company_region))

library(reshape2)

### Determine the aggregate amount invested from one region to another for each industry
both_invest <- recast(investments,
                      company_category_code + investor_region + company_region ~ variable,
                      sum,
                      measure.var="raised_per_firm",
                      id.var=c("company_category_code", "investor_region", "company_region"))
colnames(both_invest)[4] <- "total_invested"

### Total invested by/in each region for each category
invest_region_categ <- recast(both_invest,
                              investor_region + company_category_code ~ variable,
                              sum,
                              measure.var="total_invested",
                              id.var=c("investor_region", "company_category_code"))
company_region_categ <- recast(both_invest,
                               company_region + company_category_code ~ variable,
                               sum,
                               measure.var="total_invested",
                               id.var=c("company_region", "company_category_code"))
colnames(company_region_categ)[3] <- "total_raised"

### Unfortunately, RedeR doesn't allow for duplicate edges, so we have to change this
### dataframe to reflect aggregate in-out investment by region
region_invest <- merge(both_invest, both_invest,
                       by.x=c("company_category_code", "investor_region", "company_region"),
                       by.y=c("company_category_code", "company_region", "investor_region"),
                       suffixes=c(".invest", ".comp"))
### Currently only looking at regions where there is bi-directional investment
head(region_invest)
region_invest[is.na(region_invest)] <- 0
region_invest <- transform(region_invest,
                           net_invested = total_invested.invest - total_invested.comp)
### Positive net_raised reflects more money invested than received for the investor region
### Zero net_raised should reflect investing within same region
region_invest <- filter(region_invest, net_invested>0)

arrange(count(region_invest$company_category_code), freq)
### We have 40 company categories, with Software as the most common

## Let's take a look at mobile investment patterns
region_mobile <- filter(region_invest, company_category_code=="mobile")[,-1]

tail(arrange(filter(both_invest, company_category_code=="mobile"), total_invested))
### amongst all investment patterns, the largest pairing within mobile is
### sf bay investing within itself
tail(arrange(region_mobile, net_invested))
### when excluding within region investments, the largest pairing is
### sf bay investing within Seattle

library(igraph)

mobile_graph <- graph.data.frame(region_mobile, directed=T)

E(mobile_graph)$weight <- region_mobile$net_invested

#### RedeR is a fantastic tool for visualizing networks
#### This package allows for a framework for visualizing the hierarchical/modular
#### nature of networks. Most importantly, it comes with built-in force-directed
#### algorithms for optimizing network graph layouts
library(RedeR)

### Add some additional attributes to the graph
mobile_graph <- att.mapv(g=mobile_graph,
                         dat=filter(invest_region_categ,
                                    company_category_code=="mobile"))
mobile_graph <- att.setv(g=mobile_graph,
                         from="name",
                         to="nodeAlias")
mobile_graph <- att.setv(g=mobile_graph,
                         from="total_invested",
                         to="nodeSize",
                         xlim=c(25,50,1))
mobile_graph <- att.mapv(g=mobile_graph,
                         dat=filter(company_region_categ,
                                    company_category_code=="mobile"))
mobile_graph <- att.setv(g=mobile_graph,
                         from="total_raised",
                         to="nodeFontSize",
                         xlim=c(5,25,1))
mobile_graph <- att.sete(g=mobile_graph,
                         from="net_invested",
                         to="edgeWeight")
mobile_graph <- att.sete(g=mobile_graph,
                         from="net_invested",
                         to="edgeWidth",
                         xlim=c(5,20,1))

### create environment for visualization
rdp <- RedPort()
calld(rdp)
### make sure to maximize the new window for best viewing

resetd(rdp)
addGraph(rdp, mobile_graph, layout.kamada.kawai(mobile_graph))

### let's nest a hierarchical clustering

## Adjacency matrix
mobile_adj <- get.adjacency(mobile_graph, attr="weight", sparse=T)
hc_mobile <- hclust(dist(mobile_adj), method="ward")

nesthc(rdp, hc_mobile, cutlevel=5, nmemb=4, labels=V(mobile_graph)$nodeAlias,
       grid=c(2,2), nfontsz=25, gridScale=50)

### now use the built-in functionality to make the graph more readable
updateContainerSize(rdp)
relax(rdp,
      p1=245, ### edge target length
      p2=415, ### edge stiffness
      p3=123, ### node repel factor
      p4=85, ### node perimeter effect
      p5=30,  ### node speed limit
      p6=140, ### nest-nest edge target length
      p7=20,   ### nest-node repel factor
      p8=9,  ### repulse radius
      ps=T)   ### settings 

### Unfortunately the RedeR package still has visualization clarity issues

### Let's try using d3 within R!
library(d3Network)

regions <- as.data.frame(unique(stack(region_mobile[,1:2])[,1]))

d3SimpleNetwork(Data=region_mobile[,c(1,2,5)],
                fontsize=15,
                file="mobie_region_graph_test.html")
### currently working on a way to incorporate the node size attribute in d3