#Read the file
crime1 <- read.csv(file.choose(), header=T) 

#Joining the county file
county<-read.csv("C:\\Users\\priya\\Desktop\\Fall 2017\\STATS\\Final Project\\data\\NC_Counties.csv",1 )
View(county)
county<-na.omit(county)
View(county)
crimenc<- merge(crime1,county,by.x="county", by.y="County.Code")
crimenc$X <- NULL
View(crimenc)
colnames(crimenc)[25] <- "CountyName"


out <- c(440,175,174,586,353,435,436,437,438,439,441)
crimeout <- crimenc[-out,]

# Plotting dependent variable crmrte with other independent variables as per hypothesis:
qplot(y=crmrte, x=prbarr, data=crimeout, main="CrimeRate and Probability of Arrest",geom=c("point", "smooth"))
cor(crimeout$prbarr,crimeout$crmrte)
qplot(y=crmrte, x=prbpris, data=crimeout, main="CrimeRate and Probability of Prison",geom=c("point", "smooth"))
cor(crimeout$prbpris,crimeout$crmrte)
qplot(y=crmrte, x=pctymle, data=crimeout, main="CrimeRate and Percentage of male",geom=c("point", "smooth"))
cor(crimeout$pctymle,crimeout$crmrte)
qplot(y=crmrte, x=taxpc, data=crimeout, main="CrimeRate and Tax per Capita",geom=c("point", "smooth"))
cor(crimeout$taxpc,crimeout$crmrte)

#CrimeRate and PolicePerCapita

plot(density(crimenc$crmrte))
plot(density(crimeout$crmrte)) # Plot after removing outliers is better

qplot(y=crmrte, x=polpc, data=crimeout, main="PolicePerCapita and CrimeRate",xlab="Police Per Capita",ylab="Crime Rate (Crimes per Capita)",geom=c("point", "smooth"))
cor(crimeout$crmrte,crimeout$polpc)

crime_polpc<- t.test(crimeout$crmrte,crimeout$polpc, conf.level = 0.95)
crime_polpc

dev.off()
plot(aggregate(crmrte ~ year, data=crimeout, FUN="mean"), main="CrimeRate over the years",xlab="Years",ylab="Crime Rate (Crimes per Capita)")
plot(aggregate(polpc ~ year, data=crimeout, FUN="mean"), main="Police Per Capita over the years",xlab="Years",ylab="Police Per Capita")

crime_byyear<- aggregate(crmrte ~ year, data=crimeout, FUN="mean")

polpc_byyear<- aggregate(polpc ~ year, data=crimeout, FUN="mean")
plot(crime_byyear$crmrte~polpc_byyear$polpc, main="CrimeRate and PolicePerCapita")
abline(lm(crime_byyear$crmrte~polpc_byyear$polpc))
cor(crime_byyear$crmrte,polpc_byyear$polpc)

west <- crimeout[crimeout$region=="west",]
central <- crimeout[crimeout$region=="central",]
other <- crimeout[crimeout$region=="other",]
crmrteagwest <- aggregate(crmrte ~ year, data= west, FUN = "mean")
crmrteagcentral <- aggregate(crmrte ~ year, data= central, FUN = "mean")
crmrteagother <- aggregate(crmrte ~ year, data= other, FUN = "mean")

polpcagwest <- aggregate(polpc ~ year, data= west, FUN = "mean")
polpcagcentral <- aggregate(polpc ~ year, data= central, FUN = "mean")
polpcagother <- aggregate(polpc ~ year, data= other, FUN = "mean")

par(mfrow=c(2,2))
plot(crmrteagwest$crmrte~ polpcagwest$polpc,main="Crime Rate vs Police per Capita in the West",xlab="Police Per Capita",ylab="Crime Rate (per Capita)")
abline(lm(crmrteagwest$crmrte~ polpcagwest$polpc),col="red",lwd=2)
cor(crmrteagwest$crmrte, polpcagwest$polpc)

plot(crmrteagcentral$crmrte~ polpcagcentral$polpc,main="Crime Rate vs Police per Capita in Central",xlab="Police Per Capita",ylab="Crime Rate (per Capita)")
abline(lm(crmrteagcentral$crmrte~ polpcagcentral$polpc),col="red",lwd=2)
cor(crmrteagcentral$crmrte, polpcagcentral$polpc)


plot(crmrteagother$crmrte~ polpcagother$polpc,main="Crime Rate vs Police per Capita  in other regions",xlab="Police Per Capita",ylab="Crime Rate (per Capita)")
abline(lm(crmrteagother$crmrte~ polpcagother$polpc),col="red",lwd=2)
cor(crmrteagother$crmrte, polpcagother$polpc)

#Crime rate by region (Not holding constant for years, may not be useful)
par(mfrow=c(2,2))
qplot(y=crmrte, x=polpc, data=west, main="Crime Vs. Police - West", geom=c("point", "smooth"))
qplot(y=crmrte, x=polpc, data=central, main="Crime Vs. Police - Central", geom=c("point", "smooth"))
qplot(y=crmrte, x=polpc, data=other, main="Crime Vs. Police - Other", geom=c("point", "smooth"))

cor(crime_byyear$crmrte,polpc_byyear$polpc)

crime_polpc1<- t.test(crime_byyear$crmrte,polpc_byyear$polpc, conf.level = 0.95)
crime_polpc1



#CrimeRate and Density

qplot(y=crmrte, x=density, data=crimeout,xlab="Density (Hundreds of people per km)",ylab="Crime Rate (Crimes per Capita)", main="Crime Rate and Density" , geom=c("point", "smooth"))
qplot(y=crmrte, x=density, data=crimeout,xlab="Density",ylab="Crime Rate", main="Crime Rate and Density" , facets=~year, geom=c("point", "smooth"))
qplot(y=crmrte, x=density, data=crimeout,xlab="Density",ylab="Crime Rate", main="Crime Rate and Density" , facets=~smsa, geom=c("point", "smooth"))
qplot(y=crmrte, x=density, data=crimeout,xlab="Density",ylab="Crime Rate", main="Crime Rate and Density" , facets=~region, geom=c("point", "smooth"))


#NUll Hypothesis -> There is no relation between Crime Rate and Density
#Alternate Hypothesis -> There is a relation between Crime Rate and Density
mod_density<- t.test(crimeout$crmrte, crimeout$density, conf.level = 0.95)
mod_density

#Since the P-value is less than 0.05 we reject the null and accept the alternate hypothesis.

#Co-relation between Crime rate and density:
cor(crimeout$crmrte,crimeout$density)

#CrimeRate and Wages
privwageavg<- (crimeout$wcon 
               + crimeout$wfir
               + crimeout$wtuc
               + crimeout$wtrd
               + crimeout$wser)/5

govwageavg<- (crimeout$wloc
              + crimeout$wsta
              + crimeout$wfed)/3

crimeout["PrivAvgWage"] <- privwageavg
crimeout["GovAvgWage"] <- govwageavg

# Standardizing wages:
ln <- length(crimeout$county)
rw <- c(90.9/90.9, 90.9/96.5, 90.9/99.6, 90.9/103.9, 90.9/107.6, 90.9/109.6, 90.9/113.6)
real<- rep(rw, length.out = ln)


crimeout$rprivwageavg <- crimeout$PrivAvgWage*real
crimeout["RealPrivAvgWage"] <- crimeout$rprivwageavg

crimeout$rgovwageavg <- crimeout$GovAvgWage*real
crimeout["RealGovAvgWage"] <- crimeout$rgovwageavg

length(crimeout$rprivwageavg)
View(crimeout)

length(crimeout$PrivAvgWage)
length(crimeout$year)
wages_by_year<-aggregate(PrivAvgWage+GovAvgWage ~ year, data=crimeout,FUN = "mean")
plot(crime_byyear$crmrte~wages_by_year$`PrivAvgWage + GovAvgWage`)
abline(lm(crime_byyear$crmrte~wages_by_year$`PrivAvgWage + GovAvgWage`),col="red",lw=2)
cor(crime_byyear$crmrte,wages_by_year$`PrivAvgWage + GovAvgWage`)

wages_by_year<-aggregate(PrivAvgWage ~ year, data=crimeout,FUN = "mean")
plot(crime_byyear$crmrte~wages_by_year$PrivAvgWage)
abline(lm(crime_byyear$crmrte~wages_by_year$PrivAvgWage))
cor(crime_byyear$crmrte,wages_by_year$PrivAvgWage)


wages_by_year<-aggregate(GovAvgWage ~ year, data=crimeout,FUN = "mean")
plot(crime_byyear$crmrte~wages_by_year$GovAvgWage)
abline(lm(crime_byyear$crmrte~wages_by_year$GovAvgWage))
cor(crime_byyear$crmrte,wages_by_year$GovAvgWage)

crime_wages<- t.test(crime_byyear$crmrte,wages_by_year$`PrivAvgWage + GovAvgWage`, conf.level = 0.95)
crime_wages


View(crimeout)
modsmden<- lm(crmrte ~ density, data=crimeout)
summary(modsmden)

ggplot(data=crimeout, # data 
       aes(x=crmrte, y=polpc, color=region)) +  
  geom_point(pch=20, size=3) + 
  stat_smooth(method="lm", se=F, lwd=1.5) + 
  labs(title="Crime Rate vs. Police per Capita", x="Police Per Capita", y="Crime Per Capita") # annotation 

View(describe(crimeout$crmrte))

# Building final model from the statistically significant variables: 
model_final <- lm(crmrte ~ polpc+prbarr+density+prbconv, data = crimeout)
summary(model_final)
# Since the adjusted R-squared value is 65.8%, this model can predict 65.8% of a county's crime rate
# given the variables chosen.