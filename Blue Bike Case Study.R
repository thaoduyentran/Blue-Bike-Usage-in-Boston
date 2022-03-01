
#####                                           #####
##                                                 ##
##                  DATA WRANGLING                 ##
##                                                 ##
#####                                           #####

#data loading
read
start_data <- read.csv("start-data.csv")
summary(start_data)

#understanding the data
summary(start_data)
dim(start_data)
str(start_data)


#checking NA/incorrect values
colSums(is.na(start_data))
colSums(start_data=="NULL")
colSums(start_data=="00000")


#trip duration histogram
plot.new()
trip.hist <- ggplot(start_data,aes(tripduration))+
  geom_histogram(color="#08519c" ,fill="#bdd7e7")+
  ggtitle("Frequency Distribution of Trip Duration")+
  xlab("Trip Duration (in minutes)")+
  ylab("Frequency")
trip.hist



#z score for tripduration
data <- start_data
avg.trip <- mean(data$tripduration)
sd.trip <- sd(data$tripduration)

data$z_tripduration <- (data$tripduration - avg.trip) / sd.trip

# z = (x-mu)/sigma
# 3 = (x-17.12)/24.07
# 3*24.07+17.12 = x

z.cutoff <- 3
cutoff.lower <- avg.trip - z.cutoff*sd.trip
cutoff.upper <- avg.trip + z.cutoff*sd.trip

summary(data)


#number of observations with outlier trip minutes
sum(data$tripduration>89.330) #87 outliers
(sum(data$tripduration>89.330)/nrow(data))*100

summary(data$tripduration)

trip.nooutliers <- data[data$tripduration<=89.330,]


#histogram for zscore outlier removal
z.hist <- ggplot(trip.nooutliers,aes(tripduration))+
      geom_histogram(color="#08519c" ,fill="#bdd7e7" )+
      ggtitle("Frequency Distribution of Trip Duration (Z-Score Method)")+
      xlab("Trip Duration (in minutes)")+
      ylab("Frequency")
z.hist


#iqr method
summary(data$tripduration)
q1 <- 7.467
iqr <- 12.017
q3 <- 20.10
out.q1 <- q1 - 1.5*iqr # -10.5585
out.q3 <- q3 + 1.5*iqr # 38.1255


#boxplot
boxplot(data$tripduration, xlab="$tripduration", outline=TRUE)
abline(h=out.q3,col="#08519c" )
abline(h=min(data$tripduration),col="#08519c" )


#zoomed in boxplot
plot.new()

boxplot <- ggplot(data, aes(y=tripduration,x=)) + 
      stat_boxplot(geom="errorbar") +
      geom_boxplot(color="#08519c", fill="#bdd7e7",outlier.colour = "#08519c")+
  ggtitle("Boxplot of Trip Duration")+
  ylab("Trip duration (in minutes)")+
  theme(text= element_text(size = 15))
#to zoom in on boxplot, add this line
#    coord_cartesian(ylim=c(0,50))
boxplot



#how many observations are >q3+1.5(IQR)?
sum(data$tripduration>out.q3) #raw
(sum(data$tripduration>out.q3)/nrow(data))*100 #proportion


#histogram of IQR method
trip.nooutliers <- data[data$tripduration<=out.q3,]

iqr.hist <- ggplot(trip.nooutliers,aes(tripduration))+
  geom_histogram(color="#08519c" ,fill="#bdd7e7" )+
  ggtitle("Frequency Distribution of Trip Duration (IQR Method)")+
  xlab("Trip Duration (in minutes)")+
  ylab("Frequency")
iqr.hist


#df with rides <= 1 hour
df <- data[data$tripduration <= 60,]
summary(df)
dim(df)



#####                                           #####
##                                                 ##
##         VISUALIZATION & DESCRIPTIVE STATS       ##
##                                                 ##
#####                                           #####

#histogram for rides <60mins
hist <- ggplot(df,aes(tripduration))+
  geom_histogram(color="#08519c" ,fill="#bdd7e7" )+
  ggtitle("Frequency Distribution of Trips <60 mins")+
  xlab("Trip Duration (in minutes)")+
  ylab("Frequency")

hist


#looking at rides < 10 minutes (extra)
rides.10 <- df[df$tripduration<=11&df$tripduration>4,]
summary(rides.10$end.station.name)
dim(rides.10)

nrow(df)-nrow(rides.10)
                      

#contingency table for start.station.name
install.packages("gridExtra")
library(gridExtra) #using gridextra to make table look presentable

tab <- table(df$usertype,df$start.station.name)

colnames(tab) <- c("Mass Ave T Station",
                   "Northeastern University - \nNorth Parking Lot",
                   "Ruggles T Stop -\n Columbus Ave at\n Melnea Cass Blvd",
                   "Tremont St at \nNorthampton St",
                   "Wentworth Institute of \nTechnology - Huntington Ave\n at Vancouver St")

mod <- ttheme_default(
  core = list(bg_params=list(fill=c("#bdd7e7"))),
  colhead = list(fg_params=list(col="white",parse=TRUE,fontface=2,fontsize=12),
                 bg_params=list(fill="#08519c" )),
  rowhead=list(fg_params=list(col="white",fontface=2),
               bg_params=list(fill="#08519c" ))
                      )
grid.table(tab,theme=mod)
title(main="Count of User Types in Each Start Station", col.main="#08519c")



#clustered column chart for start station & userstype
colnames(tab) <- c("Mass Ave T Station",
                   "Northeastern University - \nNorth Parking Lot",
                   "Ruggles T Stop -\n Columbus Ave at Melnea Cass Blvd",
                   "Tremont St \nat Northampton St",
                   "Wentworth Institute of Technology\n - Huntington Ave at Vancouver St")


bar <- barplot(tab, beside=TRUE, col = c("#bdd7e7","#08519c"  ),
        main="Trips Taken by User Type at Each Start Station", 
        xlab="Start Station Name", ylab="Number of Trips",
        font.lab=2, cex.lab=1.2, cex.main=2, col.main="#08519c" ,
        ylim=c(0,1800),
        legend=unique(df$usertype))


#ggplot

library(ggplot2)

plot.new()
start.plot <- ggplot(df,aes(start.station.name))+
  geom_bar(aes(fill=usertype), position = "dodge")+
  ggtitle("Trips Taken by User Type at Each Start Station")+
  ylab("Number of Trips") + xlab("Start Station Name")+
  labs(fill="User Type")+
  scale_x_discrete(labels=
                     c("Mass Ave T Station",
                       "Northeastern University - \nNorth Parking Lot",
                       "Ruggles T Stop -\n Columbus Ave at Melnea Cass Blvd",
                       "Tremont St \nat Northampton St",
                       "Wentworth Institute of Technology\n - Huntington Ave at Vancouver St"))+
  scale_fill_manual(values = c("#bdd7e7","#08519c"))

start.plot


  

#table for avg trip duration & for unique start location
mass <- subset(df, start.station.name %in% "Mass Ave T Station",
                   select=c(tripduration, start.station.id))
avg.mass <- mean(mass$tripduration)

neu <- subset(df, start.station.name %in% "Northeastern University - North Parking Lot",
               select=c(tripduration, start.station.id))
avg.neu <- mean(neu$tripduration)

rugg <- subset(df, start.station.name %in% "Ruggles T Stop - Columbus Ave at Melnea Cass Blvd",
               select=c(tripduration, start.station.id))
avg.rugg <- mean(rugg$tripduration)

trem <- subset(df, start.station.name %in% "Tremont St at Northampton St",
               select=c(tripduration, start.station.id))
avg.trem <- mean(trem$tripduration)

went <- subset(df, start.station.name %in% "Wentworth Institute of Technology - Huntington Ave at Vancouver St",
               select=c(tripduration, start.station.id))
avg.went <- mean(went$tripduration)

table(df$start.station.name, c(avg.mass,avg.neu,avg.rugg,avg.trem,avg.went))


  ##code to test data accuracy by exporting to excel
  #write_xlsx(mass,"C:\\PSEUDO DESKTOP\\Homework\\GRADUATE\\Fall 2021\\MISM6202\\testmass.xlsx")


# table for avg trip duration
tab <- round(c(avg.mass,avg.neu,avg.rugg,avg.trem,avg.went),digits = 3)
tab <- as.data.frame(tab)
rownames(tab) <- c("Mass Ave T Station",
                   "Northeastern University - \nNorth Parking Lot",
                   "Ruggles T Stop -\n Columbus Ave at\n Melnea Cass Blvd",
                   "Tremont St at \nNorthampton St",
                   "Wentworth Institute of \nTechnology - Huntington Ave\n at Vancouver St")
colnames(tab) <- c("Average trip duration (mins)") 
tab

mod <- ttheme_default(
  core = list(bg_params=list(fill=c("#bdd7e7"))),
  colhead = list(fg_params=list(col="white",parse=TRUE,fontface=2,fontsize=12),
                 bg_params=list(fill="#08519c" )),
  rowhead=list(fg_params=list(col="white",fontface=2),
               bg_params=list(fill="#08519c" ))
)
grid.table(t(tab),theme=mod, 
           title(main = "Average Trip Duration for Start Station", col.main="#08519c" )) 




#using diplyr for above part
library(dplyr)

avg.trip <- df %>%
  group_by(start.station.name) %>%
  summarise(avg_trip=round(mean(tripduration),3))
  

colnames(avg.trip) <- c("Start Station Names", "Average trip duration (mins)")

library(gridExtra)

plot.new()
mod <- ttheme_default(
  core = list(bg_params=list(fill=c("#bdd7e7"))),
  colhead = list(fg_params=list(col="white",parse=TRUE,fontface=2,fontsize=12),
                 bg_params=list(fill="#08519c" )),
  rowhead=list(fg_params=list(col="white",fontface=2),
               bg_params=list(fill="#08519c" ))
)

grid.table(avg.trip,theme=mod,
           title(main = "Average Trip Duration for Start Station", col.main="#08519c")) 



#table for average trip duration and ‘usertype’ 
cust <- subset(df, usertype %in% "Customer", select=tripduration)
avg.cust <- mean(cust$tripduration)

sub <- subset(df, usertype %in% "Subscriber", select=tripduration)
avg.sub <- mean(sub$tripduration)

tab <- round(c(avg.cust,avg.sub),digits = 3)
tab <- as.data.frame(tab)
rownames(tab) <- c("Customer","Subscriber")
colnames(tab) <-"Average trip duration (mins)"
tab

plot.new()
mod <- ttheme_default(
  core = list(bg_params=list(fill=c("#bdd7e7"))),
  colhead = list(fg_params=list(col="white",fontface=2),
                 bg_params=list(fill="#08519c")),
  rowhead=list(fg_params=list(col="white",fontface=2),
               bg_params=list(fill="#08519c"))
)
grid.table(t(tab),theme=mod) 
title(main="Average Trip Duration for User Type", col.main="#08519c")

# same thing but in dplyr
avg.user <- df %>%
            group_by(usertype) %>%
            summarise(avg_trip=round(mean(tripduration),3))
colnames(avg.user) <- c("     User Type    ","Average Trip Duration (mins)")
avg.user

plot.new()
mod <- ttheme_default(
  core = list(bg_params=list(fill=c("#bdd7e7"))),
  colhead = list(fg_params=list(col="white",fontface=2),
                 bg_params=list(fill="#08519c")),
  rowhead=list(fg_params=list(col="white",fontface=2),
               bg_params=list(fill="#08519c"))
)
grid.table(avg.user,theme=mod) 
title(main="Average Trip Duration for User Type", col.main="#08519c")


#####                                           #####
##                                                 ##
##                  PROBABILITY                    ##
##                                                 ##
#####                                           #####


# probability of selecting subscriber type from dataset
sub.prob <- summary(df$usertype)/nrow(df)
sub.prob

#another method
library(dplyr)
sub.prob <- df %>% filter(usertype=="Subscriber") %>%
            tally() %>%
            summarise(prob_of_customer=n/nrow(df))
sub.prob



#conditional probability assessment - is Subscriber indepdent of Start Station?
#P(U) - prob of usertype subscriber



cond <- df %>% group_by(start.station.name) %>%
        summarise(prob_of_sub = round(mean(usertype=="Subscriber"),3)) %>%
        mutate(cond=ifelse(prob_of_sub!=sub.prob, "TRUE, it is != 0.724", "FALSE it is = 0.724"))

colnames(cond) <- c("Start Station Name", "Subscriber Probability",
                    "Dependence")
cond


plot.new()
mod <- ttheme_default(
  core = list(bg_params=list(fill=c("#bdd7e7"))),
  colhead = list(fg_params=list(col="white",fontface=2),
                 bg_params=list(fill="#08519c")),
  rowhead=list(fg_params=list(col="white",fontface=2),
               bg_params=list(fill="#08519c"))
)
grid.table(cond,theme=mod) 
title(main="Test of Dependence Between Subscriber and Start Station", col.main="#08519c")



#####                                           #####
##                                                 ##
##              SAMPLING DISTRIBUTION              ##
##                                                 ##
#####                                           #####


# repeated sampling mean and se(Xbar)
mean(df$tripduration) # repeated sampling mean = population mean = 14.65 for trip<60
mean(start_data$tripduration) #pop mean for all obs

sd(df$tripduration)/sqrt(50) # se for 1 sample w/ known sigma
sd(start_data$tripduration)/sqrt(50) #se for all obs

table(df$usertype)/nrow(df) #proportion for user type
table(start_data$usertype)/nrow(start_data) #proportion for usertype in all obs

phat <- table(df$usertype)/nrow(df)
sqrt((phat*(1-phat))/50)

phat <- table(start_data$usertype)/nrow(start_data)
sqrt((phat*(1-phat))/50)




#####                                           #####
##                                                 ##
##             STATISTICAL INFERENCE               ##
##                                                 ##
#####                                           #####

# random sample of 50 rides
sample.df <- sample_n(df, 50)


#95% CI estimation for pop mean trip duration
avg.50 <- mean(sample.df$tripduration) #mean of 15.237
sd.50 <- sd(sample.df$tripduration) #sd of 9.920
n.50 <- nrow(sample.df) # no. of observations
se.50 <- sd.50/sqrt(n.50) #SE of 1.403
t.50 <- qt(p=1-(0.05/2),df=n.50-1) #t score of 2.010
merror.50 <- t.50 * se.50 #ME of 2.819


upper.ci <- avg.50 + merror.50 #upper CI of 18.056
lower.ci <- avg.50 - merror.50 #lower CI of 12.417
print(c(upper.ci,lower.ci))

#test for trips <60mins
c(mean(df$tripduration)<=upper.ci, lower.ci<= mean(df$tripduration)) 
#test for all obs
c(mean(start_data$tripduration)<=upper.ci, lower.ci<= mean(start_data$tripduration))

#checking with t-test() function
t.test(sample.df$tripduration,conf.level = 0.95)



#95% CI sample proportion calculations for sample.df

p.hat <- mean(sample.df$usertype %in% "Customer")#p.hat is 0.26 for cust & 0.74 for sub
se.p <- sqrt((p.hat*(1-p.hat))/50) #SE of 0.062
z.p <- qnorm(1-(0.05/2)) # z(alpha/2) = 1.96
merror.p <- z.p * se.p #ME is 0.122

upper.p <- p.hat+merror.p
lower.p <- p.hat-merror.p
print(c(upper.p,lower.p)) #CI bounds of 0.3816 & 0.1384

#test for trips <60mins
c(mean(df$usertype%in%"Customer")<=upper.p, lower.p<= mean(df$usertype%in%"Customer"))
#test for all obs
c(mean(start_data$usertype%in%"Customer")<=upper.p, 
  lower.p<= mean(start_data$usertype%in%"Customer"))


#checking with prop.test() function
  #it uses exact CI, which is different from approx CI
prop.test(x=13,n=50,conf.level = 0.95) 




#mu0 dataframe using rides in 1st week of august

data <- df #make new df (with trips <=60mins) to not mess up old one

summary(df$ride.date %in%
        c("2021-08-01","2021-08-02","2021-08-03",
          "2021-08-04","2021-08-05","2021-08-06","2021-08-07"))
#theres 1670 observations in the first week of Aug (1-7)
summary(df$ride.date)  #checking for accuracy
summary(data$ride.date) #checking for accuracy
#ride numbers from Aug1-7 are: 250, 234, 257, 250, 161, 262, 256

#subsetting using base R
week1 <- data[data$ride.date %in% c("2021-08-01","2021-08-02","2021-08-03",
                                     "2021-08-04","2021-08-05","2021-08-06","2021-08-07"),]

#subsetting using dplyr
week1 <- data %>% group_by(ride.date) %>% 
  filter(ride.date %in% c("2021-08-01","2021-08-02","2021-08-03",
                     "2021-08-04","2021-08-05","2021-08-06","2021-08-07")) 

mu0 <- mean(week1$tripduration)
#the mean trip duration of mu0 is 14.557 minutes

summary(mu0)#checking for accuracy
mean(mu0$tripduration)


#week4 dataframe with rides in last week of august

summary(data$ride.date)
#last week of august has a total of 1700 rides
#ride numbers from Aug25-31 are: 260, 236, 238, 218, 225, 263, 260
week4 <- data %>% group_by(ride.date) %>% 
  filter(ride.date %in% c("2021-08-25","2021-08-26","2021-08-27",
                          "2021-08-28","2021-08-29","2021-08-30","2021-08-31"))
summary(week4)
mean(week4$tripduration) #mean of 15.271


#sampling of 100 from week 4
set.seed(999)

sample.100 <- sample(week4$tripduration,100)


# hypothesis testing with alpha=0.05
avg.100 <- mean(sample.100) #mean of 16.54
sd.100 <- sd(sample.100) #sd of 10.77
n.100 <- length(sample.100) #n of 100

t.100 <- (avg.100-mu0) / (sd.100/sqrt(n.100)) #t stat of 1.841


#checking with qt() function in R
pt(t.100, n.100-1,lower.tail = FALSE)



#rerunning the sampling codes
t.crit <- qt(p=0.05, df=n.100-1, lower.tail=FALSE) #t critical value is 1.66


#creating new table for test results
test.table <- data.frame(mean=integer(),sdev=integer(),
                         tstat=integer(),reject=character()) #make empty df to populate values


#for loop to determine rejection/non-rejection of null
for(i in 1:25){

sample.100 <- sample(week4$tripduration,100)
avg.100 <- mean(sample.100)
sd.100 <- sd(sample.100)
n.100 <- length(sample.100)
t.100 <- (avg.100-mu0) / (sd.100/sqrt(n.100))


decision <- ifelse(t.100 >= t.crit, "Reject H0 (p>0.05)", "Don't Reject H0 (p<0.05)")



test.table <- rbind(test.table,
                    data.frame(mean=avg.100, sdev=sd.100,
                    tstat=t.100, reject=decision, stringsAsFactors = FALSE))
}


test.table


#make table to present findings
colnames(test.table) <- c("Sample mean", "Sample St.Dev",
                    "Sample T-stat", "Rejection Decision")
test.table



plot.new()
mod <- ttheme_default(
  core = list(bg_params=list(fill=c("#bdd7e7"))),
  colhead = list(fg_params=list(col="white",fontface=2),
                 bg_params=list(fill="#08519c" )),
  rowhead=list(fg_params=list(col="white",fontface=2),
               bg_params=list(fill="#08519c" ))
)
grid.table(test.table,theme=mod) 



###another method to do Q14
data <- read.csv('~/Downloads/start-data.csv')
colSums(is.null(data))
# Question 14

data$ride.date <- as.Date(data$ride.date)
data$ride.week <- cut(data$ride.date, breaks = 
                        c(as.Date('2021-08-01'),as.Date('2021-08-08'),as.Date('2021-08-15'),
                          as.Date('2021-08-22'),as.Date('2021-09-01')),labels = c("Week1","Week2", 
                                                                                  "Week3","Week4"))


row_to_keep_1 = which(data$ride.week =='Week1')
week1 = data[row_to_keep_1,]
mu0 = mean(week1$tripduration)
row_to_keep_4 = which(data$ride.week =='Week4')
week4 = data[row_to_keep_4,]

set.seed(999)
sampleweek4 <- sample(week4$tripduration, 100, replace = FALSE, prob = NULL)

H0 : mu4 <= mu0
Ha: mu4 > mu0 
alpha = 0.05 

xbar = mean(sampleweek4)
se = sd(sampleweek4)
t = (xbar - mu0)/ (se-sqrt(100))
t_alpha = 1.664 

t > -t_alpha (-0.3435 < -1.664) -> Do not reject H0 
average tripduration in week4 > avg tripduration in week1 

#cancel set seed
set.seed(Sys.time())
set.seed(NULL)
sample2 <- sample(week4$tripduration, 100, replace = FALSE, prob = NULL)
xbar2 = mean(sample2)
se2 = sd(sample2)
t2 = (xbar2 - mu0)/ (se-sqrt(100))
t_alpha = 1.664 

t2 < t_alpha -> Reject H0 
average tripduration in week4 > avg tripduration in week1 





#####                                           #####
##                                                 ##
##                  QUESTION 15                    ##
##                                                 ##
#####                                           #####


#most frequent end stations and relationship to start station

data <- df #making new frame to not mess up old one

head(summary(df$end.station.name)) #finding most popular end stations
#there are 6 stations (w/ >200 rides) totalling 1539 observations


library(dplyr)

end.top5 <- data %>% select(end.station.name,start.station.name) %>%
                filter(end.station.name %in%
                c("Roxbury Crossing T Stop - Columbus Ave at Tremont St",
                  "Huntington Ave at Mass Art",
                  "Christian Science Plaza - Massachusetts Ave at Westland Ave",
                  "Boylston St at Jersey St",
                  "Northeastern University - North Parking Lot",
                  "Brigham Circle - Francis St at Huntington Ave"))%>%
                group_by(end.station.name) %>%
                droplevels()
                
summary(end.top5)


tab <- table(end.top5$end.station.name,end.top5$start.station.name)

colnames(tab) <- c("Mass Ave T Station",
                   "Northeastern University - \nNorth Parking Lot",
                   "Ruggles T Stop -\n Columbus Ave at\n Melnea Cass Blvd",
                   "Tremont St at \nNorthampton St",
                   "Wentworth Institute of \nTechnology - Huntington Ave\n at Vancouver St")

rownames(tab) <- c("Boylston St \nat Jersey St",
                   "Brigham Circle - \nFrancis St at Huntington Ave",
                   "Christian Science Plaza - \nMassachusetts Ave at Westland Ave",
                   "Huntington Ave \nat Mass Art",
                   "Northeastern University - \nNorth Parking Lot",
                   "Roxbury Crossing T Stop - \nColumbus Ave at Tremont St")

summary(end.top5$end.station.name)
summary(end.top5$start.station.name)

plot.new()
mod <- ttheme_default(
  core = list(bg_params=list(fill=c("#bdd7e7"))),
  colhead = list(fg_params=list(col="white",fontface=2, parse=TRUE),
                 bg_params=list(fill="#08519c" )),
  rowhead=list(fg_params=list(col="white",fontface=2, parse=TRUE),
               bg_params=list(fill="#08519c" )) )


colnames(tab) <- c("Mass Ave T Station",
                   "Northeastern University - \nNorth Parking Lot",
                   "Ruggles T Stop -\n Columbus Ave at\n Melnea Cass Blvd",
                   "Tremont St \nat Northampton St",
                   "Wentworth Institute of\n Technology - Huntington Ave\n at Vancouver St")


grid.table(tab,theme=mod) 
title(main="Contingency Table Between Start Stations and the Top 6 End Stations", 
      col.main="#08519c" )


#make barplot for contingency table
library(ggplot2)

plot.new()
end.plot <- ggplot(end.top5,aes(start.station.name))+
  geom_bar(aes(fill=end.station.name), position = "dodge")+
  ggtitle("Number of Trips Started and Ended at Top 6 End Stations")+
  ylab("Number of Trips") + xlab("Start Station Name")+
  labs(fill="End Station Name")+
  scale_x_discrete(labels=
                     c("Mass Ave T Station",
                       "Northeastern University - \nNorth Parking Lot",
                       "Ruggles T Stop -\n Columbus Ave at Melnea Cass Blvd",
                       "Tremont St \nat Northampton St",
                       "Wentworth Institute of Technology\n - Huntington Ave at Vancouver St"))+
 scale_fill_brewer(palette="Blues")

end.plot



#another method for barplot (but this one has really big legends that cant be fixed)
  barplot(tab, beside=TRUE, col = c("#3300FF", "#6600FF", "#6666FF",  "#3366CC" , "#0099FF", 
                                    "lightblue"),
          main="Number of Trips Started and Ended at Top 6 End Stations", 
          xlab="Start Station Name", ylab="Number of Trips",
          font.lab=2, cex.lab=1.2, cex.main=2, col.main="#3366CC")

legend("topright", legend=c("Boylston St at Jersey St",
                            "Brigham Circle - \nFrancis St at Huntington Ave",
                            "Christian Science Plaza - \nMassachusetts Ave at Westland Ave",
                            "Huntington Ave at Mass Art",
                            "Northeastern University - \nNorth Parking Lot",
                            "Roxbury Crossing T Stop -n Columbus Ave at Tremont St"),
       ncol=1, cex=0.8,
       fill=c("#3300FF", "#6600FF", "#6666FF",  "#3366CC" , "#0099FF", "lightblue"))



##comparing ride numbers for end stations during weekday & weekend
library(dplyr)

weekend <- c('(2021-08-01)|(2021-08-07)|(2021-08-08)|(2021-08-14)
             |(2021-08-15)|(2021-08-21)|(2021-08-22)|(2021-08-28)|(2021-08-29)')
#Subset weekends & weekdays
Weekend <- grep(weekend, data$ride.date,perl=T)
w.end = data[Weekend,]
w.day = data[-Weekend,]
g <- w.end %>% group_by(end.station.name) %>% summarise(n = n()) %>% arrange(.,desc(n))
g2 <- w.day %>% group_by(end.station.name) %>% summarise(n = n()) %>% arrange(.,desc(n))
topw.end <- top_n(g,6)
topw.day <- top_n(g2,6)
topw.end
topw.day

w.endd = w.end[w.end$end.station.name %in% c('One Brigham Circle','Landmark Center - Brookline Ave at Park Dr',
                                             'Brigham Circle - Francis St at Huntington Ave',
                                             'Huntington Ave at Mass Art',
                                             'Christian Science Plaza - Massachusetts Ave at Westland Ave',
                                             'Roxbury Crossing T Stop - Columbus Ave at Tremont St'),]
w.dayy = w.day[w.day$end.station.name %in% c('Brigham Circle - Francis St at Huntington Ave',
                                             'Christian Science Plaza - Massachusetts Ave at Westland Ave',
                                             'Northeastern University - North Parking Lot',
                                             'Boylston St at Jersey St',
                                             'Roxbury Crossing T Stop - Columbus Ave at Tremont St',
                                             'Huntington Ave at Mass Art'),]


library(ggplot2)

plot.new()
weekend.plot <- ggplot(w.endd,aes(end.station.name))+
  geom_bar(aes(fill=usertype), position = "dodge")+
  ggtitle("Number of Trips Taken by User Type at Top 6 End Stations on the Weekend")+
  ylab("Number of Trips") + xlab("End Station Name")+
  labs(fill="User Type")+
  scale_x_discrete(labels=
                     c('One Brigham Circle','Christian Science Plaza - \nMassachusetts Ave\n at Westland Ave',
                       'Landmark Center - \nBrookline Ave\n at Park Dr',
                       'Brigham Circle - \nFrancis St at Huntington Ave',
                       'Huntington Ave \nat Mass Art',
                       'Roxbury Crossing T Stop - \nColumbus Ave\n at Tremont St'))+
  scale_fill_manual(values = c("#bdd7e7","#08519c"))

weekend.plot

plot.new()
weekday.plot <- ggplot(w.dayy,aes(end.station.name))+
  geom_bar(aes(fill=usertype), position = "dodge")+
  ggtitle("Number of Trips Taken by User Type at Top 6 End Stations on the Weekday")+
  ylab("Number of Trips") + xlab("End Station Name")+
  labs(fill="User Type")+
  scale_x_discrete(labels=
                     c('Brigham Circle - \nFrancis St at \nHuntington Ave',
                       'Christian Science Plaza - \nMassachusetts Ave \nat Westland Ave',
                       'Boylston St \nat Jersey St',
                       'Huntington Ave \nat Mass Art','Northeastern University - \nNorth Parking Lot',
                       'Roxbury Crossing T Stop - \nColumbus Ave \nat Tremont St'
                     ))+
  scale_fill_manual(values = c("#bdd7e7","#08519c"))

weekday.plot


## END OF CODES
## THANK YOU FOR LOOKING THROUGH THE CODES!

###############################
#              ,              #
#             /|      __      #
#            / |   ,-~ /      #
#           Y :|  //  /       #
#           | jj /( .^        #
#           >-"~"-v"          #
#         /       Y           #
#        jo  o    |           #
#       ( ~T~     j           #
#        >._-' _./            #
#       /   "~"  |            #
#      Y     _,  |            #
#     /| ;-"~ _  l            #
#    |   / l/ ,-"~\           #
#    \//\/      .- \          #
#     Y        /    Y         #
#     l       I     !         #
#     ]\      _\    /"\       #
#   (" ~----( ~   Y.  )       #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###############################