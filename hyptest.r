#Exploratory Data Analysis
#---Find Duplicate Customer ID's----
#How many distinct customer ids there are? 
length(unique(CustomerData$CID))

# Sum up total spending by customer
s<-sum(CustomerData$spend)
s
#Find the average and median spending of the customers
spent<-median(CustomerData$spend)
spent
customer_mean<-mean(CustomerData$spend)
customer_mean

#What % of customers have a total spend less than median spend 
dt <- data.table(CustomerData,spent)
dt2 <- dt[,list (sumamount = sum(spend),(sum(spend) <= spent), freq = .N), by = c("CID")]
dt3<-dt2[,list(dt2[!duplicated(dt2$CID)])]
dt3
dt2
dt3
X<-sum(dt3$V2=="TRUE")
Y<-sum(dt3$V2=="FALSE")
Z<-((X/(X+Y))*100)
X
Y
Z
dtz <- dt[,list (sumamount = sum(spend), freq = .N), by = c("month")]
dtz

X<-sum(dt3$V2=="TRUE")
Y<-sum(dt3$V2=="FALSE")
Z<-((X/(X+Y))*100)
dt2
dt
X
Y
Z


# Calculate total_spending by Customer in December & May
Q<-filter(CustomerData,month %in% c ('7','8','9','10','11','12'))
L<-filter(CustomerData,month %in% c ('1','2','3','4','5','6'))
S<-filter(CustomerData,month == '5')
K<-filter(CustomerData,monCustomerDatath == '12')

# Test if total_spending in May is same as total_spending in Dec
t.test(S$spend,K$spend, alternative = "two.sided", var.equal = FALSE)
# Write the null, alternate hypothesis, what test you used, and do you reject the null and why

# Calculate total_spending by Customer in November & September
C<-filter(CustomerData,month == '11')
O<-filter(CustomerData,month == '9')
t.test(S$spend,K$spend, alternative = "two.sided", var.equal = FALSE)
var.test(K$spend, C$spend, alternative = "greater")

v1<-var(O$spend)
v2<-var(K$spend)
v3<-v1/v2
v3

#Correlation between impressions and clicks? What does it intuitively mean? 
n <- cor.test(L$clicks, L$impressions, method = "pearson")
n
g<-cor.test(Q$clicks, Q$impressions, method = "pearson")
g
#What is the correlation between clicks and impression between first and second half of the year 
r <- cor.test(CustomerData$clicks, CustomerData$impressions, method = "pearson")
r
#Scatter plot between clicks and impression. What is your POV on nature of correlation?
ggscatter(CustomerData, x = "clicks", y = "impressions",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "clicks", ylab = "impression")

#Compute CTR
##data$ctr = data$clicks/data$impressions
##Compare CTR between quarters at 95% sig level (alpha = .05)
#Compute anova
my_data <- data.table[,list(sumamount = sum(spend),(sum(spend) <= spent), freq = .N), by = c("CID","AID")]
my_data<-data.table(CustomerData)

my_data

DAT<-data.frame(CustomerData$month==c("1", "2","3"), 
                CustomerData=c("1000", "1300"),
                Clicks=c("500", "600"),

data<-aggregate(cbind(clicks,impressions)~month, dt, sum)
merging <- mutate(anova, Quarter1=data$month=='1'|data$month=='2'|data$month=='3',Quarter2=data$month=='4'|data$month=='5'|data$month=='6',Quarter3=data$month=='7'|data$month=='8'|data$month=='9',Quarter4=data$month=='10'|data$month=='11'|data$month=='12')
merging
res.aov <- aov(anova ~ V2, datah )
res.aov
summary(res.aov)
ctr<- (x/y)
ctr
x<-sum(CustomerData$clicks)
x
y<-sum(CustomerData$impressions)
y

data<-aggregate(cbind(clicks,impressions)~month, dt, sum)
data
anova<-(data$clicks/data$impressions)
anova
datah<-data.table(anova,data$month)
datah
res.aov <- aov(anova ~ V2, datah )
res.aov
summary(res.aov)


