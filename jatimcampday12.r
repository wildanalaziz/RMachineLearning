getwd()
library(twitteR)
library(dplyr)
library(zoo)
library(ggplot2)
library(reshape)
library(VIM)
library(Hmisc)
library(mice)
library(datasets)
library(gganimate)
library(gapminder)
library(tidyr)
library(graphics)
library(RCurl)
library(cowplot)
library(forecast)
library(caret)
library(DMwR)
library(MASS)
library(caTools)
###Getting Collecting Data
#download
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip", 
              destfile = "C:/Users/user/Documents/household_power_consumption.zip")
#unzip
unzip("C:/Users/user/Documents/household_power_consumption.zip")
#read to R
power = read.table("C:/Users/user/Documents/household_power_consumption.txt", sep=";",
                   header=T, na.strings=c("?",""), stringsAsFactors=FALSE)
head(power)
##readcsv
telco = read.csv("TelcoChurn.csv",sep = ",",header=T)
head(telco)
##sedikit penggunaan api untuk crawling dari twitter
consumer_key = '******************'
consumer_secret = '******************'
access_token = '************************' 
access_secret = '**************************************'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets = searchTwitter("#AvengersEndgame", n = 1000, lang = "en")
tweets = twListToDF(tweets)
head(tweets)
#power = power[1:200000,]
#Cleaning and preparing data
str(power)
head(power)
tail(power)
str(telco)
head(telco)
tail(telco)
#covert char to date
power$Date = as.Date(power$Date, format = "%d/%m/%Y")
#create datetime object
power$DateTime = as.POSIXct(paste(power$Date, power$Time))
power$Month = format(power$Date,"%Y-%m")
summary(power)
#menghitung missing value
power$Missing = ifelse(is.na(power$Global_active_power), 1, 0)
sum(power$Missing)
power_group_day = group_by(power,Date)
power_day_missing = summarize(power_group_day, Count_Missing = sum(Missing))
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
calendarHeat(power_day_missing$Date, power_day_missing$Count_Missing, 
             varname="Missing Data", color="w2b")
power$Global_active_power_locf = na.locf(power$Global_active_power)
power_long = melt(power, id.vars= "DateTime",
                  measure.vars= c("Global_active_power","Global_active_power_locf"))
ggplot(power_long,aes(value,fill=variable,color=variable)) +
  geom_density(alpha=1) +
  facet_wrap(~variable)
power_group = group_by(power,Month)
power_monthly = summarize(power_group,
  Max_Demand_kW = max(Global_active_power_locf),
  Total_use_kWh = sum(Global_active_power_locf)/60)
str(power_monthly)
power_monthly = power_monthly[2:47,]
power_monthly$Month = as.Date(paste0(power_monthly$Month, "-01"))
#telco
str(telco)
summary(telco)
max(telco$SeniorCitizen)
telco$SeniorCitizen = as.factor(telco$SeniorCitizen)
summary(is.na(telco))
summary(telco$TotalCharges)
hist(telco$TotalCharges)
histMiss(telco$TotalCharges) #visualize missing value
me = median(telco$TotalCharges,na.rm = T)
tc = telco[,20]
tc[is.na(tc)] <- 999999
tc[tc==999999] = me
telco[,20] = tc
summary(is.na(telco))
mean(telco$TotalCharges)
##data visualisasi demo
data("iris")
ggplot(iris, aes(x = Sepal.Width)) + 	geom_histogram()
ggplot(mtcars, aes(x = factor(cyl), 
                   fill = factor(am))) + geom_bar()
df = data.frame(time=c("breakfeast", "Lunch", "Dinner"),bill=c(10, 30, 15))
ggplot(data=df, aes(x=time, y=bill, group=1)) + geom_line()+ geom_point()
df = data.frame(
  group = c("Male", "Female", "Child"),
  value = c(25, 25, 50))
bp=ggplot(df, aes(x="", y=value, fill=group))+  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0)
pie
data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
ggplot(ToothGrowth, aes(x = dose,y = len)) + geom_boxplot()
#ggcorplot
data(mtcars)
#compute correlation matrix
corr <- round(cor(mtcars),1)
head(corr)
#compute a matrix of corr p-values
p.mat <- cor_pmat(mtcars)
head(p.mat)
#visualize corr matrix
#method square by default
ggcorrplot(corr)
#gganimate
theme_set(theme_bw())
head(gapminder)
##static plot
p = ggplot(
  gapminder,
  aes(x=gdpPercap,y=lifeExp,size=pop,colour=country)
) +
  geom_point(show.legend=F,alpha=0.7)+
  scale_color_viridis_d()+
  scale_size(range=c(2,12)) +
  scale_x_log10()+
  labs(x="GDP per Capita",y="Life Expectancy")
)
p
p + transition_time(year) + 
  labs(title="Year: {frame_time}")
###Kembali ke data power dan telco
#plot data
ggplot(power_monthly,aes(Month, Total_use_kWh)) + 
  geom_line(col="blue",lwd=1)
ggplot(power_monthly, aes(x = power_monthly$Total_use_kWh )) + 	geom_histogram(bins = 30)
power_monthly_separate = power_monthly
power_monthly_separate = power_monthly_separate %>% separate(Month,sep="-",into= c("year","month","day"))
powerp =  ggplot(power_monthly_separate,aes(x=month,y=Total_use_kWh,group=year,color=year)) +
  geom_line(lwd=1) + labs(x="Month",y="Total Use Kwh")
powerp
powerp + transition_reveal(power_monthly$Month)
#untuk data bertype factor, bar chart sangat power full
#1.Tampilkan proporsi pelanggan yang beralih dan tidak
ggplot(telco, aes(x = Churn)) + 
  geom_bar()
#proporsi churn rate
prop.table(table(telco$Churn))
ggplot(telco, aes(x = Churn)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Churn Customer",
       title = "Churn Rate")
#2.Bagaimana hubungan antara churn rate dengan jenis kelamin?
ggplot(telco, aes(x = gender, fill = Churn)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Customer Count",
       title = "Churn Rates by Sex")
#------------------------""------------------- senior citizen?
ggplot(telco, aes(x = SeniorCitizen, fill = Churn)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Customer Count",
       title = "Churn Rates by SeniorCitizen")
options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(telco, aes(x=Partner,fill=Churn))+ geom_bar(), 
          ggplot(telco, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(telco, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(telco, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(telco, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(telco, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill'))
plot_grid(ggplot(telco, aes(x=OnlineBackup,fill=Churn))+ geom_bar(), 
          ggplot(telco, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(telco, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(telco, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(telco, aes(x=StreamingMovies,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(telco, aes(x=Contract,fill=Churn))+ geom_bar(position = 'fill'))
plot_grid(ggplot(telco, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(), 
          ggplot(telco, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill'))
options(repr.plot.width =6, repr.plot.height = 2)
ggplot(telco, aes(x = gender, fill = Churn)) + 
  theme_bw() +
  facet_wrap(~ Partner) +
  geom_bar() +
  labs(y = "Customer Count",
       title = "Churn Rates by Partner and Sex")
ggplot(telco, aes(y= tenure, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")
ggplot(telco, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")
ggplot(telco, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")
#checking correlation
telco_cor = round(cor(telco[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)
ggcorrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))

#########analytic
total_use_ts = ts(power_monthly$Total_use_kWh, start=c(2007,1), frequency=12)
total_use_fc = forecast(total_use_ts, h=12)
summary(total_use_fc)  
plot(total_use_fc)
###telco
telco = telco[,-1]
#create logistic model
logmodel = glm(Churn~.,family=binomial,data=telco)
summary(logmodel)
predlogmodel = predict(logmodel,telco[,-20])
pred_churnlogmodel = factor(ifelse(predlogmodel >= 0.5, "Yes", "No"))
actual_churn = factor(ifelse(telco$Churn=="Yes","Yes","No"))
t = table(actual_churn,pred_churnlogmodel)
cutoff_churn = factor(ifelse(predlogmodel >=0.5, "Yes", "No"))
conf_final = confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy = conf_final$overall[1]
sensitivity = conf_final$byClass[1]
specificity = conf_final$byClass[2]
accuracy
sensitivity
specificity
##############
#dari data visualisasi kita tahu bahwa pada feature terdapat no phone service dan no internet service
telco2 = telco
max(telco2$tenure)
telco2 = data.frame(lapply(telco2,function(x){
  gsub("No internet service","No",x)}))
telco2 = data.frame(lapply(telco2,function(x){
  gsub("No phone service","No",x)}))
##standardize continus feature
numeric_feat = c("tenure","MonthlyCharges","TotalCharges")
telco2[numeric_feat] = sapply(telco2[numeric_feat], as.numeric)
telco_int = telco2[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int = data.frame(scale(telco_int))
#creat deriving feauture
telco2 = mutate(telco2,tenure_bin = tenure)
telco2$tenure_bin
telco2$tenure_bin[telco2$tenure_bin >=0 & telco2$tenure_bin <= 12] <- '0-1 year'
telco2$tenure_bin[telco2$tenure_bin > 12 & telco2$tenure_bin <= 24] <- '1-2 years'
telco2$tenure_bin[telco2$tenure_bin > 24 & telco2$tenure_bin <= 36] <- '2-3 years'
telco2$tenure_bin[telco2$tenure_bin > 36 & telco2$tenure_bin <= 48] <- '3-4 years'
telco2$tenure_bin[telco2$tenure_bin > 48 & telco2$tenure_bin <= 60] <- '4-5 years'
telco2$tenure_bin[telco2$tenure_bin > 60 & telco2$tenure_bin <= 73] <- '5-6 years'
telco2$tenure_bin <- as.factor(telco2$tenure_bin)
ggplot(telco2, aes(tenure_bin, fill = tenure_bin)) + geom_bar()+ theme_bw()
##create dummy variable
telco2[,21]
telco_cat <- telco2[,-c(5,18,19)]
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))
head(dummy)
#combining
telco_final = cbind (telco_int,dummy)
telco_final$Churn = as.factor(telco_final$Churn)
str(telco_final)
#balancing data
nrow(telco_final)-sum(telco_final$Churn)
args(SMOTE)
balancedata = SMOTE(Churn~.,telco_final,k=5)
table(balancedata$Churn)
ggplot(balancedata, aes(x = Churn)) + 
  geom_bar()
##model1 semua variabel
model1 = glm(Churn ~., data=balancedata,family = "binomial")
print(model1)
summary(model1)
model2 <- stepAIC(model1,direction="both")
model2 <- glm(formula = Churn ~ tenure + MonthlyCharges + TotalCharges + 
                Partner + Dependents + PhoneService + InternetService.xFiber.optic + 
                InternetService.xNo + OnlineSecurity + OnlineBackup + DeviceProtection + 
                TechSupport + StreamingTV + StreamingMovies + Contract.xOne.year + 
                Contract.xTwo.year + PaperlessBilling + PaymentMethod.xElectronic.check + 
                PaymentMethod.xMailed.check + tenure_bin.x1.2.years + tenure_bin.x3.4.years + 
                tenure_bin.x4.5.years + tenure_bin.x5.6.years, family = "binomial", 
              data = train)
summary(model2)
####
set.seed(123)
indices = sample.split(balancedata$Churn, SplitRatio = 0.8)
train = balancedata[indices,]
validation = balancedata[!(indices),]
model1 <- glm(Churn~.,data=train,family='binomial')
summary(model1)
stepAIC(model1)
model2 <- glm(Churn~tenure + MonthlyCharges + Partner + Dependents + PhoneService + 
                InternetService.xFiber.optic + InternetService.xNo + OnlineSecurity + 
                OnlineBackup + DeviceProtection + TechSupport + StreamingTV + 
                StreamingMovies + Contract.xOne.year + Contract.xTwo.year + 
                PaperlessBilling + PaymentMethod.xCredit.card..automatic. + 
                PaymentMethod.xElectronic.check + tenure_bin.x1.2.years + 
                tenure_bin.x3.4.years + tenure_bin.x4.5.years + tenure_bin.x5.6.years,data=train,family = "binomial")
model3 <- stepAIC(model2,direction = 'both')
model3 <- glm(Churn ~ tenure + MonthlyCharges +  Partner + Dependents + 
                PhoneService + InternetService.xFiber.optic + InternetService.xNo + 
                OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + 
                StreamingTV + StreamingMovies + Contract.xOne.year + Contract.xTwo.year + 
                PaperlessBilling + PaymentMethod.xElectronic.check + PaymentMethod.xMailed.check + 
                tenure_bin.x1.2.years + tenure_bin.x3.4.years + tenure_bin.x4.5.years + 
                tenure_bin.x5.6.years,family='binomial',train)
summary(model3)
model4 <- glm(Churn ~ tenure + MonthlyCharges +  Partner + Dependents + 
                PhoneService + InternetService.xFiber.optic + InternetService.xNo + 
                OnlineSecurity + OnlineBackup + TechSupport + 
                StreamingTV + StreamingMovies + Contract.xOne.year + Contract.xTwo.year + 
                PaperlessBilling + PaymentMethod.xElectronic.check + PaymentMethod.xMailed.check + 
                tenure_bin.x1.2.years + tenure_bin.x3.4.years + tenure_bin.x4.5.years + 
                tenure_bin.x5.6.years,family='binomial',train)
summary(model4)
model5 <-glm(Churn ~ tenure + MonthlyCharges +  Partner + Dependents + 
               PhoneService + InternetService.xFiber.optic + InternetService.xNo + 
               OnlineSecurity + OnlineBackup + TechSupport + 
               StreamingTV +  Contract.xOne.year + Contract.xTwo.year + 
               PaperlessBilling + PaymentMethod.xElectronic.check + PaymentMethod.xMailed.check + 
               tenure_bin.x1.2.years + tenure_bin.x3.4.years + tenure_bin.x4.5.years + 
               tenure_bin.x5.6.years,family='binomial',train)
summary(model5)
grep("Churn",colnames(validation))
final_model <- model5
predf <- predict(final_model,type = 'response',newdata = validation[,-24])
summary(predf)
validation$prob <- predf
predf_churn = factor(ifelse(predf>=0.50,"Yes","No"))
actual_churnf = factor(ifelse(validation$Churn==1,"Yes","No"))
table(actual_churnf,predf_churn)
cutoff_churnf <- factor(ifelse(predf >=0.50, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churnf, actual_churnf, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity
