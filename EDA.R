
setwd("Documents/Anupama Data Science/GreatLakes/Capstone")
getwd()

toload_libraries <- c("reshape2", "rpsychi", "car", "psych", "corrplot", "forecast", "GPArotation", "psy", "MVN", "DataExplorer", "ppcor", "Metrics", "foreign", "MASS", "lattice", "nortest", "Hmisc","factoextra", "nFactors")
#new.packages <- toload_libraries[!(toload_libraries %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
lapply(toload_libraries, require, character.only= TRUE)
library(funModeling)
library(Hmisc)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(PerformanceAnalytics)
library(nFactors)
library(psych)
library(flextable)
library(officer)
library(GGally)
library(caTools)
library(data.table)
library(ROCR)
library(class)
library(gmodels)
library(naivebayes)
library(gridExtra)
library(caret)
library(dplyr)



Insurance_Claims_Data <- readxl::read_xlsx("/Users/anupama/Documents/Anupama Data Science/GreatLakes/Capstone/Insurance_Claims/Insurance_Claims_Data.xlsx")

View(Insurance_Claims_Data)   
str(Insurance_Claims_Data)
dim(Insurance_Claims_Data)  #75,200 records and 32 data points or columns
class(Insurance_Claims_Data)
#attach(Insurance_Claims_Data)

summary(Insurance_Claims_Data)

#converting into a data frame
Insurance_Claims_Data<- as.data.frame(Insurance_Claims_Data)

str(insurance)
df_stats= autofit(flextable(df_status(Insurance_Claims_Data)))


names(insurance)



#converting all character to factor variables
aa <- Insurance_Claims_Data %>%
  mutate_if(sapply(Insurance_Claims_Data, is.character), as.factor)

#checking the missing values, data type
df_status(aa)
str(aa)

#no missing values
# all unique claim ids

q=profiling_num(aa)
write.csv(q,"q.csv")
str(aa)

#removing the unique key
aa= aa[,-1]

describe(aa$Txt_Policy_Year) #97% of the data claims are from 2009-2012
describe(aa$DRV_CLAIM_STATUS) #94.8% claims are accepted(closed) and only 0.05 are rejected

summary(aa)

describe(aa$Boo_Endorsement) #32.8% policies have amendment option

#using dplyr to understand the relation between  claim status over different categorical and continuous variables 


#insights:
# 67.22% of the Claims do not have a Boo_Endorsement or "no amendments" can be made to the policy
# 35% of the vehicles are new and had claims in the same year as when they were bought
# 98.5% of the claims had a wider cover
# 93.2% claim cases did not have a total loss
# 5% claim cases do not have a claim amount. this needs to be further investigated, what claim status are they.
# 80.2% claims were not given an anti theft discount
#  73.5% claim cases were not given an NCB discount




#Uni-variate Analysis - Explore each variable and understand it deeper
#attach(aa)

describe(aa$Txt_Policy_Year)
class(aa$Txt_Policy_Year)
freq(aa,"Txt_Policy_Year", plot = T)
# This is a factor, hence this needs to be converted to a date field if any calculation has to be made.
# Policy Year of the claims are as old as 1998-99 to as recent as 2012-13. 
# Approx. 85.8% of the data for policy year of the claims are done in between years 2010-2012

describe(Boo_Endorsement)
# As mentioned above 67.2% claims has no amendment policy where as 32.8% claims 

describe(Txt_Location_RTA)
class(Txt_Location_RTA)
#there are 5578 unique cities/locations from where the RTA registered for the claims

describe(Txt_Policy_Code)
freq(aa$Txt_Policy_Code, plot= TRUE)

levels(aa$Txt_Policy_Code)
levels(aa$Txt_Policy_Code)[levels(aa$Txt_Policy_Code)== "1"]= "Liability Only"
levels(aa$Txt_Policy_Code)[levels(aa$Txt_Policy_Code)== "2"]= "Package Policy"
levels(aa$Txt_Policy_Code)[levels(aa$Txt_Policy_Code)== "6"]= "Liability Only-Fire"
levels(aa$Txt_Policy_Code)[levels(aa$Txt_Policy_Code)== "7"]= "Liability Only-Theft"
levels(aa$Txt_Policy_Code)[levels(aa$Txt_Policy_Code)== "8"]= "Liability Only-Fire & Theft"

#all claims are of either Liability only, Package Policy, Liability only with Fire, Liability only with Theft, Liability only with fire and theft
#also 99.8% of the claims are for Package Policy

describe(Txt_Class_Code)


levels(aa$Txt_Class_Code)
levels(aa$Txt_Class_Code)[levels(aa$Txt_Class_Code)== "11"]= "Private Car"
levels(aa$Txt_Class_Code)[levels(aa$Txt_Class_Code)== "13"]= "Private Car_3wheelers_le750cc"
levels(aa$Txt_Class_Code)[levels(aa$Txt_Class_Code)== "14"]= "Two Wheelers"
levels(aa$Txt_Class_Code)[levels(aa$Txt_Class_Code)== "17"]= "Public Goods Vehicles_oth_than_3wheelers"
levels(aa$Txt_Class_Code)[levels(aa$Txt_Class_Code)== "18"]= "Private Goods Vehicles_oth_than_3wheelers"
levels(aa$Txt_Class_Code)[levels(aa$Txt_Class_Code)== "19"]= "Public Goods motorised three wheelers and pedal cycles"
levels(aa$Txt_Class_Code)[levels(aa$Txt_Class_Code)== "20"]= "Private Goods motorised three wheelers and pedal cycles"
levels(aa$Txt_Class_Code)[levels(aa$Txt_Class_Code)== "21"]= "Passenger Carrying four wheeled vehicles"
levels(aa$Txt_Class_Code)[levels(aa$Txt_Class_Code)== "22"]= "Passenger Carrying motorised three wheeled vehicles"
levels(aa$Txt_Class_Code)[levels(aa$Txt_Class_Code)== "23"]= "Special type of vehicles"

class= freq(aa,'Txt_Class_Code', plot=F)
#write.csv(class, "class.csv")
# 59.3% of the claims have the vehicle class as Private Cars
# Approx. 21% of the claims are coming from vehicle class with two wheelers
# Approx. 11.8% of the claims are coming from vehicle class with Goods Carrying vehicles other than three wheeler- Public

describe(Txt_Zone_Code)
freq(Txt_Zone_Code, plot=TRUE)


colour= freq(aa, "Txt_Colour_Vehicle")
#write.csv(colour, "colour.csv")
# 36 which is Zone B - Sections 2 ,3,4.C.1 and 4.C.4 contribute around 58.7% of the total claims
#followed by 35 and 39 i.e. Zone A - Sections 2 ,3,4.C.1 and 4.C.4 and  Zone C - Sections 4.A, 4.C.2,4.C.3 and 4.D
#there is small percent of 4.9% which doesn't have a zone, i.e. 40.


aa$Num_Vehicle_Age= as.numeric(aa$Num_Vehicle_Age) #this needs to be changed to numeric
describe(Num_Vehicle_Age)
class(Num_Vehicle_Age)
hist(Num_Vehicle_Age)

#plot_num(aa[,-1])


describe(Txt_CC_PCC_GVW_Code)
class(Txt_CC_PCC_GVW_Code)
freq(Txt_CC_PCC_GVW_Code, plot=TRUE)
#Approx 34% of the claims arise out of Private Cars & taxis whose cc is between 1000 and 1500
#whereas combining code 50 and 51, approx. 49.8% claims arise out of Private cars and Taxis  whose cc is less than 1500.
#combining 50, 51 and 52, approx. 62% claims arise out of private cars and taxis, which is similar to txt_class_code variable

describe(Txt_Colour_Vehicle)
freq(Txt_Colour_Vehicle, plot=TRUE)  #Look at it a little later

class(Txt_Colour_Vehicle)
#black , White and Platinum Metallic Cars have higher claim percent after other color, approx 8%



#Txt_Permit_code
describe(aa$Txt_Permit_Code)
freq(aa$Txt_Permit_Code, plot=TRUE)
#73.7% claims are of commercial vehicles that are holding local permit

levels(aa$Txt_Permit_Code)
levels(aa$Txt_Permit_Code)[levels(aa$Txt_Permit_Code)=="1"] = "Local"
levels(aa$Txt_Permit_Code)[levels(aa$Txt_Permit_Code)=="2"] = "State"
levels(aa$Txt_Permit_Code)[levels(aa$Txt_Permit_Code)=="3"] = "Zonal"
levels(aa$Txt_Permit_Code)[levels(aa$Txt_Permit_Code)=="4"] = "National"
levels(aa$Txt_Permit_Code)[levels(aa$Txt_Permit_Code)=="5"] = "Hilly"


#Txt_nature_goods_code
describe(aa$Txt_Nature_Goods_Code)
freq(aa$Txt_Nature_Goods_Code, plot=TRUE)
levels(aa$Txt_Nature_Goods_Code)[levels(aa$Txt_Nature_Goods_Code)=="1"] = "Hazardous"
levels(aa$Txt_Nature_Goods_Code)[levels(aa$Txt_Nature_Goods_Code)=="2"] = "Others"
#Only 18.84% Claims arised out vehicles that carried hazardous goods


#Txt_Road_type_code
describe(aa$Txt_Road_Type_Code)
freq(aa$Txt_Road_Type_Code, plot=TRUE)
levels(aa$Txt_Road_Type_Code)[levels(aa$Txt_Road_Type_Code)=="1"] ="Hilly"
levels(aa$Txt_Road_Type_Code)[levels(aa$Txt_Road_Type_Code)=="2"] ="Nat/State Highways"
levels(aa$Txt_Road_Type_Code)[levels(aa$Txt_Road_Type_Code)=="3"] ="City/Town Roads"
levels(aa$Txt_Road_Type_Code)[levels(aa$Txt_Road_Type_Code)=="4"] ="District Roads"
levels(aa$Txt_Road_Type_Code)[levels(aa$Txt_Road_Type_Code)=="5"] ="Others"

#only 15% of the claims arise out of district roads, National-State Highways or Hilly Areas
#which means majority of claims arise withing city or others

describe(aa$Txt_Vehicle_Driven_By_Code)
freq(aa$Txt_Vehicle_Driven_By_Code, plot=TRUE)
levels(aa$Txt_Vehicle_Driven_By_Code)[levels(aa$Txt_Vehicle_Driven_By_Code)=="1"] = "Owners"
levels(aa$Txt_Vehicle_Driven_By_Code)[levels(aa$Txt_Vehicle_Driven_By_Code)=="2"] = "Others"
# 35.3% Owners are insurance claimers


#Txt_Driver_Exp_Code
describe(aa$Txt_Driver_Exp_Code)
freq(aa$Txt_Driver_Exp_Code, plot=TRUE)
levels(aa$Txt_Driver_Exp_Code)[levels(aa$Txt_Driver_Exp_Code)=="1"] = "<1year"
levels(aa$Txt_Driver_Exp_Code)[levels(aa$Txt_Driver_Exp_Code)=="2"] = "1-3years"
levels(aa$Txt_Driver_Exp_Code)[levels(aa$Txt_Driver_Exp_Code)=="3"] = "3-5years"
levels(aa$Txt_Driver_Exp_Code)[levels(aa$Txt_Driver_Exp_Code)=="4"] = "5-10years"
levels(aa$Txt_Driver_Exp_Code)[levels(aa$Txt_Driver_Exp_Code)=="5"] = "10-15years"
levels(aa$Txt_Driver_Exp_Code)[levels(aa$Txt_Driver_Exp_Code)=="6"] = ">15years"
# 35.5% of the claims arise out of highly experienced drivers with more than 15 years
# Another 31.9% claims arise out of new drivers with less than one year of experience


#Txt_Claims_History_Code
describe(aa$Txt_Claims_History_Code)
freq(aa$Txt_Claims_History_Code, plot=TRUE)
levels(aa$Txt_Claims_History_Code)[levels(aa$Txt_Claims_History_Code)=="1"] = "A.No Claims"
levels(aa$Txt_Claims_History_Code)[levels(aa$Txt_Claims_History_Code)=="2"] = "B.1 Claim"
levels(aa$Txt_Claims_History_Code)[levels(aa$Txt_Claims_History_Code)=="3"] = "C.2 Claims"
levels(aa$Txt_Claims_History_Code)[levels(aa$Txt_Claims_History_Code)=="4"] = "D.3 Claims"
levels(aa$Txt_Claims_History_Code)[levels(aa$Txt_Claims_History_Code)=="5"] = "E.4 Claims"
levels(aa$Txt_Claims_History_Code)[levels(aa$Txt_Claims_History_Code)=="6"] = "F.5 Claims & Above"
claimhistory=freq(aa,'Txt_Claims_History_Code', plot=F)
write.csv(claimhistory, "claimhistory.csv")
# Almost 33.3% of the claims data have a history of 1 one claim in the past years
# there is 26.4% claims which are new as well


#Txt_Driver_Qualification_Code
describe(aa$Txt_Driver_Qualification_Code)
freq(aa$Txt_Driver_Qualification_Code, plot=TRUE)
levels(aa$Txt_Driver_Qualification_Code)[levels(aa$Txt_Driver_Qualification_Code)=="1"] ="Below 10th"
levels(aa$Txt_Driver_Qualification_Code)[levels(aa$Txt_Driver_Qualification_Code)=="2"] ="10th Pass"
levels(aa$Txt_Driver_Qualification_Code)[levels(aa$Txt_Driver_Qualification_Code)=="3"] ="12th Pass"
levels(aa$Txt_Driver_Qualification_Code)[levels(aa$Txt_Driver_Qualification_Code)=="4"] ="Grad/PG"
# 30% drivers are Well Educated
# Only  25.7% drivers are less the 10th pass

#Txt_Incurred_Claims_Code
describe(aa$Txt_Incurred_Claims_Code)
freq(aa$Txt_Incurred_Claims_Code, plot=TRUE)
#Ranking based on previous claims experience, 1 being the highest and 9 being the lowest
# 40% claims have been rated as 1, which indicates high ranking for claims experience or in other words the insurers are highly satisfied with their previous claims experience and based on that they load their premiums

#Txt_TAC_NOL_Code

#grouping the nature of loss
describe(aa$Txt_TAC_NOL_Code)
freq(aa$Txt_TAC_NOL_Code, plot=TRUE)
NOL_freq=freq(aa, 'Txt_TAC_NOL_Code', plot = F)
NOL_freq[1:5,]
No_freq1=NOL_freq[1:5,]
  

aa$Loss_Nature= ifelse(as.numeric(aa$Txt_TAC_NOL_Code) %in% as.numeric(No_freq1$Txt_TAC_NOL_Code), aa$Txt_TAC_NOL_Code, 'other' )

aa$Loss_Nature= as.factor(aa$Loss_Nature)
freq(aa$Loss_Nature, plot=TRUE)

levels(aa$Loss_Nature)[levels(aa$Loss_Nature)=="17"] ="Accident External Means"
levels(aa$Loss_Nature)[levels(aa$Loss_Nature)=="25"] ="Others"
levels(aa$Loss_Nature)[levels(aa$Loss_Nature)=="18"] ="Theft of Entire Vehicle"
levels(aa$Loss_Nature)[levels(aa$Loss_Nature)=="1"] ="Fire"
levels(aa$Loss_Nature)[levels(aa$Loss_Nature)=="27"] ="Accessories-Two wheelers"
levels(aa$Loss_Nature)[levels(aa$Loss_Nature)=="other"] ="Others"
#86.4% claims are of Accident External Means Nature where as a small 5.2% are of Theft of Entire Vehicle.

#Variable Transformation
describe(Num_IDV)
summary(Num_IDV)
class(Num_IDV)
quantile(Num_IDV, probs = c( 0.25,0.50,0.75,0.99,1))
range(Num_IDV)
boxplot(Num_IDV, col="darkgreen" ,horizontal = T, main="Boxplot of Sum Insured") #this wasn't very helpful

tukey_outlier(Num_IDV)
max(Num_IDV)

quantiles <- quantile(Num_IDV, probs = c(0.05,0.99), na.rm =TRUE)

aa$Num_IDV.= ifelse(aa$Num_IDV> quantiles[4],quantiles[4], aa$Num_IDV)
summary(aa$Num_IDV.treat)

par(mfrow=c(2,2))
hist(aa$Num_IDV,col="blue", main="Insured Sum")
hist(aa$DRV_CLAIM_AMT,col="blue", main = "Claim Amount")
hist(aa$Num_Net_OD_Premium,col="blue", main = "OD Premium")
hist(aa$Num_Vehicle_Age, col="blue", main= "Vehicle Age")


#Capping the outliers

pcap <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.01, .99 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}

aa1= pcap(aa)


colnames(aa1)

#rechecking the histograms post outlier treatment
par(mfrow=c(2,2))
hist(aa1$Num_IDV,col="blue", main="Insured Sum")
hist(aa1$DRV_CLAIM_AMT,col="blue", main = "Claim Amount")
hist(aa1$Num_Net_OD_Premium,col="blue", main = "OD Premium")
hist(aa1$Num_Vehicle_Age, col="blue", main= "Vehicle Age")

# Checking the histogram of numeric variables
plot_num(aa1[,c(7,10,27,31,33,35)])
plot_density(aa1[,c(7,10,27,31)], title ="Density Plots")

dev.off()
# Box plots for Numerical Variables
boxplot(aa1$Num_Vehicle_Age, aa1$Num_IDV, aa1$DRV_CLAIM_AMT, aa1$Num_Net_OD_Premium,
        main = "Multiple boxplots for comparision",
        at = c(1,2,3,4),
        names = c("Age","insuredSum","ClaimAmt","ODPrem"),
        las = 3, cex=0.4,
        col = c("yellow","red","green","blue"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
# Interpreting the box plots:
#*  Insured Sum & Claim Amount after treating the outliers, still has outliers post capping to 99th percentile;
#*  OD premium and Vehicle Age are in range;

#bivariate analysis

library(gridExtra)
p1 = ggplot(aa1, aes(Num_IDV, fill= status)) + geom_density(alpha=0.4) 
p2 = ggplot(aa1, aes(Num_Vehicle_Age, fill= status)) + geom_density(alpha=0.4)
p3 = ggplot(aa1, aes(DRV_CLAIM_AMT, fill= status)) + geom_density(alpha=0.4)
p4 = ggplot(aa1, aes(Num_Net_OD_Premium, fill= status)) + geom_density(alpha=0.4)
p5 =ggplot(aa1, aes(PaymentDays, fill= status)) + geom_density(alpha=0.4)
p8 = ggplot(aa1, aes(x=DRV_CLAIM_STATUS, color = DRV_CLAIM_STATUS, fill= DRV_CLAIM_STATUS))+theme_bw()+
  geom_bar()+labs(y="counts", title= "Claim Status")

grid.arrange(p1, p2, p3, p4,p5, p8, ncol = 2, nrow = 3)


#Boxplots
plot_boxplot(aa1[,c(7,10,27,28,31,33,35)], by = "DRV_CLAIM_STATUS", 
             geom_boxplot_args = list("outlier.color" = "red"))

#Looking at the box plots, except for the claim amount which is almost negligible for claims rejected, rest other numerical variables do not show a very different trend for closed vs rejected claims.


#checking the correlation of numerical variables:
plot_correlation(aa[,c(7,10,27,31)])


# More the Age of the vehicle, lesser the claim amount and Sum Insured
# Sum Insured is positively correlated with Claim Amount and Net Own Damage Premium


library(GGally)
ggpairs(aa[,c(7,10,27,31,32)],
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))

#Exploring the Relation between different categorical variables
xtabs(~Txt_Claim_Year+DRV_CLAIM_STATUS,data=aa1)
xtabs(~Txt_Claims_History_Code+DRV_CLAIM_STATUS, data= aa1)

colnames(aa1)

aa1$status= ifelse(aa1$DRV_CLAIM_STATUS == "REJECTED", 1, 0)
aa1$status= as.factor(aa1$status)

#Chi- SQuare test for Categorical variables check 
chisq.test(table(aa1$Txt_Policy_Year,aa1$status)) #Significant
chisq.test(table(aa1$Boo_Endorsement,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Location_RTA,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Policy_Code,aa1$status))
chisq.test(table(aa1$Txt_Class_Code,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Zone_Code,aa1$status)) #Significant
chisq.test(table(aa1$Txt_CC_PCC_GVW_Code,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Colour_Vehicle,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Permit_Code,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Nature_Goods_Code,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Road_Type_Code,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Vehicle_Driven_By_Code,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Driver_Exp_Code,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Claims_History_Code,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Incurred_Claims_Code,aa1$status)) #Significant
chisq.test(table(aa1$Boo_TPPD_Statutory_Cover_only,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Claim_Year,aa1$status)) #Significant
chisq.test(table(aa1$Txt_Place_Accident,aa1$status)) #Significant
chisq.test(table(aa1$Txt_TAC_NOL_Code,aa1$status)) #Significant
chisq.test(table(aa1$Boo_OD_Total_Loss,aa1$status)) #Significant
chisq.test(table(aa1$Boo_AntiTheft,aa1$status)) #Significant
chisq.test(table(aa1$Boo_NCB,aa1$status))  #Significant





# contingency table of dicotomous variables with target variable
cat.data = subset(aa, select = c("Txt_Policy_Year","Txt_Claim_Year","Txt_Policy_Code","Txt_Class_Code","Txt_Vehicle_Driven_By_Code","Txt_Permit_Code", "Txt_Nature_Goods_Code","Txt_Claims_History_Code","Txt_TAC_NOL_Code"
                                  ))

names(cat.data)
# for 6 categorical variables draw the barplot w.r.t to target variable
par(mfrow=c(3,3))
for (i in names(cat.data)) {
  print(i)
  print(table(status, cat.data[[i]]))
  barplot(table(status, cat.data[[i]]),
          col=c("grey","red"),
          main = names(cat.data[i]))
}


# Additional Variable creation

#Payment Days
df$diff_in_days<- difftime(df$datevar1 ,df$datevar2 , units = c("days"))

aa1$PaymentDays = difftime(aa1$Date_Disbursement, aa1$Date_Claim_Intimation, units = c("days"))
aa1$PaymentDays= as.numeric(aa1$PaymentDays)
aa1$PaymentDays[is.na(aa1$PaymentDays)]= 0

#intimation flag and days
aa1$intimation_flag = ifelse(aa1$Date_Accident_Loss == aa1$Date_Claim_Intimation, 1, 0)
aa1$intimation_flag = as.factor(aa1$intimation_flag)

aa1$IntimationDays = difftime(aa1$Date_Claim_Intimation, aa1$Date_Accident_Loss, units = c("days"))
aa1$IntimationDays= as.numeric(aa1$IntimationDays)

# Claim year Vs Policy year

aa1$policyYear = sapply(strsplit(as.character(aa1$Txt_Policy_Year),"-"), `[`, 1)
aa1$policyYear = as.factor(aa1$policyYear)
aa1$claim_year = sapply(strsplit(as.character(aa1$Txt_Claim_Year),"-"), `[`, 1)
aa1$claim_year = as.factor(aa1$claim_year)

aa1$policy_claim_year_comparison = ifelse(aa1$policyYear == aa1$claim_year, 1, 0)

#variable transformation : 
# DRV_Claim_Status as 1 and 0
#Location_RTA - Cleaning into proper cities
#Payment days - Disbursement date - Intimation date
#Policy year and Claim year is the same -- Flag
#Location_RTA and Place_Accident is same or different Flag
#

#Contigent tables
xtabs(~Txt_Vehicle_Driven_By_Code+Txt_Driver_Exp_Code+status, data= aa)

driver=aa%>%filter(DRV_CLAIM_STATUS== "REJECTED")%>%group_by(Txt_Vehicle_Driven_By_Code, Txt_Driver_Exp_Code)%>%summarise(count=n(),percentage_count=n()/3876)%>%ungroup()%>%arrange(-count)


colnames(aa1)

#Checking for cardinality in variables that have a high number of levels

#Class Code
xtabs(~Txt_Class_Code+ status,data=aa)
prop.table(table(aa$Txt_Class_Code,aa$status))

#Txt_CC_PCC_GVW_Code (auto grouping done)
xtabs(~Txt_CC_PCC_GVW_Code+ status,data=aa)
prop.table(table(aa$Txt_CC_PCC_GVW_Code,aa$status))


#`categ_analysis` is available in "funModeling" >= v1.6, please install it before using it.
Txt_CC_PCC_GVW_Code_profile=categ_analysis(data=aa, input="Txt_CC_PCC_GVW_Code", target = "status")


# Ordering Txt_CC_PCC_GVW_Code_profile by mean_target and then take the first 10 
arrange(Txt_CC_PCC_GVW_Code_profile, -mean_target) %>%  head(.)

#using the technique of auto_grouping 
# Reducing the cardinality for variables : Location, Place_accident, vehicle colour
Txt_CC_PCC_GVW_groups=auto_grouping(data = aa, input = "Txt_CC_PCC_GVW_Code", target="status", n_groups=5, seed = 999)
Txt_CC_PCC_GVW_groups$df_equivalence

Txt_CC_PCC_GVW_groups$recateg_results

#groups of 5
aa=aa %>% inner_join(Txt_CC_PCC_GVW_groups$df_equivalence, by="Txt_CC_PCC_GVW_Code")
summary(aa$Txt_CC_PCC_GVW_Code_rec)
aa$Txt_CC_PCC_GVW_Code_rec= as.factor(aa$Txt_CC_PCC_GVW_Code_rec)
xtabs(~Txt_CC_PCC_GVW_Code_rec +status,data=aa)
prop.table(table(aa$Txt_CC_PCC_GVW_Code_rec,aa$status))


#Colour_vehicle
xtabs(~Txt_Colour_Vehicle +status,data=aa)
prop.table(table(aa$Txt_Colour_Vehicle,aa$status))

#`categ_analysis` is available in "funModeling" >= v1.6, please install it before using it.
Txt_Colour_Vehicle_Code_profile=categ_analysis(data=aa, input="Txt_Colour_Vehicle", target = "status")

# Ordering Txt_Colour_Vehicle_Code_profile by mean_target and then take the first 10 
arrange(Txt_Colour_Vehicle_Code_profile, -mean_target) %>%  head(.)

#using the technique of auto_grouping 
# Reducing the cardinality for variables : Location, Place_accident, vehicle colour
Txt_Colour_Vehicle_Code_groups=auto_grouping(data = aa, input = "Txt_Colour_Vehicle", target="status", n_groups=5, seed = 999)

Txt_Colour_Vehicle_Code_groups$df_equivalence

Txt_Colour_Vehicle_Code_groups$recateg_results

#groups of 5
aa=aa %>% inner_join(Txt_Colour_Vehicle_Code_groups$df_equivalence, by="Txt_Colour_Vehicle")
summary(aa$Txt_Colour_Vehicle_rec)
aa$Txt_Colour_Vehicle_rec= as.factor(aa$Txt_Colour_Vehicle_rec)
xtabs(~Txt_Colour_Vehicle_rec +status,data=aa)
prop.table(table(aa$Txt_Colour_Vehicle_rec,aa$status))



#Txt_TAC_NOL_Code
xtabs(~Txt_TAC_NOL_Code+ status,data=aa)
prop.table(table(aa$Txt_TAC_NOL_Code,aa$status))


#`categ_analysis` is available in "funModeling" >= v1.6, please install it before using it.
Txt_TAC_NOL_Code=categ_analysis(data=aa, input="Txt_TAC_NOL_Code", target = "status")

# Ordering Txt_Colour_Vehicle_Code_profile by mean_target and then take the first 10 
arrange(Txt_TAC_NOL_Code_profile, -mean_target) %>%  head(.)

# using the technique of auto_grouping 
# Reducing the cardinality for variables : Location, Place_accident, vehicle colour
Txt_TAC_NOL_Code_groups=auto_grouping(data = aa, input = "Txt_TAC_NOL_Code", target="status", n_groups=3, seed = 999)

Txt_TAC_NOL_Code_groups$df_equivalence

Txt_TAC_NOL_Code_groups$recateg_results

#groups of 3
aa=aa %>% inner_join(Txt_TAC_NOL_Code_groups$df_equivalence, by="Txt_TAC_NOL_Code")
summary(aa$Txt_TAC_NOL_Code_rec)
aa$Txt_TAC_NOL_Code_rec= as.factor(aa$Txt_TAC_NOL_Code_rec)
xtabs(~Txt_TAC_NOL_Code_rec +status,data=aa)
prop.table(table(aa$Txt_TAC_NOL_Code_rec,aa$status))



#location
xtabs(~Txt_Location_RTA+ status,data=aa)
prop.table(table(aa$Txt_Location_RTA,aa$status))


#`categ_analysis` is available in "funModeling" >= v1.6, please install it before using it.
Txt_Location_RTA_profile=categ_analysis(data=aa, input="Txt_Location_RTA", target = "status")

# Ordering Txt_Colour_Vehicle_Code_profile by mean_target and then take the first 10 
arrange(Txt_Location_RTA_profile, -mean_target) %>%  head(.)

#using the technique of auto_grouping 
# Reducing the cardinality for variables : Location, Place_accident, vehicle colour
Txt_Location_RTA_groups=auto_grouping(data = aa, input = "Txt_Location_RTA", target="status", n_groups= 9, seed = 999)

Txt_Location_RTA_groups$df_equivalence

Txt_Location_RTA_groups$recateg_results

#groups of 9
aa=aa %>% inner_join(Txt_Location_RTA_groups$df_equivalence, by="Txt_Location_RTA")
summary(aa$Txt_Location_RTA_rec)
aa$Txt_Location_RTA_rec= as.factor(aa$Txt_Location_RTA_rec)
xtabs(~Txt_Location_RTA_rec +status,data=aa)
prop.table(table(aa$Txt_Location_RTA_rec,aa$status))



#Place of Accident
xtabs(~Txt_Place_Accident+ status,data=aa)
prop.table(table(aa$Txt_Place_Accident,aa$status))

#`categ_analysis` is available in "funModeling" >= v1.6, please install it before using it.
Txt_Place_Accident_profile=categ_analysis(data=aa, input="Txt_Place_Accident", target = "status")

# Ordering Txt_Colour_Vehicle_Code_profile by mean_target and then take the first 10 
arrange(Txt_Place_Accident_profile, -mean_target) %>%  head(.)

#using the technique of auto_grouping 
# Reducing the cardinality for variables : Location, Place_accident, vehicle colour
Txt_Place_Accident_groups=auto_grouping(data = aa, input = "Txt_Place_Accident", target="status", n_groups=9, seed = 999)

Txt_Place_Accident_groups$df_equivalence
Txt_Place_Accident_groups$recateg_results

#groups of 9
aa=aa %>% inner_join(Txt_Place_Accident_groups$df_equivalence, by="Txt_Place_Accident")
summary(aa$Txt_Place_Accident_rec)
aa$Txt_Place_Accident_rec= as.factor(aa$Txt_Place_Accident_rec)
xtabs(~Txt_Place_Accident_rec +status,data=aa)
prop.table(table(aa$Txt_Place_Accident_rec,aa$status))



df= aa[,-c(3,8,9,20:25,28)]
save(df, file="groupdf.RData")
save(aa, file="groupAA.RData")



library(GGally)

ggpairs(df,aes(color = status),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))  

chart.Correlation(carsNum, histogram = T, pch=15)