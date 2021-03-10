

##################################################################### PRE PROCESSING ###############################################################
####################################################################################################################################################
####################################################################################################################################################

#Load libraries
library(readr)
library(car)
require(lmtest)
require(plm)
require(visreg)
require(dplyr)
require(psych)
require(ggplot2)
require(ggpubr)
require(methods)
require(caTools)
require(splines)
library(boot)
library(stargazer)
library(MASS)
library(klaR)
library(gbm)
library(caret)
library(gbm)
library(randomForest)
require(caret)
library(caret)
require(foreign)
require(nnet)
require(reshape2)
library(e1071)
library(factoextra)
library(NbClust)
library(ggfortify)
library(RColorBrewer)
library(dplyr)
library(readr)
library(gridExtra)
#install.packages('caret')
#install.packages('generics')


##### READ THE DATA
hospital_data <- read_csv("C:/Users/Sophie/Downloads/mimic3d.csv")
#hospital_data <- read_csv("Final/mimic3d.csv")
attach(hospital_data)

require(dplyr)

#rename hospital data to consistent format
hospital_data <- hospital_data%>%
  rename(
    patient_id = hadm_id,
    LOS_days = LOSdays,
    admit_diagnosis = AdmitDiagnosis,
    num_callouts = NumCallouts,
    num_diagnosis = NumDiagnosis,
    num_procedures = NumProcs,
    admit_procedure = AdmitProcedure,
    num_CPT_events = NumCPTevents,
    num_inputs = NumInput,
    num_labs = NumLabs,
    num_microlabs = NumMicroLabs,
    num_notes = NumNotes,
    num_outputs = NumOutput,
    num_drugs = NumRx,
    num_procedural_events = NumProcEvents,
    num_transfers = NumTransfers,
    num_chart_events = NumChartEvents,
    expired_hospital = ExpiredHospital,
    total_num_interactions = TotalNumInteract,
    LOS_group = LOSgroupNum
  )
attach(hospital_data)

########STEP 1: NULL VALUES########## 
#note that missing values from the continuous variables have already been imputed with the average by the creator 
lapply(hospital_data,function(x) { length(which(is.na(x)))})
#admit diagnosis, religion, and marital status have null values and will be encoded as their respective "unknown" grouping category 
hospital_data$admit_diagnosis[is.na(hospital_data$admit_diagnosis)] <- 'OTHER'
hospital_data$religion[is.na(hospital_data$religion)] <- 'NOT SPECIFIED'
hospital_data$marital_status[is.na(hospital_data$marital_status)] <- 'UNKNOWN (DEFAULT)'

########STEP 2: DROP NON-PREDICTIVE VARIABLES########## 
#drop patient ID, a unique identifier categorical variable 
hospital_data = select(hospital_data, -patient_id)
#drop admit procedure
hospital_data = select(hospital_data, -admit_procedure)
#drop the pre-grouped LOS variable 
hospital_data = select(hospital_data, -LOS_group)


########STEP 3: RECODE TARGET VARIABLE ########## 


#divide target into relatively equal sized bins, and regroup 

length(LOS_days[LOS_days <= 5]) #23K
length(LOS_days[LOS_days > 5 & LOS_days <= 10]) #18K
length(LOS_days[LOS_days > 10]) #17K 


hospital_data$LOS_days[hospital_data$LOS_days <= 5] <- 0
hospital_data$LOS_days[hospital_data$LOS_days > 5 & hospital_data$LOS_days <= 10] <- 1
hospital_data$LOS_days[hospital_data$LOS_days > 10] <- 2


########STEP 4: RECODE CATEGORICAL VARIABLES INTO FEWER CLASSES########## 
#drop patient ID, a unique identifier categorical variable 

#recode admit location class data class 
hospital_data$admit_location[hospital_data$admit_location == '** INFO NOT AVAILABLE **'] <- 'UNKNOWN'
hospital_data$admit_location[hospital_data$admit_location == 'TRSF WITHIN THIS FACILITY' | hospital_data$admit_location == 'TRANSFER FROM OTHER HEALT' | hospital_data$admit_location == 'TRANSFER FROM SKILLED NUR' | hospital_data$admit_location == 'TRANSFER FROM HOSP/EXTRAM'] <- 'TRANSFER'  
hospital_data$admit_location[hospital_data$admit_location == 'HMO REFERRAL/SICK'] <- 'PHYS REFERRAL/NORMAL DELI'

#recode religion into fewer classes 
hospital_data$religion[hospital_data$religion == 'UNOBTAINABLE'] <- 'NOT SPECIFIED'
hospital_data$religion[hospital_data$religion != 'CATHOLIC' & hospital_data$religion != 'PROTESTANT QUAKER'& hospital_data$religion != 'JEWISH' & hospital_data$religion != 'NOT SPECIFIED'] <- 'OTHER'


#recode ethnicity into fewer classes -- classes are designed to minimize high-cardinality and create groups most representative of groups of interest in the USA. 
hospital_data$ethnicity[hospital_data$ethnicity == 'ASIAN - CHINESE' | hospital_data$ethnicity == 'ASIAN - ASIAN INDIAN' | hospital_data$ethnicity == 'ASIAN - VIETNAMESE' | hospital_data$ethnicity ==  'ASIAN - FILIPINO' | hospital_data$ethnicity == 'ASIAN - CAMBODIAN' | hospital_data$ethnicity == 'ASIAN - OTHER' | hospital_data$ethnicity == 'ASIAN - KOREAN' | hospital_data$ethnicity == 'ASIAN - JAPANESE' | hospital_data$ethnicity ==  'ASIAN - THAI'] <- 'ASIAN' 
hospital_data$ethnicity[hospital_data$ethnicity == 'WHITE - RUSSIAN'| hospital_data$ethnicity ==  'WHITE - OTHER EUROPEAN' | hospital_data$ethnicity == 'WHITE - BRAZILIAN'| hospital_data$ethnicity == 'WHITE - EASTERN EUROPEAN' | hospital_data$ethnicity == 'PORTUGUESE' ] <- 'WHITE' 
hospital_data$ethnicity[hospital_data$ethnicity == 'HISPANIC/LATINO - PUERTO RICAN' | hospital_data$ethnicity == 'HISPANIC/LATINO - DOMINICAN' | hospital_data$ethnicity == 'HISPANIC/LATINO - GUATEMALAN' | hospital_data$ethnicity == 'HISPANIC/LATINO - CUBAN' | hospital_data$ethnicity ==  'HISPANIC/LATINO - SALVADORAN'| hospital_data$ethnicity ==  'HISPANIC/LATINO - CENTRAL AMERICAN (OTHER)' | hospital_data$ethnicity == 'HISPANIC/LATINO - MEXICAN' | hospital_data$ethnicity == 'HISPANIC/LATINO - COLOMBIAN'| hospital_data$ethnicity == 'HISPANIC/LATINO - HONDURAN'] <- 'HISPANIC OR LATINO'
hospital_data$ethnicity[hospital_data$ethnicity == 'AMERICAN INDIAN/ALASKA NATIVE' | hospital_data$ethnicity == 'AMERICAN INDIAN/ALASKA NATIVE FEDERALLY RECOGNIZED TRIBE' |hospital_data$ethnicity == 'BLACK/AFRICAN' | hospital_data$ethnicity == 'BLACK/CAPE VERDEAN' | hospital_data$ethnicity == 'BLACK/HAITIAN' | hospital_data$ethnicity == 'CARIBBEAN ISLAND' | hospital_data$ethnicity == 'MIDDLE EASTERN' | hospital_data$ethnicity == 'MULTI RACE ETHNICITY' | hospital_data$ethnicity == 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER' | hospital_data$ethnicity == 'SOUTH AMERICAN'] <-'OTHER'
hospital_data$ethnicity[hospital_data$ethnicity == 'UNABLE TO OBTAIN' | hospital_data$ethnicity == 'UNKNOWN/NOT SPECIFIED' | hospital_data$ethnicity == 'PATIENT DECLINED TO ANSWER'] <- 'UNKNOWN'

#recode marital status into fewer classes 
hospital_data$marital_status[hospital_data$marital_status == 'LIFE PARTNER'] <- 'MARRIED'
hospital_data$marital_status[hospital_data$marital_status == 'UNKNOWN (DEFAULT)'] <- 'UNKNOWN'
hospital_data$marital_status[hospital_data$marital_status == 'DIVORCED' | hospital_data$marital_status == 'SEPARATED'] <- 'DIVORCED OR SEPARATED'

#recode admission diagnoses into fewer classes 
#correct encoding errors & group diagnoses by generalized category 
hospital_data$admit_diagnosis[hospital_data$admit_diagnosis == 'CORONARY ARTERY DISEASE\\CORONARY ARTERY BYPASS GRAFT /SDA' | hospital_data$admit_diagnosis ==  'CORONARY ARTERY DISEASE\\CORONARY ARTERY BYPASS GRAFT/SDA' | hospital_data$admit_diagnosis == 'CORONARY ARTERY DISEASE\\CATH'] <- 'CORONARY ARTERY DISEASE'
hospital_data$admit_diagnosis[hospital_data$admit_diagnosis == 'UPPER GI BLEED' | hospital_data$admit_diagnosis == 'LOWER GI BLEED' | hospital_data$admit_diagnosis == 'GI BLEED'  | hospital_data$admit_diagnosis == 'UPPER GASTROINTESTINAL BLEED' | hospital_data$admit_diagnosis == 'LOWER GASTROINTESTINAL BLEED'] <- 'GASTROINTESTINAL BLEED' 
hospital_data$admit_diagnosis[hospital_data$admit_diagnosis == 'ACUTE SUBDURAL HEMATOMA'] <-'SUBDURAL HEMATOMA' 

#take top 30 hospital admission diagnoses and code the rest as OTHER 
hospital_data$admit_diagnosis[hospital_data$admit_diagnosis != 'NEWBORN' & hospital_data$admit_diagnosis != 'PNEUMONIA' & hospital_data$admit_diagnosis != 'SEPSIS' & hospital_data$admit_diagnosis != 'CONGESTIVE HEART FAILURE' & hospital_data$admit_diagnosis != 'CORONARY ARTERY DISEASE' & hospital_data$admit_diagnosis != 'CHEST PAIN' & hospital_data$admit_diagnosis != 'INTRACRANIAL HEMORRHAGE' & hospital_data$admit_diagnosis !='ALTERED MENTAL STATE' & hospital_data$admit_diagnosis != 'GASTROINTESTINAL BLEED' & hospital_data$admit_diagnosis != 'FEVER' & hospital_data$admit_diagnosis != 'S/P FALL' & hospital_data$admit_diagnosis != 'LIVER FAILURE' & hospital_data$admit_diagnosis != 'OVERDOSE'& hospital_data$admit_diagnosis != 'S/P MOTOR VEHICLE ACCIDENT'& hospital_data$admit_diagnosis != 'DIABETIC KETOACIDOSIS' & hospital_data$admit_diagnosis != 'SUBDURAL HEMATOMA'] <- 'OTHER'

View(hospital_data)
attach(hospital_data)

write.csv(hospital_data, 'hospital_data.csv')


##########################################################################################################################################################
############################################################# DATA AND VARIBLE  EXPLORATION ###############################################################
##########################################################################################################################################################


### TARGET VARIABLE - Length of stay in days 
summary(LOS_days)
boxplot(LOS_days)
hist(LOS_days)



###### CATEGORICAL PREDICTORS #######

#Factorizing the Categorical Variables

hospital_data$gender=as.factor(hospital_data$gender)
hospital_data$admit_type=as.factor(hospital_data$admit_type)
hospital_data$admit_location=as.factor(hospital_data$admit_location)
hospital_data$admit_diagnosis=as.factor(hospital_data$admit_diagnosis)
hospital_data$insurance=as.factor(hospital_data$insurance)
hospital_data$religion=as.factor(hospital_data$religion)
hospital_data$marital_status=as.factor(hospital_data$marital_status)
hospital_data$ethnicity=as.factor(hospital_data$ethnicity)
hospital_data$expired_hospital=as.factor(hospital_data$expired_hospital)



#### 1 - GENDER
lm = lm(LOS_days~gender)
summary(lm)

confint(lm,'gender',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))



####2 - ADMIT TYPE
lm = lm(LOS_days~admit_type)
summary(lm)

confint(lm,'admit_type',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))





####3 - ADMIT LOCATIOn
lm = lm(LOS_days~admit_location)
summary(lm)

confint(lm,'admit_location',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))





####4 - ADMIT DIAGNOSIS
lm = lm(LOS_days~admit_diagnosis)
summary(lm)

confint(lm,'admit_diagnosis',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))





####5 - INSURANCE
lm = lm(LOS_days~insurance)
summary(lm)

confint(lm,'insurance',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))





####6 - Religion
lm = lm(LOS_days~religion)
summary(lm)

confint(lm,'religion',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))




####7 - MARITAL STATUS
lm = lm(LOS_days~marital_status)
summary(lm)

confint(lm,'marital_status',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))




####8 - ETHNICITY
lm = lm(LOS_days~ethnicity)
summary(lm)

confint(lm,'ethnicity',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))





####9 - EXPIRED HOSPITAL (Death)
lm = lm(LOS_days~expired_hospital)
summary(lm)

confint(lm,'expired_hospital',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))






################ CONTINUOUS PREDICTORS ##################

### 1 - AGE
hist(age)
summary(age)
boxplot(age)
plot(age,LOS_days)

lm = lm(LOS_days~age)
summary(lm)
confint(lm,'age',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))

#Outliers Test
qqPlot(lm)
outlierTest(lm)


### 2 - NUM CALLOUTS
hist(num_callouts)
summary(num_callouts)
boxplot(num_callouts)
plot(num_callouts,LOS_days)

lm = lm(LOS_days~num_callouts)
summary(lm)
confint(lm,'age',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))

#Outliers Test
qqPlot(lm)
outlierTest(lm)




### 3 - Num Diagnosis
hist(num_diagnosis)
summary(num_diagnosis)
boxplot(num_diagnosis)
plot(num_diagnosis,LOS_days)

lm = lm(LOS_days~num_diagnosis)
summary(lm)
confint(lm,'age',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))

#Outliers Test
qqPlot(lm)
outlierTest(lm)



### 4 - NUM PROCEDURES
hist(num_procedures)
summary(num_procedures)
boxplot(num_procedures)
plot(num_procedures,LOS_days)

lm = lm(LOS_days~num_procedures)
summary(lm)
confint(lm,'age',level=0.95)

#Linearity Test
residualPlots(lm)

#Heteroskedasticity
ncvTest(lm)
coeftest(lm, vcov=vcovHC(lm, type="HC1"))

#Outliers Test
qqPlot(lm)
outlierTest(lm)



### 5 - NUM CPT EVENTS
hist(num_CPT_events)
summary(num_CPT_events)
boxplot(num_CPT_events)

lm = lm(LOS_days~num_CPT_events)
summary(lm)
confint(lm,'num_CPT_events',level=0.95)

#Outliers Test
qqPlot(lm)
outlierTest(lm)



### 6 - NUM INPUT
hist(num_inputs)
summary(num_inputs)
boxplot(num_inputs)

lm = lm(LOS_days~num_inputs)
summary(lm)
confint(lm,'num_inputs',level=0.95)

#Outliers Test
qqPlot(lm)
outlierTest(lm)


### 7 - NUM LABS
hist(num_labs)
summary(num_labs)
boxplot(num_labs)

lm = lm(LOS_days~num_labs)
summary(lm)
confint(lm,'num_labs',level=0.95)

#Outliers Test
qqPlot(lm)
outlierTest(lm)



### 8 - NUM LABS
hist(num_microlabs)
summary(num_microlabs)
boxplot(num_microlabs)

lm = lm(LOS_days~num_microlabs)
summary(lm)
confint(lm,'num_microlabs',level=0.95)

#Outliers Test
qqPlot(lm)
outlierTest(lm)



### 9 - NUM NOTES
hist(num_notes)
summary(num_notes)
boxplot(num_notes)

lm = lm(LOS_days~num_notes)
summary(lm)
confint(lm,'num_notes',level=0.95)

#Outliers Test
qqPlot(lm)
outlierTest(lm)



### 10 - NUM OUTPUTS
hist(num_outputs)
summary(num_outputs)
boxplot(num_outputs)

lm = lm(LOS_days~num_outputs)
summary(lm)
confint(lm,'num_outputs',level=0.95)

#Outliers Test
qqPlot(lm)
outlierTest(lm)


### 11 - NUM DRUGS
hist(num_drugs)
summary(num_drugs)
boxplot(num_drugs)

lm = lm(LOS_days~num_drugs)
summary(lm)
confint(lm,'num_drugs',level=0.95)

#Outliers Test
qqPlot(lm)
outlierTest(lm)


### 12 - NUM PROC EVENTS
hist(num_procedural_events)
summary(num_procedural_events)
boxplot(num_procedural_events)

lm = lm(LOS_days~num_procedural_events)
summary(lm)
confint(lm,'num_procedural_events',level=0.95)

#Outliers Test
qqPlot(lm)
outlierTest(lm)




### 13 - NUM TRANSFERS
hist(num_transfers)
summary(num_transfers)
boxplot(num_transfers)

lm = lm(LOS_days~num_transfers)
summary(lm)
confint(lm,'num_transfers',level=0.95)

#Outliers Test
qqPlot(lm)
outlierTest(lm)




### 14 - NUM CHART EVENTS
hist(num_chart_events)
summary(num_chart_events)
boxplot(num_chart_events)

lm = lm(LOS_days~num_chart_events)
summary(lm)
confint(lm,'num_chart_events',level=0.95)

#Outliers Test
qqPlot(lm)
outlierTest(lm)



### 15 - TOTAL NUM INTERACTIONS
hist(total_num_interactions)
summary(total_num_interactions)
boxplot(total_num_interactions)

lm = lm(LOS_days~total_num_interactions)
summary(lm)
confint(lm,'total_num_interactions',level=0.95)

#Outliers Test
qqPlot(lm)
outlierTest(lm)


######################### CORRELATION #############################
#################################################################

#Correlation matrix
names(hospital_data)

quantvars = hospital_data[, c(2,11,12,13,14,15,16,17,18,19,20,21,22,23,25)]
corr_matrix = cor(quantvars)
corr_plot = corrplot(corr_matrix, tl.col="black", col=brewer.pal(n = 8, name = "YlGnBu"))
round(corr_matrix,2)

install.packages('corrplot')
library(corrplot)

corrplot(corr_matrix)

#VIF test
mreg = lm(LOS_days~age+num_callouts+num_diagnosis+num_procedures+num_CPT_events+num_inputs+num_labs+num_microlabs+num_notes+num_outputs+num_drugs+num_procedural_events+num_transfers+num_chart_events)
#alias(mreg) total number of interaction is considered an alias, it had to be flagged and removed 
vif(mreg)


######################### REMOVING OUTLIERS ######################
#################################################################

hospital_D=hospital_data[-c(3,20,32,34,1138,1690,837,301,65,29583, 31512,34533, 19673, 45458, 16593, 2986, 29432, 42772,  6734, 15871, 35476, 15871, 46920, 19673, 42772, 2986, 53905,28998,19673, 51941, 19673, 46766, 1266, 11440,  1266,  19673, 5581, 45744, 14880, 38305, 30427, 31484, 42560,19673, 20642),]



######################### VISUALIZATION ##########################
################################################################# 
attach(hospital_D)
table(LOS_days) #see the number of observations in each category 
hospital_D$LOS_days=as.factor(hospital_D$LOS_days)


####  BOX PLOTS - with continuous variables ####

#Visualize relationship between age and length of stay at hospital - with proper axis titles and colors (will do later for other graphs we select)
install.packages("RColorBrewer")
library(RColorBrewer)

display.brewer.pal(n = 6, name = 'YlGnBu')
brewer.pal(n = 6, name = "YlGnBu")
#"#FFFFCC" "#C7E9B4" "#7FCDBB" "#41B6C4" "#2C7FB8" "#253494"



## we have a boxplot with age  
age <- ggplot(hospital_D, aes(x = LOS_days, y = age, fill = LOS_days)) + geom_boxplot()+ scale_fill_brewer(palette="YlGnBu")
thetheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15)),
                  axis.text = element_text(family = "Courier",  size = (10)))
plot1 <- age + thetheme + labs(y="age", x = "Hospital duration")
plot1

#or we can do it age and ethnicity
ageboxplot <- ggplot(hospital_D, aes(x = LOS_days, y = age, fill = ethnicity)) + geom_boxplot()+ scale_fill_brewer(palette="YlGnBu")
thetheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15)),
                  axis.text = element_text(family = "Courier",  size = (10)))
plot2 <- ageboxplot + thetheme + labs(y="age", x = "Length of Hospital Stay")
plot2


#total number of interactions
HD10 <- hospital_D %>% 
  filter(total_num_interactions < 5000)

interactions <- ggplot(HD10, aes(x = LOS_days, y = total_num_interactions, fill = LOS_days)) + geom_boxplot()+ scale_fill_brewer(palette="YlGnBu")
thetheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15)),
                  axis.text = element_text(family = "Courier",  size = (10)))
plot3 <- interactions + thetheme + labs(y="Total number of interactions", x = "Length of Hospital Stay")
plot3


#THe following graphs were not used as they do not convey meaningful interpretation 
#Number of procedures
HD1 <- hospital_D %>% 
  filter(num_procedures < 20)

ggplot(HD1, aes(x = LOS_days, y = num_procedures)) + geom_boxplot()


#Number of call outs
HD2 <- hospital_D %>% 
  filter(num_callouts < 2)

ggplot(HD2, aes(x = LOS_days, y = num_callouts)) + geom_boxplot()


#number of diagnosis
HD3 <- hospital_D %>% 
  filter(num_diagnosis < 25)

ggplot(HD3, aes(x = LOS_days, y = num_diagnosis)) + geom_boxplot()


#number of cpt events
HD4 <- hospital_D %>% 
  filter(num_CPT_events < 10)

ggplot(HD4, aes(x = LOS_days, y = num_CPT_events)) + geom_boxplot()


#number of inputs
HD5 <- hospital_D %>% 
  filter(num_inputs < 300)

ggplot(HD5, aes(x = LOS_days, y = num_inputs)) + geom_boxplot()

#number of labs
HD6 <- hospital_D %>% 
  filter(num_labs < 500)

ggplot(HD6, aes(x = LOS_days, y = num_labs)) + geom_boxplot()


#number of outputs
HD7 <- hospital_D %>% 
  filter(num_outputs < 100)

ggplot(HD7, aes(x = LOS_days, y = num_outputs)) + geom_boxplot()


#number of drugs
HD7b <- hospital_D %>% 
  filter(num_drugs < 50)

ggplot(HD7b, aes(x = LOS_days, y = num_drugs)) + geom_boxplot()


#Number of transfers
HD8 <- hospital_D %>% 
  filter(num_transfers < 20)

ggplot(HD8, aes(x = LOS_days, y = num_transfers)) + geom_boxplot()


#Number of chart events
HD9 <- hospital_D %>% 
  filter(num_chart_events < 5000)

ggplot(HD9, aes(x = LOS_days, y = num_chart_events)) + geom_boxplot()





####  HISTOGRAMS ####
#age
agehist <- ggplot(hospital_D, aes(x = age, fill = age)) + geom_histogram(bins=50)+facet_grid(LOS_days)
thetheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15)),
                  axis.text = element_text(family = "Courier",  size = (10)))
print(agehist + thetheme + labs(y="Frequency", x = "age"))


#The following graphs were excluded from the analysis 
hist2 = ggplot(HD7b, aes(x = num_drugs)) + geom_histogram(bins=50)+facet_grid(LOS_days)
hist2

#num diagnosis
hist3 = ggplot(hospital_D, aes(x = num_diagnosis)) + geom_histogram(bins=100)+facet_grid(LOS_days)
hist3

#num procedures
hist4 = ggplot(hospital_D, aes(x = num_procedures)) + geom_histogram(bins=30)+facet_grid(LOS_days)
hist4

#num cpt events
hist5 = ggplot(hospital_D, aes(x = num_CPT_events)) + geom_histogram(bins=30)+facet_grid(LOS_days)
hist5

#num inputs
hist6 = ggplot(hospital_D, aes(x = num_inputs)) + geom_histogram(bins=30)+facet_grid(LOS_days)
hist6

#num labs
hist7 = ggplot(hospital_D, aes(x = num_labs)) + geom_histogram(bins=100)+facet_grid(LOS_days)
hist7

#num microlabs
hist8 = ggplot(hospital_D, aes(x = num_microlabs)) + geom_histogram(bins=100)+facet_grid(LOS_days)
hist8

#num notes
hist9 = ggplot(hospital_D, aes(x = num_notes)) + geom_histogram(bins=10)+facet_grid(LOS_days)
hist9

#num outputs
hist10 = ggplot(hospital_D, aes(x = num_outputs)) + geom_histogram(bins=50)+facet_grid(LOS_days)
hist10

#num procedural events
hist11 = ggplot(hospital_D, aes(x = num_procedural_events)) + geom_histogram(bins=25)+facet_grid(LOS_days)
hist11

#num transfers
hist12 = ggplot(hospital_D, aes(x = num_transfers)) + geom_histogram(bins=50)+facet_grid(LOS_days)
hist12

#num chart events
hist13 = ggplot(hospital_D, aes(x = num_chart_events)) + geom_histogram(bins=50)+facet_grid(LOS_days)
hist13

#total num interact
hist14 = ggplot(hospital_D, aes(x = total_num_interactions)) + geom_histogram(bins=100)+facet_grid(LOS_days)
hist14


### CATEGORICAL VARIABLES - distribution plots 

#Age, even if its not categorical - its sill interesting to see the age distribution of the patients
agebar<- ggplot(data.frame(hospital_D), aes(x=age, fill = insurance)) +geom_bar() 
thetheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15)),
                  axis.text = element_text(family = "Courier", size = (10)))
print(agebar + thetheme + labs(y="frequency", x = "age"))


#Admission type  
admitbar<- ggplot(data.frame(hospital_D), aes(x=admit_type)) +geom_bar(fill = "#7FCDBB")
thetheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15)),
                  axis.text = element_text(family = "Courier", size = (10)))
print(admitbar + thetheme + labs(y="frequency", x = "admission type"))


#Admit location - might not be ideal, harder to read and alot of categories 
location<- ggplot(data.frame(hospital_D), aes(x=admit_location)) +geom_bar(fill = "#7FCDBB")
thetheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15), colour = "#2C7FB8"),
                  axis.text = element_text(angle = 45, hjust=1, family = "Courier", colour = "#253494", size = (7)))
print(location + thetheme + labs(y="frequency", x = "Location"))

#Admit diagnosis 
admit <- hospital_data[hospital_data$admit_diagnosis != 'OTHER',]
location<- ggplot(data.frame(admit), aes(x=admit_diagnosis)) +geom_bar(fill = "#7FCDBB")
thetheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15)),
                  axis.text = element_text(family = "Courier", size = (10)),
                  axis.text.x = element_text(angle = 45, hjust =  1))
print(location + thetheme + labs(y="frequency", x = "Admit Diagnosis"))


#Insurance 
insuranceplot<- ggplot(data.frame(hospital_D), aes(x=insurance)) +geom_bar(fill = "#C7E9B4")
thetheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15)),
                  axis.text = element_text(family = "Courier", size = (10)),
                  axis.text.x = element_text(angle = 45, hjust =  1))
print(insuranceplot + thetheme + labs(y="frequency", x = "insurance"))


#Religion
religion<- ggplot(data.frame(hospital_D), aes(x=religion)) +geom_bar(fill = "#41B6C4")
thetheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15), colour = "#2C7FB8"),
                  axis.text = element_text(angle = 45, hjust=1, family = "Courier", colour = "#253494", size = (10)))
print(religion + thetheme + labs(y="frequency", x = "religion"))



#Marital status
maritalStatus<- ggplot(data.frame(hospital_D), aes(x=marital_status)) +geom_bar(fill = "#E7B800")
thetheme <- theme(plot.title = element_text(family = "URWGothic", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15), colour = "#E69F00"),
                  axis.text = element_text(family = "Courier", colour = "#D55E00", size = (10)))
print(maritalStatus + thetheme + labs(title= "Graph 8: Distribution of marital status of the patients",
                                      y="frequency", x = "marital status"))


#Ethnicity
eth<- ggplot(data.frame(hospital_D), aes(x=ethnicity)) +geom_bar(fill = "#FFFFCC")
thetheme <- theme(plot.title = element_text(family = "URWGothic", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15)),
                  axis.text = element_text(family = "Courier", size = (10)),
                  axis.text.x = element_text(angle = 45, hjust =  1))
print(eth + thetheme + labs(y="frequency", x = "ethnicity"))


#Expired :-(
death<- ggplot(data.frame(hospital_D), aes(x=expired_hospital)) +geom_bar(fill = "midnightblue")
thetheme <- theme(plot.title = element_text(family = "URWGothic", face = "bold", size = (15)), 
                  axis.title = element_text(family = "Helvetica", size = (15)),
                  axis.text = element_text(family = "Courier", size = (10)))
print(death + thetheme + labs(y="frequency", x = "Survival and Death"))










