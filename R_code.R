#set working directory
path <- "/home/arpit/Desktop/Valiance Solution"
setwd(path)

#load library
library(data.table)
library(stringr)
library(ggplot2)
library(mlr)
library(grid)
library(caret)
library(rpart)
library(kernlab)
library(rpart.plot)

#load packages & data
direct_mail<-fread("/home/arpit/Desktop/Shared folder/dm.csv",na.strings = c(""," ","?","NA",NA))
zip <- fread("/home/arpit/Desktop/Shared folder/zippopulation.csv",na.strings = c(""," ","?","NA",NA))

dim(direct_mail); str (direct_mail); View(direct_mail)
dim(zip); str (zip); View(zip)

#adding zero to zip codes
zip$ZIP <- as.character(zip$ZIP)
zip <- zip[,ZIP:=ifelse(nchar(zip$ZIP)==4, paste0("0", zip$ZIP),ZIP)]
View(zip)

direct_mail$zip <- as.character(direct_mail$zip)
direct_mail <- direct_mail[,zip:=ifelse(nchar(direct_mail$zip)==4, paste0("0", direct_mail$zip),zip)]
View(direct_mail)

names(direct_mail)[2] <- "ZIP"

#merging dm and zip 
train <- merge(direct_mail,zip,all.x = TRUE, by = "ZIP")
View(train)
str(train); dim(train)

# taking 4 digits of sic_code
train <- train[,sic_code:= substr(sic_code,1,4)]
train$sic_code <- as.integer(train$sic_code)

#check target variables
unique(train$Outcome)

(prop.table(table(train$Outcome))*100) # this shows data is imbalanced one , since most of the outcomes are NoResp

#percent of NA
mvtr <- (sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100)


#set column classes
factcols <- c(1:5,8:16,19:20,23:34,37,41:45)
numcols <- setdiff(1:52,factcols)

train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

#subset categorical and numerical variables
cat_train <- train[,factcols, with=FALSE]
num_train <- train[,numcols,with=FALSE]


#adding class variable to numerical features
num_train[,Outcome := cat_train$Outcome]
#taking subset of numerical variables where outcome != "noresp"
num_train1 <- num_train[Outcome!="NoResp"]


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#writing a plot function
tr <- function(a){
  ggplot(data = num_train, aes(x= a, y =..density..))+geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100)+geom_density() 
}

##INFO EMPLOYESS VARIABLE
tr(num_train$info_employees)
#rightly skewed graph, so taking a log transformation 
tr(log(num_train$info_employees))

#Q1. IS lat and long of an area is affecting the outcome
#create a scatter plot
#LAT AND LAG EFFECT ON THE OUTCOME
ggplot(data=num_train,aes(x = lat, y= lng))+geom_point(aes(colour=num_train$Outcome))

ggplot(data=num_train1,aes(x = lat, y= lng))+geom_point(aes(colour=num_train1$Outcome))
 
#INFO LONG AND INFO LAT EFFECT ON OUTCOME 
ggplot(data=num_train,aes(x = info_longitude, y = info_latitude))+geom_point(aes(colour= Outcome)) + facet_wrap(~Outcome)

ggplot(data=num_train1,aes(x = info_longitude, y = info_latitude))+geom_point(aes(colour= Outcome)) + facet_wrap(~Outcome)

#INFO SALES AND INFO EMPLOYEES VARIABLE
require(scales)
ggplot(data=num_train,aes(x = info_sales, y = info_employees))+geom_point(aes(colour= Outcome))+facet_wrap( ~ Outcome)+ scale_x_continuous(labels = comma)
#this shows low sales and low number of employess generate response
ggplot(data=num_train1,aes(x = info_sales, y = info_employees))+geom_point(aes(colour= Outcome))+facet_wrap( ~ Outcome)+ scale_x_continuous(labels = comma)

#INFO SALES VARIABLE BOXPLOT 
ggplot(data=num_train,aes(x=Outcome, y= info_sales)) + geom_boxplot(aes(colour=Outcome), outlier.colour = "red")

ggplot(data=num_train1,aes(x=Outcome, y= info_sales)) + geom_boxplot(aes(colour=Outcome))

# GROUP EMPOYEES BY NO. AND CHECKING WITH OUTCOME
num_train$employee_grp <- cut(num_train$info_employees, breaks = seq(0,2510,31))
table(num_train$employee_grp, num_train$Outcome)


ggplot(data=num_train,aes(x = dnb_Longitude, y = dnb_Latitude))+geom_point(aes(colour= Outcome))+ facet_wrap( ~ Outcome)
#dnb_lat and dnb_long are not able to show the real behaviour

tr(num_train$dnb_facility_size)
tr(log(num_train$dnb_facility_size)) #normalized using log 



#lets see whether small facilitiy size and  less sales affect the outcome 
ggplot(data=num_train,aes(x = dnb_sales, y = dnb_facility_size))+geom_point(aes(colour= Outcome))+facet_wrap( ~ Outcome)+ scale_x_continuous(labels = comma)


# NB FACILITY AND DNB SALES VARIABLE( LOTS OF MISSING VALUES , NOT GOOD VARIABLES)
ggplot(data=num_train1,aes(x = dnb_sales, y = dnb_facility_size))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

#POPULATION and CAT Variable
g1<-ggplot(data=num_train,aes(x = Cat1, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

g2<-ggplot(data=num_train,aes(x = Cat2, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

g3<-ggplot(data=num_train,aes(x = Cat3, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

g4<-ggplot(data=num_train,aes(x = Cat4, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

g5<-ggplot(data=num_train,aes(x = Cat5, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

g6<-ggplot(data=num_train,aes(x = Cat6, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

multiplot(g1,g2,g3,g4,g5,g6,cols = 3)

p1<-ggplot(data=num_train1,aes(x = Cat1, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

p2<-ggplot(data=num_train1,aes(x = Cat2, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

p3<-ggplot(data=num_train1,aes(x = Cat3, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

p4<-ggplot(data=num_train1,aes(x = Cat4, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

p5<-ggplot(data=num_train1,aes(x = Cat5, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

p6<-ggplot(data=num_train1,aes(x = Cat6, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

multiplot(p1,p2,p3,p4,p5,p6,cols = 3)

# CHECKING FOR A ZIP CODE WHETHER IT IS USEFUL OR NOT 
ZIP_train <- as.data.frame.matrix(table(cat_train$ZIP,cat_train$Outcome))
ZIP_Won <- ZIP_train[ZIP_train$Won>0,]

cat_all <- cat_train[cat_train$Outcome != "NoResp"]
cat_won <- cat_train[cat_train$Outcome == "Won"]
cat_lost <- cat_train[cat_train$Outcome == "Lost"]
cat_DNQ <- cat_train[cat_train$Outcome == "DNQ"]
cat_NoResp<- cat_train[cat_train$Outcome=="NoResp"]

all_bar <- function(c,i){
  ggplot(c,aes(x=i,fill=c$Outcome))+geom_bar()+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}

# initial how, outcomes are lined up
h1<-all_bar(cat_all,cat_all$Outcome)

h2<-all_bar(cat_won,cat_won$state) 
h3<-all_bar(cat_lost,cat_lost$state)
h4<-all_bar(cat_DNQ,cat_DNQ$state)
h5<-all_bar(cat_all,cat_all$state)

multiplot(h2,h3,h4,h5,cols=2)

all_dodge <- function(c,i){
  ggplot(c,aes(x=i,fill=c$Outcome))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}

all_dodge(cat_all,cat_all$naics_code)
#since lot of values are NA , so not useful 

all_dodge(cat_all,cat_all$sic_code)
# few sic_codes  can be used to predict outcome

all_dodge(cat_all,cat_all$empolyees)
#1 to 10 there has been mixed respons , but 10 above there are hardly companies
#which are giving response
all_dodge(cat_NoResp,cat_NoResp$empolyees)

j1<- all_dodge(cat_all,cat_all$provider)
# DM16 info USA is the best provider and D&B can also be used

j2 <- all_dodge(cat_NoResp,cat_NoResp$provider)

multiplot(j1,j2, cols=2)
#info data provider should be used


all_dodge(cat_all,cat_all$info_first_name)
#since half of the first names and half of the names result in true, so there might be some other factor which also results in decision

all_dodge(cat_all,cat_all$info_middle_name)

all_dodge(cat_all,cat_all$info_last_name)
#first name and last name are there for each so they have the same thing

#INFO_TITLE (OWERS ARE IMPORTANT)
n1<- all_dodge(cat_all,cat_all$info_title)
n2<- all_dodge(cat_NoResp,cat_NoResp$info_title)

multiplot(n1,n2,cols=2)

#INFO STATE
z1<-all_dodge(cat_all,cat_all$info_state)
z2<-all_bar(cat_won,cat_won$info_state) 
z3<-all_bar(cat_lost,cat_lost$info_state)
z4<-all_bar(cat_DNQ,cat_DNQ$info_state)

multiplot(z1,z2,z3,z4,cols=2)

#INFO PLUS4
all_dodge(cat_all,cat_all$info_plus4)
#lots of NA

#INFO URL
all_dodge(cat_all,cat_all$info_url)
#not much of help as in single parameter

table(is.na(cat_train$sic_code))

#INFO NAICS SOMEWHAT USEFUL
all_dodge(cat_all,cat_all$info_naics)

#INFO SIC not required since SIC had less NA values and was better

#INFO_ESTABLISHED
all_dodge(cat_all,cat_all$info_established)
# mostly business established after 2000 won and before that lost 

#INFO_YEARS
all_dodge(cat_all,cat_all$info_years)
# no lost between 3 to 5 ( so in order to decrease lost )

mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100

all_dodge(cat_all,cat_all$dnb_state)

# dnb has high number of NA so not useful in analytics 

mvnr <- sapply(num_train, function(x){sum(is.na(x))/length(x)})*100


####################################################################
#Q Important Parameters that I am left with are : 
#1. state
#2. sic_code
#3. empolyees/ info_employees
#4. info_first_name
#5. info_title
#6. info_naics
#7. info_established
#8. info_years
#9. info_sales
#10.Population
#11. Cat1 to Cat6
#Q1. What Sales range and number of employees  should be targeted ?'''

require(scales)
ggplot(data=num_train,aes(x = info_sales, y = info_employees))+geom_point(aes(colour= Outcome))+facet_wrap( ~ Outcome)+ scale_x_continuous(labels = comma)
#this shows low sales and low number of employess generate response
ggplot(data=num_train1,aes(x = info_sales, y = info_employees))+geom_point(aes(colour= Outcome))+facet_wrap( ~ Outcome)+ scale_x_continuous(labels = comma)

#Q2. What sales should be targeted for each outcome ? 

ggplot(data=num_train1,aes(x=Outcome, y= info_sales)) + geom_boxplot(aes(colour=Outcome))+scale_y_continuous( labels = comma, breaks = seq(0,3500000,500000))+scale_y_continuous(breaks = seq(0,3000000, 250000))+labs(y="Sales")

# to reduce DNQ ,Lost and increase Won, sales should be above 5,00,000 and and less than 12,50,000

#Q3. How many employess should be targeted ? 
ggplot(data=num_train1,aes(x=Outcome, y= info_employees)) + geom_boxplot(aes(colour=Outcome))+scale_y_continuous(name="Employees", labels = comma, breaks = seq(0,40,2))+labs(y="Employees")

#number of employee should be between 5 to 10 to increase won and decrease DNQ and Lost '''

#Q4. Which population to be targeted in every ZIP ?

library(grid)
ggplot(data=num_train1,aes(x=Outcome, y= Population)) + geom_boxplot(aes(colour=Outcome))+scale_y_continuous(name="Population" ,labels = comma, breaks = seq(0,100000,10000))


#Q5. Which income category to target for all ZIP  ?
p1<-ggplot(data=num_train1,aes(x = Cat1, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

p2<-ggplot(data=num_train1,aes(x = Cat2, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

p3<-ggplot(data=num_train1,aes(x = Cat3, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

p4<-ggplot(data=num_train1,aes(x = Cat4, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

p5<-ggplot(data=num_train1,aes(x = Cat5, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

p6<-ggplot(data=num_train1,aes(x = Cat6, y = Population))+geom_point(aes(colour= Outcome))+ scale_x_continuous(labels = comma)

multiplot(p1,p2,p3,p4,p5,p6,cols = 3)

# Cat6 people to reach out for since no. of people in this bracket are less and we will reach out to more people so chances of 
# It will create more hits 

#Q6. Does Title has any effect on the outcome ?
n1<- all_dodge(cat_all,cat_all$info_title)
n2<- all_dodge(cat_NoResp,cat_NoResp$info_title)+ scale_y_continuous(breaks = seq(0,60000,5000))

multiplot(n1,n2,cols=2)
# president and manager should be avoided to decrease lost , Owners should be targetd to increase Won

#Q7. States which performs better ?
d0<- all_dodge(cat_all,cat_all$info_state)+scale_x_discrete(name="State")+ labs(title = "ALL STATES" )

# SC is a state which should be targeted more to increase Won 
# NC and VA are started which has no DNQ 

#Q8. Which particular ZIP to target within a state ?
cat_DE <- cat_all[state=="DE",]
cat_GA <- cat_all[state=="GA",]
cat_MD <- cat_all[state=="MD",]
cat_NC <- cat_all[state=="NC",]
cat_NJ <- cat_all[state=="NJ",]
cat_NY <- cat_all[state=="NY",]
cat_OH <- cat_all[state=="OH",]
cat_PA <- cat_all[state=="PA",]
cat_SC <- cat_all[state=="SC",]
cat_VA <- cat_all[state=="VA",]

d1<- all_dodge(cat_GA,cat_GA$ZIP) + labs(title = "State GA")+labs(fill = "Outcome")+labs(x= "ZIP")
d2<- all_dodge(cat_MD,cat_MD$ZIP) + labs(title = "State MD")+labs(fill = "Outcome")+labs(x= "ZIP")
d3<- all_dodge(cat_NC,cat_NC$ZIP) + labs(title = "State NC")+labs(fill = "Outcome")+labs(x= "ZIP")
d3<- all_dodge(cat_NC,cat_NC$ZIP) + labs(title = "State NC")+labs(fill = "Outcome")+labs(x= "ZIP")
d4<- all_dodge(cat_NJ,cat_NJ$ZIP) + labs(title = "State NJ")+labs(fill = "Outcome")+labs(x= "ZIP")
d5<- all_dodge(cat_NY,cat_NY$ZIP) + labs(title = "State NY")+labs(fill = "Outcome")+labs(x= "ZIP")
d6<- all_dodge(cat_OH,cat_OH$ZIP) + labs(title = "State OH")+labs(fill = "Outcome")+labs(x= "ZIP")
d7<- all_dodge(cat_PA,cat_PA$ZIP) + labs(title = "State PA")+labs(fill = "Outcome")+labs(x= "ZIP")
d8<- all_dodge(cat_SC,cat_SC$ZIP) + labs(title = "State SC")+labs(fill = "Outcome")+labs(x= "ZIP")
d9<- all_dodge(cat_VA,cat_VA$ZIP) + labs(title = "State VA")+labs(fill = "Outcome")+labs(x= "ZIP")
d10<-all_dodge(cat_DE,cat_DE$ZIP) + labs(title = "State DE")+labs(fill = "Outcome")+labs(x= "ZIP")
multiplot(d0,d1,d2,d3,d4,d5,cols=3)
multiplot(d0,d6,d7,d8,d9,d10,cols=3)

#Particular ZIP could be targeted in particular state for outcome to be "Won","Lost","DNQ"

#Q9.Maybe younger companies are reacting better than older ones?
f1<- all_dodge(cat_all,cat_all$info_years) + labs(title="No. of Years ", x = "Number of Years")
# 10 years or more give more positive response 

f2<-all_dodge(cat_all,cat_all$info_established)+scale_y_continuous(breaks = seq(0,16,2))+ labs(x="Year",fill="Outcome")


# here companies mostly between 2000 and 2015 have given positive response

multiplot(f1,f2,cols=1)

#Q10. Any particular SIC code which may result in increase in won?
all_dodge(cat_all,cat_all$sic_code)+ labs ( fill = "Outcome", x = "SIC CODE")+scale_y_continuous(breaks = seq(0,12,2)) 
# to reduce lost 5411,5651,5941 should not be targeted
# to decrease DNQ 7692 should not be targeted

#Q11. Which info_ Naics code should be targeted ? 
all_dodge(cat_all,cat_all$info_naics)+ labs(x="INFO_NAICS", fill="Outcome")+labs(x="NAICS CODE")
# to reduce lost 445110,445292,448310,81198 should be avoided
# to reduce DNQ 811118,811310 should be avoided
#to increase won more 8122320,812113, 722513 and 441310 should be targeted

#Q12. Does 1st nname has any effect on the outcome ?
# nope
table(name_first$Outcome,name_first$name)

#Q13. Which is  the most important variable for outcome?

train1<- train[Outcome !="NoResp"]
train2<- as.data.frame(train1)
k<- c(1:2,4,12,19,21:24,45:52)
train2 <- train2[,k]
#create task
train2.task <- makeClassifTask(data =train2,target = "Outcome")


#remove zero variance features
train.task <- removeConstantFeatures(train.task)

#get variable importance char
var_imp <- generateFilterValuesData(train2.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE) + labs( x= "FEATURES", title = " Feature Importance")

m <- c(1:2,4,12,19,21:24,45:46)
train3 <- train1[,m]
#create task
train3.task <- makeClassifTask(data =train3,target = "Outcome")


#remove zero variance features
train.task <- removeConstantFeatures(train.task)

#get variable importance char
var_imp1 <- generateFilterValuesData(train3.task, method = c("information.gain"))
plotFilterValues(var_imp1,feat.type.cols = TRUE) + labs( x= "FEATURES", title = " Feature Importance")


n<- c(1:2,4,12,19,21:24,45)
train4 <- train1[,n]
#create task
train4.task <- makeClassifTask(data =train4,target = "Outcome")


#remove zero variance features
train.task <- removeConstantFeatures(train.task)

#get variable importance char
var_imp2 <- generateFilterValuesData(train4.task, method = c("information.gain"))
plotFilterValues(var_imp2,feat.type.cols = TRUE) + labs( x= "FEATURES", title = " Feature Importance")

#Q14. What multiple parameters(segments) to target in state,sales, population for true, lost and DNQ ?


train_para <- train[train$Outcome!="NoResp"]
train_para$Outcome<- factor(train_para$Outcome)

#train the model

mo <- rpart(Outcome ~ state+info_sales+Population, data=train_para, method="class")

#Plot the decision tree
rpart.plot(mo,type=3,extra=101,fallen.leaves=T)

summary(mo)
print(m)
plotcp(m)

