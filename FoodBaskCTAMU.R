
# Load in library
library(rpart)
library(rpart.plot)
library(readr)
library(dplyr)
library(foreign)
library(ggplot2)
library(class)
library(useful)
library(tidyverse)
library(caret)
library(gbm)
library(ISLR)
library(skimr)
library(party)
library(tree)
library(DAAG)
library(mlbench)
library(pROC)
library(rattle)

options(scipen = 999)

FB1 <- read_csv("FB1.csv")
fb1<- FB1[-1]

#eliminating the following rows because they have no values at all
fb1 %>% select(-c(`Visited Agency`, `Visited Agency (Short Name)`, 
                  `Client Race Category`, `Client Student Loans`,
                  `Client Scholarships`, `Client No Income`, 
                  `Client Status`))-> fb1

#these data set is assigned because it contains only dates
fb1 %>% select(c(`Client First Visit-Date`, `Client First Visit- Personal Tab`, 
                  `Client CSFP Initial Certification Date`, 
                  `Client CSFP Next Recertification Date`,
                  `Client CSFP Next Check-In Date`, `Recorded At`,
                 `Client ID`, `Household ID`, `Client CSFP ID`,
                 `HH Mem 1- ID`,`HH Mem 2- ID`,`HH Mem 3- ID`,))-> Datfb1

fb1 %>% select(-c(`Client First Visit-Date`, `Client First Visit- Personal Tab`, 
                 `Client CSFP Initial Certification Date`, 
                 `Client CSFP Next Recertification Date`,
                 `Client CSFP Next Check-In Date`, `Recorded At`,
                 `Client ID`, `Household ID`, `Client CSFP ID`,
                 `HH Mem 1- ID`,`HH Mem 2- ID`,`HH Mem 3- ID`,
                 ))-> fb1
#fb1$`Client First Visit- Personal Tab`<- as.Date(fb1$`Client First Visit- Personal Tab`,
# format = "%d-%m-%Y")
###################################################################################

fb1$`Client ID`<- as.factor(fb1$`Client ID`)
fb1$`Client Preferred Agency` <- with(fb1, 
                                      ifelse(grepl("Interfaith Food Bank",
                                                   `Client Preferred Agency`), 1, 0))
fb1$`Client Age`<- as.factor(fb1$`Client Age`)
fb1$`Client Gender Identity-Labels` <- factor(fb1$`Client Gender Identity-Labels` ,
                                              levels = c("Didn't Ask", "Female", "Male","Transgender"),
                                              labels = c(0,1,2,3))

fb1$`Client Gender Identity-Parent Types` <- factor(fb1$`Client Gender Identity-Parent Types` ,
                                                    levels = c("Didn't Ask", "Female", "Male","Transgender"),
                                                    labels = c(0,1,2,3))

fb1$`Client Marital Status` <- factor(fb1$`Client Marital Status`,
                                      levels = c("0", "common_law", "declined_to_answer","did_not_ask",
                                                 "divorced", "do_not_know", "married", "separated",
                                                 "single", "widowed"),
                                      labels = c(0,1,2,3,4,5,6,7,8,9))

fb1$`Client Ethnicity-Labels` <- with(fb1, 
                                      ifelse(grepl("Hispanic / Latino",
                                                   `Client Ethnicity-Labels`), 1,0))

fb1$`Client Ethnicity-Parent Types`<- with(fb1, 
                                           ifelse(grepl("Hispanic / Latino",
                                                        `Client Ethnicity-Parent Types`), 1,0))

fb1$`Client Disability`<- factor(fb1$`Client Disability`, 
                                 levels = c("0", "did_not_ask", "no", "yes"),
                                 labels = c(0,1,2,3))

fb1$`Client Self-Identifies As`<- with(fb1, 
                                       ifelse(grepl("mental_illness",
                                                    `Client Self-Identifies As`), 1,0))

fb1$`Client Is Student`<- as.factor(fb1$`Client Is Student`)

fb1$`Client Employment`<- with(fb1, 
                               ifelse(grepl("retired",
                                            `Client Employment`), 1,0))

fb1$`Client Disability`<- factor(fb1$`Client Disability`, 
                                 levels = c("0", "Angleton Jr. High School", 
                                            "Not Attending School", " Other", "Out of Age Range"),
                                 labels = c(0,1,2,3,4))

fb1$`Highest Education Level`<- factor(fb1$`Highest Education Level`, 
                                       levels = c("0", "associate_degree", 
                                                  "bachelors_degree", "declined_to_answer", "did_not_ask",
                                                  "do_not_know","ged", "grade_0_8", "grade_12", "grade_9_11",
                                                  "masters_degree", "phd", "post_secondary_some", 
                                                  "trade_cert_professional"),
                                       labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13))

fb1$`Country of Highest Education`<- as.factor(fb1$`Country of Highest Education`)
fb1$`Country of Highest Education` <- with(fb1, 
                                           ifelse(grepl("United States",
                                                        `Country of Highest Education`), 1,0))

fb1$`Client Full Time Employment`<- as.numeric(fb1$`Client Full Time Employment`)
fb1$`Client Part Time Employment`<- as.numeric(fb1$`Client Part Time Employment`)
fb1$`Client Other`<- as.numeric(fb1$`Client Other`)
fb1$`Client Private Disability`<- as.numeric(fb1$`Client Private Disability`)
fb1$`Client Private Pension`<- as.numeric(fb1$`Client Private Pension`)
fb1$`Client Social Assistance`<- as.numeric(fb1$`Client Social Assistance`)
fb1$`Client Social Security Disability Insurance (SSDI)`<- as.numeric(fb1$`Client Social Security Disability Insurance (SSDI)`)
fb1$`Client Spouse/Family Support`<- as.numeric(fb1$`Client Spouse/Family Support`)
fb1$`Client SSA`<-as.numeric(fb1$`Client SSA`)  
fb1$`Client Supplemental Security Income (SSI)`<- as.numeric(fb1$`Client Supplemental Security Income (SSI)`)
fb1$`Client Undisclosed`<- as.numeric(fb1$`Client Undisclosed`)
fb1$`Zip Code`<- with(fb1, ifelse(grepl("77531", `Zip Code`), 1,0))
#fb1$`Household ID`<-as.factor(fb1$`Household ID`)
fb1$`Household Size`<- as.factor(fb1$`Household Size`)
#fb1$`Client CSFP ID` <- as.factor(fb1$`Client CSFP ID`)

fb1$`Client CSFP Status` <- as.factor(fb1$`Client CSFP Status`)
fb1$`Client CSFP Status`<- with(fb1, ifelse(grepl("inactive", 
                                                  `Client CSFP Status`), 1,0))

fb1$`Client CSFP Location`<- with(fb1, ifelse(grepl("Brazoria County Dream Center", 
                                                    `Client CSFP Location`), 1,0))

fb1$`Client Ethnicity Category`<- with(fb1, ifelse(grepl("99 Other", 
                                                         `Client Ethnicity Category`), 1,0))
fb1$City <- as.factor(fb1$City)
fb1$County <- as.factor(fb1$County)
fb1$`Housing Type`<- with(fb1, ifelse(grepl("Own Home",`Housing Type`), 1,0))
fb1$`Household Languages`<- with(fb1, ifelse(grepl("English",`Household Languages`), 1,0))

fb1$`Household Primary Income Source`<- with(fb1, 
                                             ifelse(grepl("Full Time Employment",
                                                          `Household Primary Income Source`), 1,0))

fb1$`Total Monthly Expenses`  <- as.numeric(fb1$`Total Monthly Expenses`)
fb1$`Total Monthly Net Income` <- as.numeric(fb1$`Total Monthly Net Income`)
fb1$`Household Expense (Childcare)`<- as.numeric(fb1$`Household Expense (Childcare)`)
fb1$`Household Expense (Food)`<- as.numeric(fb1$`Household Expense (Food)`)
fb1$`Household Expense (Medical)`<- as.numeric(fb1$`Household Expense (Medical)`)
fb1$`Household Expense (Mortgage)`<- as.numeric(fb1$`Household Expense (Mortgage)`)
fb1$`Household Expense (Other)`<- as.numeric(fb1$`Household Expense (Other)`)
fb1$`Household Expense (Rent)`<- as.numeric(fb1$`Household Expense (Rent)`)
fb1$`Household Expense (School Expenses)`<- as.numeric(fb1$`Household Expense (School Expenses)`)
fb1$`Household Expense (Transit)`<- as.numeric(fb1$`Household Expense (Transit)`)
fb1$`Household Expense (Utilities)`<- as.numeric(fb1$`Household Expense (Utilities)`)
fb1$`Household Expense (Vehicle)`<- as.numeric(fb1$`Household Expense (Vehicle)`)
fb1$`Dietary Considerations` <- as.factor(fb1$`Dietary Considerations`)
fb1$`Social Assistance`<- with(fb1, ifelse(grepl("Medicaid", 
                                                 `Social Assistance`), 1,0))

fb1$`Program Name`<- with(fb1, ifelse(grepl("TEFAP Pantry Visit", 
                                            `Program Name`), 1,0))

fb1$`Program Type`<- with(fb1, ifelse(grepl("tefap_food_bank", 
                                            `Program Type`), 1,0))

#fb1$`HH Mem 1- ID`<- as.factor(fb1$`HH Mem 1- ID`)

fb1$`HH Mem 1- Status`<- with(fb1, ifelse(grepl("active", 
                                                `HH Mem 1- Status`), 1,0))

fb1$`HH Mem 1- Age` <- as.factor(fb1$`HH Mem 1- Age`)

fb1$`HH Mem 1- Gender Identity-Labels`<- factor(fb1$`HH Mem 1- Gender Identity-Labels`, 
                                                levels = c("0", "Didn't Ask", "Female",
                                                           "Male"), labels = c(0,1,2,3))

fb1$`HH Mem 1- Gender Identity-Parent Types` <- factor(fb1$`HH Mem 1- Gender Identity-Parent Types` ,
                                                       levels = c("Didn't Ask", "Female", "Male","Transgender"),
                                                       labels = c(0,1,2,3))

fb1$`HH Mem 1- Ethnicity-Labels` <- with(fb1, ifelse(grepl("Hispanic / Latino", 
                                                           `HH Mem 1- Ethnicity-Labels`), 1,0))

fb1$`HH Mem 1- Ethnicity-Parent Types`<- with(fb1, ifelse(grepl("Hispanic / Latino", 
                                                                `HH Mem 1- Ethnicity-Parent Types`), 1,0))

fb1$`HH Mem 1- Disability`<- with(fb1, ifelse(grepl("no", 
                                                    `HH Mem 1- Disability`), 1,0))

fb1$`HH Mem 1- Self-Identifies As`<- with(fb1, ifelse(grepl("none", 
                                                            `HH Mem 1- Self-Identifies As`), 1,0))

fb1$`HH Mem 1- Is Student` <- with(fb1, ifelse(grepl("No", 
                                                     `HH Mem 1- Is Student` ), 1,0))


fb1$`HH Mem 1- Relationship to Main Client`<- with(fb1, ifelse(grepl("spouse", 
                                                                     `HH Mem 1- Relationship to Main Client`), 1,0))

#fb1$`HH Mem 2- ID`<- as.factor(fb1$`HH Mem 2- ID`)

fb1$`HH Mem 2- Status`<- with(fb1, ifelse(grepl("active", 
                                                `HH Mem 2- Status`), 1,0))

fb1$`HH Mem 2- Age`<- as.factor(fb1$`HH Mem 2- Age`)

fb1$`HH Mem 2- Gender Identity-Labels`<- factor(fb1$`HH Mem 2- Gender Identity-Labels`, 
                                                levels = c("0", "Didn't Ask", "Female",
                                                           "Male", "Transgender"), labels = c(0,1,2,3,4))

fb1$`HH Mem 2- Gender Identity-Parent Types` <- factor(fb1$`HH Mem 2- Gender Identity-Parent Types` ,
                                                       levels = c("0","Didn't Ask", "Female", "Male","Transgender"),
                                                       labels = c(0,1,2,3,4))

fb1$`HH Mem 2- Ethnicity-Labels`<- with(fb1, ifelse(grepl("Hispanic / Latino", 
                                                          `HH Mem 2- Ethnicity-Labels`), 1,0))

fb1$`HH Mem 2- Ethnicity-Parent Types`<- with(fb1, ifelse(grepl("Hispanic / Latino", 
                                                                `HH Mem 2- Ethnicity-Parent Types`), 1,0))

fb1$`HH Mem 2- Disability`<- with(fb1, ifelse(grepl("no", 
                                                    `HH Mem 2- Disability`), 1,0))

fb1$`HH Mem 2- Self-Identifies As`<- with(fb1, ifelse(grepl("none", 
                                                            `HH Mem 2- Self-Identifies As`), 1,0))

fb1$`HH Mem 2- Is Student` <- with(fb1, ifelse(grepl("No", 
                                                     `HH Mem 2- Is Student` ), 1,0))

fb1$`HH Mem 2- Relationship to Main Client`<- with(fb1, ifelse(grepl("child", 
                                                                     `HH Mem 2- Relationship to Main Client`), 1,0))

#fb1$`HH Mem 3- ID` <- as.factor(fb1$`HH Mem 3- ID`)


fb1$`HH Mem 3- Status`<- with(fb1, ifelse(grepl("active", 
                                                `HH Mem 3- Status`), 1,0))

fb1$`HH Mem 3- Age`<- as.factor(fb1$`HH Mem 3- Age`)


fb1$`HH Mem 3- Gender Identity-Labels`<- factor(fb1$`HH Mem 3- Gender Identity-Labels`, 
                                                levels = c("0", "Didn't Ask", "Female",
                                                           "Male"), 
                                                labels = c(0,1,2,3))

fb1$`HH Mem 3- Gender Identity-Parent Types` <- factor(fb1$`HH Mem 3- Gender Identity-Parent Types` ,
                                                       levels = c("0","Didn't Ask", "Female", "Male"),
                                                       labels = c(0,1,2,3))

fb1$`HH Mem 3- Ethnicity-Labels`<- with(fb1, ifelse(grepl("Hispanic / Latino", 
                                                          `HH Mem 3- Ethnicity-Labels`), 1,0))

fb1$`HH Mem 3- Ethnicity-Parent Types`<- with(fb1, ifelse(grepl("Hispanic / Latino", 
                                                       `HH Mem 3- Ethnicity-Parent Types`), 1,0))

fb1$`HH Mem 3- Self-Identifies As`<- with(fb1, ifelse(grepl("none", 
                                                          `HH Mem 3- Self-Identifies As`), 1,0))

fb1$`HH Mem 3- Is Student` <- with(fb1, ifelse(grepl("No", 
                                                     `HH Mem 3- Is Student` ), 1,0))

fb1$`HH Mem 3- Relationship to Main Client`<- with(fb1, ifelse(grepl("child", 
                                              `HH Mem 3- Relationship to Main Client`), 1,0))

#######################################################################
fb1<- fb1 %>% rename(ClntID =`Client ID`)
fb1<- fb1 %>% rename(ClntAge =`Client Age`)
fb1<- fb1 %>% rename(CltGdrILbs = `Client Gender Identity-Labels`)
fb1<- fb1 %>% rename(CltGdrIdPT = `Client Gender Identity-Parent Types`)
fb1<- fb1 %>% rename(CltMarStus = `Client Marital Status`)
fb1<- fb1 %>% rename(CltEthLab = `Client Ethnicity-Labels`)
fb1<- fb1 %>% rename(CltEthParT = `Client Ethnicity-Parent Types`)
fb1<- fb1 %>% rename(CltDisabty = `Client Disability`)
fb1<- fb1 %>% rename(CltSlfIdAs = `Client Self-Identifies As`)
fb1<- fb1 %>% rename(CltIsStud = `Client Is Student`)
fb1<- fb1 %>% rename(CltEmplnt = `Client Employment`)
fb1<- fb1 %>% rename(CltSchAtd = `Client School Attended`)
fb1<- fb1 %>% rename(HighEduLev = `Highest Education Level`)
fb1<- fb1 %>% rename(CntyHigEduc = `Country of Highest Education`)
fb1<- fb1 %>% rename(CltFTEmpyt = `Client Full Time Employment`)
fb1<- fb1 %>% rename(ClntOthr = `Client Other`)
fb1<- fb1 %>% rename(CltPrtTEmp = `Client Part Time Employment`)
fb1<- fb1 %>% rename(CltPriDisa = `Client Private Disability`)
fb1<- fb1 %>% rename(CltPriPens = `Client Private Pension`)
fb1<- fb1 %>% rename(CltSocAstc = `Client Social Assistance`)
fb1<- fb1 %>% rename(CltSSDI = `Client Social Security Disability Insurance (SSDI)`)
fb1<- fb1 %>% rename(ClnSpseFamS = `Client Spouse/Family Support`)
fb1<- fb1 %>% rename(ClntCSFPID = `Client CSFP ID`)
fb1<- fb1 %>% rename(ClntCSFPSt = `Client CSFP Status`)
fb1<- fb1 %>% rename(ClntCSFPLctn = `Client CSFP Location`)
fb1<- fb1 %>% rename(ClntEthCat = `Client Ethnicity Category`)
fb1<- fb1 %>% rename(ZipCode = `Zip Code`)
fb1<- fb1 %>% rename(HousngTyp = `Housing Type`)
fb1<- fb1 %>% rename(HouldID = `Household ID`)
fb1<- fb1 %>% rename(HouldSiz = `Household Size`)
fb1<- fb1 %>% rename(HouldLang = `Household Languages`)
fb1<- fb1 %>% rename(HhldPrInSce = `Household Primary Income Source`)
fb1<- fb1 %>% rename(MothHoldInc = `Monthly Household Income`)
fb1<- fb1 %>% rename(TotMontExp = `Total Monthly Expenses`)
fb1<- fb1 %>% rename(TotMonNtInc = `Total Monthly Net Income`)
fb1<- fb1 %>% rename(HouhdExpChild = `Household Expense (Childcare)`)
fb1<- fb1 %>% rename(HouhExpFood = `Household Expense (Food)`)
fb1<- fb1 %>% rename(HouhExpMed = `Household Expense (Medical)`)
fb1<- fb1 %>% rename(HouhExpMrt = `Household Expense (Mortgage)`)
fb1<- fb1 %>% rename(HouhExpOth = `Household Expense (Other)`)
fb1<- fb1 %>% rename(HouhExpRnt = `Household Expense (Rent)`)
fb1<- fb1 %>% rename(HouhExpScEx = `Household Expense (School Expenses)`)
fb1<- fb1 %>% rename(HouhExpTran = `Household Expense (Transit)`)
fb1<- fb1 %>% rename(HouhExpUti = `Household Expense (Utilities)`)
fb1<- fb1 %>% rename(HouhExpVeh = `Household Expense (Vehicle)`)
fb1<- fb1 %>% rename(DietConsid = `Dietary Considerations`)
fb1<- fb1 %>% rename(SocialAsst = `Social Assistance` )
fb1<- fb1 %>% rename(ProgNam = `Program Name`)
fb1<- fb1 %>% rename(ProgTyp = `Program Type`)
fb1<- fb1 %>% rename(HHMM1ID = `HH Mem 1- ID`)
fb1<- fb1 %>% rename(HHMM1Sts = `HH Mem 1- Status`)
fb1<- fb1 %>% rename(HHMM1Age = `HH Mem 1- Age`)
fb1<- fb1 %>% rename(HHMM1GILab = `HH Mem 1- Gender Identity-Labels`)
fb1<- fb1 %>% rename(HHMM1GIPT = `HH Mem 1- Gender Identity-Parent Types`)
fb1<- fb1 %>% rename(HHMM1EthL = `HH Mem 1- Ethnicity-Labels`)
fb1<- fb1 %>% rename(HHMM1EthParT = `HH Mem 1- Ethnicity-Parent Types`)
fb1<- fb1 %>% rename(HHMM1Disb = `HH Mem 1- Disability`)
fb1<- fb1 %>% rename(HHMM1SfIdAs = `HH Mem 1- Self-Identifies As`)
fb1<- fb1 %>% rename(HHMM1IsStd = `HH Mem 1- Is Student`)
fb1<- fb1 %>% rename(HHMM1RhpMClt = `HH Mem 1- Relationship to Main Client`)
fb1<- fb1 %>% rename(HHMM2ID = `HH Mem 2- ID`)
fb1<- fb1 %>% rename(HHMM2Stat = `HH Mem 2- Status`)
fb1<- fb1 %>% rename(HHMM2Ag = `HH Mem 2- Age`)
fb1<- fb1 %>% rename(HHMM2GIL = `HH Mem 2- Gender Identity-Labels`)
fb1<- fb1 %>% rename(HHMM2GIPT = `HH Mem 2- Gender Identity-Parent Types`)
fb1<- fb1 %>% rename(HHMM2Eth = `HH Mem 2- Ethnicity-Labels`)
fb1<- fb1 %>% rename(HHMM2EthPrt = `HH Mem 2- Ethnicity-Parent Types`)
fb1<- fb1 %>% rename(HHMM2Disbty = `HH Mem 2- Disability`)
fb1<- fb1 %>% rename(HHMM2SfId= `HH Mem 2- Self-Identifies As`)
fb1<- fb1 %>% rename(HHMM2IsStd = `HH Mem 2- Is Student`)
fb1<- fb1 %>% rename(HHMM2RlspMC = `HH Mem 2- Relationship to Main Client`)
fb1<- fb1 %>% rename(HHMM3ID = `HH Mem 3- ID` )
fb1<- fb1 %>% rename(HHMM3Stat = `HH Mem 3- Status`)
fb1<- fb1 %>% rename(HHMM3Age = `HH Mem 3- Age`)
fb1<- fb1 %>% rename(HHMM3GdIdLab = `HH Mem 3- Gender Identity-Labels`)
fb1<- fb1 %>% rename(HHMM3GndIdPrt = `HH Mem 3- Gender Identity-Parent Types`)
fb1<- fb1 %>% rename(HHMM3EthLab = `HH Mem 3- Ethnicity-Labels` )
fb1<- fb1 %>% rename(HHMM3EthPrt = `HH Mem 3- Ethnicity-Parent Types`)
fb1<- fb1 %>% rename(HHMM3SIdAs= `HH Mem 3- Self-Identifies As`)
fb1<- fb1 %>% rename(HHMM3IsStd = `HH Mem 3- Is Student`)
fb1<- fb1 %>% rename(HHMM3RlhMnCl = `HH Mem 3- Relationship to Main Client`)

#names(fb1)
attach(fb1)
#count total missing values in entire data frame
sum(is.na(fb1))     #116631 values missing, but those are dates
#prints the number of N/A values for each variable
#apply(X = is.na(fb1), MARGIN = 2, FUN = sum)
#prints the proportion of missing values per column in this case
fb1 <- fb1[complete.cases(fb1),]     
#sum(is.na(fb1))   #now we have 0 values missing in the data
dim(fb1)          #19,428   101

set.seed(84310)
partition <- createDataPartition(y = fb1$`MothHoldInc`, 
                                 p = 0.8, list = FALSE)
cs_train <- fb1[partition, ]
cs_test <- fb1[-partition, ]

set.seed(7291)
cs_mdl_cart_full <- rpart(`MothHoldInc` ~ ., 
                          cs_train, method = "anova")

#fb2$ProgNam<- factor(fb2$ProgNam,levels = c("Pantry Visit", "TEFAP Pantry Visit"),
#labels = c(TRUE, FALSE))
print(cs_mdl_cart_full)
rpart.plot(cs_mdl_cart_full, yesno = TRUE)
printcp(cs_mdl_cart_full)

set.seed(3187)
cs_mdl_cart_full$cptable %>%
  data.frame() %>%
  mutate(min_xerror_idx = which.min(cs_mdl_cart_full$cptable[, "xerror"]),
         rownum = row_number(),
         xerror_cap = cs_mdl_cart_full$cptable[min_xerror_idx, "xerror"] + 
           cs_mdl_cart_full$cptable[min_xerror_idx, "xstd"],
         eval = case_when(rownum == min_xerror_idx ~ "min xerror",
                          xerror < xerror_cap ~ "under cap",
                          TRUE ~ "")) %>%
  select(-rownum, -min_xerror_idx) 

plotcp(cs_mdl_cart_full, upper = "splits")


cs_mdl_cart <- prune(cs_mdl_cart_full,
                     cp = cs_mdl_cart_full$cptable[cs_mdl_cart_full$cptable[, 2] == 3, 
                                                   "CP"])
rpart.plot(cs_mdl_cart, yesno = TRUE)

cs_mdl_cart$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Variable Importance with Simple Regression")

cs_preds_cart <- predict(cs_mdl_cart, cs_test, type = "vector")

cs_rmse_cart <- RMSE(
  pred = cs_preds_cart,
  obs = cs_test$MothHoldInc)

cs_rmse_cart

data.frame(Predicted = cs_preds_cart, Actual = cs_test$MothHoldInc) %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "cadetblue") +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(title = "Carseats CART, Predicted vs Actual")

p <- predict(cs_mdl_cart_full, cs_train)

sqrt(mean(cs_train$`MothHoldInc`-p)^2)

#sually, the larger the R2, the better the regression model fits 
#your observations, r square, 0.826503
(cor(cs_train$`MothHoldInc`, p))^2

#prunning the tree in this case will not increase the level of confidence,
#rather it lowers it down 
prunep<-predict(cs_mdl_cart, cs_train)
sqrt(mean(cs_train$`MothHoldInc`-prunep)^2)
(cor(cs_train$`MothHoldInc`, prunep))^2
