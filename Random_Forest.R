Nguyen Doan
#Topic: Analyze survival rate of passengers from Titanic accident
#Number of observation: 3128
#Variables: PassengerID, SurvivedID, Passenger Class, Name, Sex, Age, 
#---------- Number of siblings/spouses aboard Titanic
#---------- Number of parents/children aboard Titanic
#---------- Ticket number, Passenger fare, Cabin number, Port of embarkation

install.packages('ggplot2')
install.packages('ggthemes')
install.packages('scales')
install.packages('dplyr')
install.packages('mice')
install.packages('randomForest')
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

#setting full dataset

rm(list=ls()) #clear current R environment
setwd("~/Downloads/") #need to modify to directory that contains the dataset

train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)
full  <- bind_rows(train, test) # bind train & test data

#remove special character and extract Title from Name
full$Title <- gsub('(.*, )|(\\..*)','', full$Name)

#Edit title 
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

full$Title[full$Title == "Mlle"] <- 'Miss'
full$Title[full$Title == 'Ms']   <- 'Miss'
full$Title[full$Title == 'Mme']  <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

#extract Surname from Name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
nlevels(factor(full$Surname)) #good trick to count by turning each name into factor because it combines same names into one factor

#extract Family size
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep='_')

ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size')

#factor Family size into 3 categories
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

split <- function(x){
  strsplit(x, NULL)[[1]][1]
}

full$Deck <- factor(sapply(full$Cabin, split))

#Check NA in Deck 
table(full$Deck, useNA = "always")

#Get rid of passengers with missing ID
embark_fare <- full %>% 
  filter(PassengerId != 62 & PassengerId != 830) 


#Average fare per embarked deck
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few() #change to white background

#Because most passengers from class 1 embarked from deck C
full$Embarked[c(62, 830)] <- 'C'

#Fare density plot
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff',alpha=0.4) + #alpha is used for transparency of area  
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

#impute value for NA in fare
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

#checking number of missing values
md.pattern(full)

#assign random values to NA values in PassengerID, Name, Ticket, Cabin, Family, Surname, Survived
set.seed(129)
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf')
mice_output <- complete(mice_mod)

#age distribution before and after imputation are the same
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04)) #set freq=F to return probability density instead of frequency
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

#recheck number of missing values in Age
full$Age <- mice_output$Age

#number of NAs is zero now
sum(is.na(full$Age))

ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  facet_grid(.~Sex) + 
  theme_few()

full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'
table(full$Child, full$Survived)

full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$`Parch` > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

full$Child <- factor(full$Child)
full$Mother <- factor(full$Mother)

md.pattern(full) #investigate NA/missing values

#re-create train and test dataset after manipulating full dataset
train = sample(1:nrow(full), nrow(full)*2/3)
final.train = full[train,]
final.test = full[-train,]
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         data = final.train)

plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

importance    <- importance(rf_model)
importance

varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
varImportance

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
rankImportance

## ggplot : reorder() -> sorting variables
##          stat='identity' -> set heights of bars to represent values in the data
##          coord_flip() -> flip x and y coordinates
ggplot(rankImportance, aes(x = reorder(Variables, Importance) , 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55,size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
solution
View(full)

