#Read in file
library(readxl)
library(radiant)
#Set working directory to the folder where I saved my code 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Indicate the file to be used
Merch <- read_excel("Merch_group 1.xlsx")
View(Merch)


Merch <- Merch %>%
  gather(respondent, rating, starts_with("Respondent")) %>%  
  mutate(Profile = factor(Profile), respondent = factor(respondent),  
         Price = factor(Price), Material = factor(Material), Straw = factor(Straw), Size = factor(Size))

#Conjoint analysis with individual respondents
respondent1 <- Merch %>% filter(respondent == "Respondent 1")

#Perform conjoint analysis
conjoint_respondent1 <- conjoint(respondent1, rvar = "rating", evar = c("Price", "Material", "Straw", "Size"))
summary(conjoint_respondent1)


#Plot the results
plot(conjoint_respondent1)

#Predicting preference for respondent 1
#first, you need to establish how many profiles are tested
profiles1 <- Merch %>% 
  filter(respondent == "Respondent 1") %>% 
  select(Price,Material,Straw,Size) 

profiles1

#Now you're ready to predict. You are predicting the ratings for the profiles based on the conjoint analysis result of that one individual
predict(conjoint_respondent1, profiles1) %>% arrange(desc(Prediction))



#Let's do the conjoint for all the respondents

#Remember to rename the variables created by expand.grid, so the predict function can read the data

profiles.all <- expand.grid(levels(Merch$Price),levels(Merch$Material),levels(Merch$Straw),levels(Merch$Size)) %>% 
  rename("Price" = "Var1", "Material" = "Var2", "Straw" = "Var3", "Size" = "Var4") 

#same as before, but the whole dataset
conjoint_allrespondents <- conjoint(Merch, rvar = "rating", evar = c("Price","Material","Straw","Size")) 

summary(conjoint_allrespondents) 

#Plot it
plot(conjoint_allrespondents)

# Predict ratings for all possible combinations based on all participants
predict(conjoint_allrespondents, profiles.all) %>% 
  arrange(desc(Prediction)) 


#Let's predict the market share for different options
#You can use the formula shown in the slides. Or you can see proportionally, how many respondents preferred a certain option.
# use slice() to select rows
market_profiles <- profiles.all %>% 
  slice(c(5, 10, 21, 45)) # CHANGE HERE!! from profiles.all,you should choose the combination that interests you.

market_profiles

# We already know how to predict the ratings
predict(conjoint_respondent1, market_profiles) %>%
  arrange(desc(Prediction))

#This tell us the overall rating but not the marketshare. To know the share, you need to know how every single respondents would react
# same model as before, but now add by = "respondent"
conjoint_perrespondent <- conjoint(Merch, rvar = "rating", evar = c("Price","Material","Straw","Size"), by = "respondent")

predict(conjoint_perrespondent, market_profiles) %>% 
  arrange(respondent, desc(Prediction)) # sort by respondent and then by predicted rating

#Retain for each individual only his or her highest rated profile
highest_rated <- predict(conjoint_perrespondent, market_profiles) %>% 
  group_by(respondent) %>% 
  mutate(ranking = rank(Prediction))
# have a look
highest_rated %>% 
  arrange(respondent, desc(ranking))
# we need to retain only the highest ranked pizza
highest_rated <- highest_rated %>% 
  arrange(respondent, ranking) %>% 
  filter(ranking == 3)

highest_rated

#Now you're finally ready to estimate the market share
market_share <- highest_rated %>% 
  group_by(Price, Material, Straw, Size) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

market_share 
