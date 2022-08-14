#----------------------------LOGISTIC REGRESSION------------------------------
##-------------------------------First steps----------------------------------
#Setting the working directory
setwd("C:/Users/user/Desktop/Maputo")

#Libraries
library(sf) #to import files ".shp"
library(caTools) #to split the data set
library(GGally) #for the scatterplot

#Import the data set
lance = st_read("lance.shp")
View(lance)
summary(lance)
#We are gonna work on the first 2558 rows of the data set
data = lance[1:2558,]

#The target variable is "osm_surf" which is of type character and it can be
#either "paved" or "unpaved". We set "paved" = 1 and "unpaved" = 0.
data$osm_surf = ifelse(data$osm_surf == "paved",1,0)
View(data)
summary(data)

#Now we need to split the data set in two: the training set which contains the 
#80% of the observations, and the test set. We are going to work on the first one
#to build the model and for its validation while the second one will be used to
#test the accuracy of the model.
#Then, we will apply the model on the remaining streets of "datonite" for
#which we do not know the condition of the surface
set.seed(12052022)
split = sample.split(data,SplitRatio=0.8)
split
training = subset(data,split=="TRUE")
rownames(training) = 1:nrow(training)
dim(training)
View(training)

testing = subset(data,split=="FALSE")
rownames(testing)=1:nrow(testing)
dim(testing)
View(testing)

attach(training) #from now on we are gonna work on the training set

##-----------------------------First model--------------------------------------
#The goal is to classify the streets surface in "paved" (1) and "unpaved" (0)
#and the idea is to use the distribution of the colors red, green and blue 
#contained in each picture. To characterize these distributions we use their 
#mean values, the variances, the medians and the minimum and maximum values.
model = glm(osm_surf ~  
                       rmean+rvar+rmed+rmin+rmax+
                       gmean+gvar+gmed+gmin+gmax+
                       bmean+bvar+bmed+bmin+bmax,
                       data = training,family = binomial(link = logit))
summary(model)
BIC(model)

#Looking at the p-values we can try to improve the model leaving behind less 
#influential predictors.
model2 = glm(osm_surf ~  
                        rmin+rmax+
                        gmean+gvar+gmed+gmax+
                        bvar+bmin,
                        data = training,family = binomial(link = logit))
summary(model2)
BIC(model2)
#We notice that both AIC and BIC have increased.

#Let's do the Chi-squared test to see if one model is less informative than 
#the other.
anova(model,model2,test = "Chisq")
#The probability of the test statistic is large if the two models provided the 
#same fit. In this case, we have to reject the null hypothesis (that the 
#likelihoods of the two models are equivalent) since the p-value is smaller than 
#0.05 and this means that the model with less predictors is less informative than 
#the more complex one. This is confirmed by looking at the AIC and BIC, since we 
#know that the lower they are, the better the model is.

#We tried to remove the statistically insignificant parameters and the AIC of 
#the model did not improve: this migth be explained by the collinearity of the 
#predictors.
x11()
ggpairs(data = training, columns = 7:21, title ="Relationships between predictors", 
        lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))
#When predictor variables in the same regression model are correlated, they 
#cannot independently predict the value of the dependent variable. In other 
#words, they explain some of the same variance in the dependent variable, which 
#in turn reduces their statistical significance.

##---------------------------Stepwise selection--------------------------------
#The stepwise selection consists of iteratively adding removing predictors, in 
#the predictive model, in order to find the subset of variables in the data set 
#resulting in the best performing model, that is a model that lowers prediction 
#error.
step(model, direction = "backward" , trace = T)
#Backward selection (or backward elimination), which starts with all predictors 
#in the model (full model), iteratively removes the least contributive 
#predictors, and stops when you have a model where all predictors are 
#statistically significant.

model = glm(osm_surf ~  rmed+rmin+rmax+
                        gmean+gvar+gmax+
                        bmean+bvar+bmed+bmin+bmax,
                        data = training,family = binomial(link = logit))
summary(model)

##---------------------Diagnostic and goodness of fit--------------------------
#To know if the prediction we made with our model are actually good we build the 
#confusion matrix, but first we need to fix a cutoff. When we choose a threshold
#(initially, we are going to choose 0.4), we are saying that we would like to 
#classify every observation with a predicted probability from the model equal to 
#or greater than 0.5 as a "success" and we will classify observations meeting 
#this criteria as a success regardless if that outcome was actually observed to 
#be a success.
real.values = osm_surf
predicted.values = model$fitted.values
threshold = 0.5
predicted.values = as.numeric(predicted.values>threshold)
confusion.matrix = table(real.values,predicted.values)
confusion.matrix

#True Positive when the real value is 1 and it is predicted as 1
TP = confusion.matrix[2,2]
#True Negative when the real value is 0 and it is predicted as 0
TN = confusion.matrix[1,1]
#False Positive when the real value is 0, but it is predicted as 1
FP = confusion.matrix[1,2]
#False Negative when the real value is 1, but it is predicted as 0
FN = confusion.matrix[2,1]

#Evaluate the performance of our model in terms of the quality of its 
#predictions
#The accuracy of the model which is the proportion of correct predictions on 
#the total
Accuracy = (TP+TN)/(TP+TN+FP+FN)
Accuracy
#The sensitivity which is the proportion of true positive classified as positive
Sensitivity = TP/(TP+FN)
Sensitivity
#The specificity which is the proportion of true negative classified as negative
Specificity = TN/(TN+FP)
Specificity
#In our scenario, FN value represents a street that is predicted to be unpaved, 
#but in reality its surface is asphalted. The goal of this analysis is to do 
#classification and this can be used to organize an operation on the streets of 
#the capital, to pave all those streets which are still unpaved. An estimate on
#the number of unpaved streets is needed to create a budget and, in our opinion, 
#it is better to predict more paved streets than unpaved once, also because once
#we count the FN to create the budget, it is a waste of resources and money 
#since those streets are already paved.
#In conclusion, we need to work on the threshold to maximize the sensitivity and
#minimize the FN.

#10-fold cross-validation
threshold = c(0.2,0.25,0.3,0.35,0.4,0.45,0.5)
accuracy = c()
sensitivity = c()
specificity = c()
RMSE = c()
Acc = c()
Sens = c()
Spec = c()
rmse = c()

for (i in 1:length(threshold)) {
  for (k in 1:10) {
    validation = training[((k-1)*round(0.1*dim(training)[1])+1):(k*round(0.1*dim(training)[1])-1),]
    train = training[-c(((k-1)*round(0.1*dim(training)[1])+1):(k*round(0.1*dim(training)[1])-1)),]
    
    mod = glm(osm_surf ~ rmean+rmin+rmax+
                         gmean+gvar+gmed+gmax+
                         bvar+bmed+bmin+bmax, 
                         data = train, family = binomial(link = logit))
    
    real.values = validation$osm_surf
    
    predicted.values = as.numeric(predict(mod,newdata=validation,type="response")>threshold[i])
    
    confusion.matrix = table(real.values,predicted.values)
    confusion.matrix
    TP = confusion.matrix[2,2]
    TN = confusion.matrix[1,1]
    FP = confusion.matrix[1,2]
    FN = confusion.matrix[2,1]
    Acc = c(Acc,(TP+TN)/(TP+TN+FP+FN))
    Sens = c(Sens,TP/(TP+FN))
    Spec = c(Spec,TN/(TN+FP))
    rmse = c(rmse,sqrt(mean((real.values-predicted.values)^2)))
    
    remove(validation)
    remove(train)
  }
  
  accuracy = c(accuracy,mean(Acc))
  sensitivity = c(sensitivity,mean(Sens))
  specificity = c(specificity,mean(Spec))
  RMSE = c(RMSE,mean(rmse))
}

accuracy
sensitivity
specificity

##---------------------------Testing the model---------------------------------
#Now that we have tried to come up with a good model, we can try to use it to 
#make some prediction on the testing data set. The idea is to use the model to 
#predict whether a street (for which we still know the condition of its surface)
#is 'paved' (1) or 'unpaved' (0) and confront the predicted values with the real
#values. Once we have obtained a good proportion of correct predictions, we can
#use the model on the rest of the streets for the paviment detection.
predicted.values = predict(model,newdata=testing,type="response")
threshold = 0.35
predicted.values = ifelse(predicted.values>threshold,1,0)
head(predicted.values)

real.values = testing$osm_surf
tab = table(real.values,predicted.values)
tab

Accuracy = (tab[1,1]+tab[2,2])/(tab[1,1]+tab[1,2]+tab[2,1]+tab[2,2])
Accuracy

Sensitivity = tab[2,2]/(tab[2,1]+tab[2,2]) 
Sensitivity

Specificity = tab[1,1]/(tab[1,2]+tab[1,1])
Specificity

#We are interested in the misclassified streets
id_misclassidied = c()
misclassified = c()

for (i in 1:dim(testing)[1]) {
  if (testing$osm_surf[i] != predicted.values[i]) {
    id_misclassidied = c(id_misclassidied,testing$id[i])
    misclassified = c(misclassified,testing$osm_typo[i])
  }
}

id_misclassidied

tab_misclassified <- factor(misclassified,levels = c('footway', 'primary', 'residentia', 'secondary', 'tertiary', 'unk'))
tab_misclassified = table(tab_misclassified)
tab_misclassified

tab_total = factor(testing$osm_typo,levels = c('footway', 'primary', 'residentia', 'secondary', 'tertiary', 'unk'))
tab_total = table(tab_total)
tab_total

percent = tab_misclassified/tab_total
percent

##-----------------------New model using the type of street--------------------
#Since "osm_typo" is a categorical variable, we need to create the so called 
#dummy variables
training$osm_typo_footway = ifelse(osm_typo == 'footway',1,0)
training$osm_typo_primary = ifelse(osm_typo == 'primary',1,0)
training$osm_typo_residentia = ifelse(osm_typo == 'residentia',1,0)
training$osm_typo_secondary = ifelse(osm_typo == 'secondary',1,0)
training$osm_typo_tertiary = ifelse(osm_typo == 'tertiary',1,0)
training$osm_typo_unk = ifelse(osm_typo == 'unk',1,0)
View(training)

testing$osm_typo_footway = ifelse(testing$osm_typo == 'footway',1,0)
testing$osm_typo_primary = ifelse(testing$osm_typo == 'primary',1,0)
testing$osm_typo_residentia = ifelse(testing$osm_typo == 'residentia',1,0)
testing$osm_typo_secondary = ifelse(testing$osm_typo == 'secondary',1,0)
testing$osm_typo_tertiary = ifelse(testing$osm_typo == 'tertiary',1,0)
testing$osm_typo_unk = ifelse(testing$osm_typo == 'unk',1,0)
View(testing)

model2 = glm(osm_surf ~  
                        rmed+rmin+rmax+
                        gmean+gvar+gmax+
                        bmean+bvar+bmed+bmin+bmax+
                        osm_typo_footway+osm_typo_primary+osm_typo_residentia+osm_typo_secondary+osm_typo_tertiary+osm_typo_unk,
                        data = training,family = binomial(link = logit))
summary(model2)
#The error message "1 not defined because of singularities" occurs when we fit 
#the model and two or more predictor variables have an exact linear relationship 
#between them (perfect multicollinearity). To fix this error, we can identify 
#which variables in your data set have a perfect correlation with each other and 
#simply drop one of those variables from the regression model.
step(model2, direction = "backward" , trace = T)

model3 = glm(osm_surf ~  
                        rmed+rmax+
                        bmean+bvar+bmin+bmax+
                        osm_typo_footway+osm_typo_primary+osm_typo_residentia+osm_typo_secondary+osm_typo_tertiary,
                        data = training,family = binomial(link = logit))
summary(model3)

predicted.values = predict(model3,newdata=testing,type="response")
threshold = 0.35
predicted.values = ifelse(predicted.values>threshold,1,0)

real.values = testing$osm_surf

tab = table(real.values,predicted.values)
tab

Accuracy = (tab[1,1]+tab[2,2])/(tab[1,1]+tab[1,2]+tab[2,1]+tab[2,2])
Accuracy
Sensitivity = tab[2,2]/(tab[2,1]+tab[2,2]) 
Sensitivity
Specificity = tab[1,1]/(tab[1,2]+tab[1,1])
Specificity

id_misclassidied3 = c()
misclassified = c()

for (i in 1:dim(testing)[1]) {
  if (testing$osm_surf[i] != predicted.values[i]) {
    id_misclassidied3 = c(id_misclassidied3,testing$id[i])
    misclassified = c(misclassified,testing$osm_typo[i])
  }
}

id_misclassidied3

tab_misclassified <- factor(misclassified,levels = c('footway', 'primary', 'residentia', 'secondary', 'tertiary', 'unk'))
tab_misclassified = table(tab_misclassified)
tab_misclassified

tab_total = factor(testing$osm_typo,levels = c('footway', 'primary', 'residentia', 'secondary', 'tertiary', 'unk'))
tab_total = table(tab_total)
tab_total

percent = tab_misclassified/tab_total
percent

##----------Application of the model on the unknown streets of Maputo-----------
#We use the model built in the previous sections to predict the surface for the
#remaining streets of the data set 'datonair' for which we do not know if they 
#are paved or not.
detach(training)
data = datonite[2559:5116,]
attach(data)
data$osm_typo_footway = ifelse(osm_typo == 'footway',1,0)
data$osm_typo_primary = ifelse(osm_typo == 'primary',1,0)
data$osm_typo_residentia = ifelse(osm_typo == 'residentia',1,0)
data$osm_typo_secondary = ifelse(osm_typo == 'secondary',1,0)
data$osm_typo_tertiary = ifelse(osm_typo == 'tertiary',1,0)
data$osm_typo_unk = ifelse(osm_typo == 'unk',1,0)
predicted.values = predict(model3,newdata=data,type="response")
threshold = 0.35
predicted.values = ifelse(predicted.values>threshold,1,0)

for (i in 1:dim(data)[1]) {
  data$osm_surf[i]=predicted.values[i]
}

#Great Maputo area and plot of the segments of the streets predicted through 
#logistic regression.
total = rbind(training,testing,data)

st_bbox(total)
(st_bbox(total)[3]-st_bbox(total)[1])*(st_bbox(total)[4]-st_bbox(total)[2])
sum(total$Length)*1e-03
i_asp = which(total$osm_surf==1)
i_unp = which(total$osm_surf==0)
ttt = character(dim(total)[1])
ttt[i_unp]="1. unpaved"; ttt[i_asp]="2. paved"

windows()  
ggplot() + 
  geom_sf(data = total, aes(color=ttt,fill=ttt))+
  scale_fill_manual(values=c("forestgreen", "gold"))+
  scale_color_manual(values=c("forestgreen", "gold"))+
  labs(fill= "Pavement surface")+
  ggtitle("Road network of the Greater Maputo area") + 
  coord_sf() +
  theme(panel.grid.major = element_line(color = gray(0.9), linetype=3, size=0.2), 
        panel.background = element_rect(fill="white"))+
  guides(color=FALSE)

#-------Comparison between the misclassified streets in the two models---------
id_misclassidied #ids of the streets misclassified by the model w7 'osm_typo'
id_misclassidied3 #ids of the streets misclassified by the last model
is_in = id_misclassidied3 %in% id_misclassidied
intersection = c()
for (i in 1:length(is_in)) {
  if(is_in[i]==TRUE)
    intersection = c(intersection,id_misclassidied3[i])
}
intersection #ids of the streets misclassified in both models

misclassified_streets = testing
i=1
while (i<=dim(misclassified_streets)[1]) {
  if(misclassified_streets$id[i]%in%intersection)
    i=i+1
  else
    misclassified_streets=misclassified_streets[-i,]
}

detach(training)
attach(misclassified_streets)
categories = factor(osm_surf, labels=c(1, 0))

x11()
par(mfrow=c(1,5))
boxplot(rmean~categories, col = rainbow(2))
boxplot(rvar~categories, col = rainbow(2))
boxplot(rmed~categories, col = rainbow(2))
boxplot(rmin~categories, col = rainbow(2))
boxplot(rmax~categories, col = rainbow(2))

wilcox.test(rmean ~ categories)
wilcox.test(rvar ~ categories)
wilcox.test(rmed ~ categories)
wilcox.test(rmin ~ categories)
wilcox.test(rmax ~ categories)

x11()
par(mfrow=c(1,5))
boxplot(gmean~categories, col = rainbow(2))
boxplot(gvar~categories, col = rainbow(2))
boxplot(gmed~categories, col = rainbow(2))
boxplot(gmin~categories, col = rainbow(2))
boxplot(gmax~categories, col = rainbow(2))

wilcox.test(gmean ~ categories)
wilcox.test(gvar ~ categories)
wilcox.test(gmed ~ categories)
wilcox.test(gmin ~ categories)
wilcox.test(gmax ~ categories)

x11()
par(mfrow=c(1,5))
boxplot(bmean~categories, col = rainbow(2))
boxplot(bvar~categories, col = rainbow(2))
boxplot(bmed~categories, col = rainbow(2))
boxplot(bmin~categories, col = rainbow(2))
boxplot(bmax~categories, col = rainbow(2))

wilcox.test(bmean ~ categories)
wilcox.test(bvar ~ categories)
wilcox.test(bmed ~ categories)
wilcox.test(bmin ~ categories)
wilcox.test(bmax ~ categories)

#For all 3 colors there is statistically significant difference in the mean 
#values of the variances between paved and unpaved streets. We notice that for 
#the red and green, the variance is not statistically significant, so...

tab_misclassified = factor(misclassified_streets$osm_typo,levels = c('footway', 'primary', 'residentia', 'secondary', 'tertiary', 'unk'))
tab_misclassified = table(tab_misclassified)
tab_misclassified

#--------------------------------Final product---------------------------------
set.seed(10094)
lance = lance[-c(4287,1218,4368,3337,3325,3990),]
lance2 = lance[which(lance$osm_surf!='unk'),]
sam = sample(2558,2300)
train = lance2[sam,]
test = lance2[-sam,]
rownames(train) = 1:2300
rownames(test) = 1:257

train$osm_surf = ifelse(train$osm_surf == 'paved',1,0)
train$osm_typo_footway = ifelse(train$osm_typo == 'footway',1,0)
train$osm_typo_primary = ifelse(train$osm_typo == 'primary',1,0)
train$osm_typo_residentia = ifelse(train$osm_typo == 'residentia',1,0)
train$osm_typo_secondary = ifelse(train$osm_typo == 'secondary',1,0)
train$osm_typo_tertiary = ifelse(train$osm_typo == 'tertiary',1,0)
train$osm_typo_unk = ifelse(train$osm_typo == 'unk',1,0)

model = glm(osm_surf ~  
                      rmean+rvar+rmed+rmin+rmax+
                      gmean+gvar+gmed+gmin+gmax+
                      bmean+bvar+bmed+bmin+bmax+
                      osm_typo_footway+osm_typo_primary+osm_typo_residentia+osm_typo_secondary+osm_typo_tertiary+osm_typo_unk,
                      data = train,family = binomial(link = logit))
summary(model)

step(model, direction = "backward" , trace = T)

final_model = glm(osm_surf ~  
              rmed+rmin+rmax+
              gvar+gmin+
              bmean+bmax+
              osm_typo_footway+osm_typo_primary+osm_typo_residentia+osm_typo_secondary+osm_typo_tertiary,
              data = train,family = binomial(link = logit))
summary(final_model)

threshold = c(0.2,0.25,0.3,0.35,0.4,0.45,0.5)
accuracy = c()
sensitivity = c()
specificity = c()
RMSE = c()
Acc = c()
Sens = c()
Spec = c()
rmse = c()

for (i in 1:length(threshold)) {
  for (k in 1:10) {
    validation = train[((k-1)*round(0.1*dim(train)[1])+1):(k*round(0.1*dim(train)[1])-1),]
    trainino = train[-c(((k-1)*round(0.1*dim(train)[1])+1):(k*round(0.1*dim(train)[1])-1)),]
    
    mod = glm(osm_surf ~ rmean+rmin+rmax+
                gmean+gvar+gmed+gmax+
                bvar+bmed+bmin+bmax, 
                data = trainino, family = binomial(link = logit))
    
    real.values = validation$osm_surf
    
    predicted.values = as.numeric(predict(mod,newdata=validation,type="response")>threshold[i])
    
    confusion.matrix = table(real.values,predicted.values)
    confusion.matrix
    TP = confusion.matrix[2,2]
    TN = confusion.matrix[1,1]
    FP = confusion.matrix[1,2]
    FN = confusion.matrix[2,1]
    Acc = c(Acc,(TP+TN)/(TP+TN+FP+FN))
    Sens = c(Sens,TP/(TP+FN))
    Spec = c(Spec,TN/(TN+FP))
    rmse = c(rmse,sqrt(mean((real.values-predicted.values)^2)))
    
    remove(validation)
    remove(trainino)
  }
  
  accuracy = c(accuracy,mean(Acc))
  sensitivity = c(sensitivity,mean(Sens))
  specificity = c(specificity,mean(Spec))
  RMSE = c(RMSE,mean(rmse))
}

accuracy
sensitivity
specificity

test$osm_surf = ifelse(test$osm_surf == 'paved',1,0)
test$osm_typo_footway = ifelse(test$osm_typo == 'footway',1,0)
test$osm_typo_primary = ifelse(test$osm_typo == 'primary',1,0)
test$osm_typo_residentia = ifelse(test$osm_typo == 'residentia',1,0)
test$osm_typo_secondary = ifelse(test$osm_typo == 'secondary',1,0)
test$osm_typo_tertiary = ifelse(test$osm_typo == 'tertiary',1,0)
test$osm_typo_unk = ifelse(test$osm_typo == 'unk',1,0)

predicted.values = predict(final_model,newdata=test,type="response")
threshold = 0.35
predicted.values = ifelse(predicted.values>threshold,1,0)
head(predicted.values)

real.values = test$osm_surf
tab = table(real.values,predicted.values)
tab

Accuracy = (tab[1,1]+tab[2,2])/(tab[1,1]+tab[1,2]+tab[2,1]+tab[2,2])
Accuracy

Sensitivity = tab[2,2]/(tab[2,1]+tab[2,2]) 
Sensitivity

Specificity = tab[1,1]/(tab[1,2]+tab[1,1])
Specificity

data = lance[which(lance$osm_surf == 'unk'),]
data$osm_surf = ifelse(data$osm_surf == 'paved',1,0)
data$osm_typo_footway = ifelse(data$osm_typo == 'footway',1,0)
data$osm_typo_primary = ifelse(data$osm_typo == 'primary',1,0)
data$osm_typo_residentia = ifelse(data$osm_typo == 'residentia',1,0)
data$osm_typo_secondary = ifelse(data$osm_typo == 'secondary',1,0)
data$osm_typo_tertiary = ifelse(data$osm_typo == 'tertiary',1,0)
data$osm_typo_unk = ifelse(data$osm_typo == 'unk',1,0)

threshold = 0.35
predicted.values = ifelse(predict(final_model,newdata=data,
                                  type="response")>threshold,1,0)

for (i in 1:dim(data)[1]) {
  data$osm_surf[i]=predicted.values[i]
}

total = rbind(train,test,data)

st_bbox(total)
(st_bbox(total)[3]-st_bbox(total)[1])*(st_bbox(total)[4]-st_bbox(total)[2])
sum(total$Length)*1e-03
i_asp = which(total$osm_surf==1)
i_unp = which(total$osm_surf==0)
ttt = character(dim(total)[1])
ttt[i_unp]="1. unpaved"; ttt[i_asp]="2. paved"

windows()  
ggplot() + 
  geom_sf(data = total, aes(color=ttt,fill=ttt))+
  scale_fill_manual(values=c("forestgreen", "brown"))+
  scale_color_manual(values=c("forestgreen", "brown"))+
  labs(fill= "Pavement surface")+
    coord_sf() +
  theme(panel.grid.major = element_line(color = gray(0.9), linetype=3, size=0.2), 
        panel.background = element_rect(fill="cornsilk1"))+
  guides(color=FALSE)