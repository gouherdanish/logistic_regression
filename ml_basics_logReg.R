####################################
## STEP 1 - Data Preparation
####################################

# Loading BreastCancer data
data("BreastCancer",package="mlbench")

# Removing NA's
cancer <- BreastCancer[complete.cases(BreastCancer),]

# Checking for NA's
colSums(is.na(cancer))

# Selecting x --> cell.size, y --> class
cancerDF <- cancer[,c("Cell.size","Class")]

# Converting cell.size to numeric
cancerDF$Cell.size <- as.numeric(as.character(cancerDF$Cell.size))

# Converting class to 1 (malignant) or 0 (benign)
cancerDF$Class <- ifelse(cancerDF$Class == "malignant", 1, 0)

# Converting class to factor
cancerDF$Class <- factor(cancerDF$Class, levels = c(0, 1))

str(cancerDF)

##################################
# STEP 2 -  Logistic Regression ##
##################################
logModel <- glm(Class ~ Cell.size, family="binomial", data = cancerDF)

summary(logModel)

##################################
# STEP 3 - Prediction ##
##################################
pred <- predict(logModel,cancerDF,type = "response")

pred <- ifelse(pred>0.5,1,0)

newdf <- cbind(cancerDF,pred)

##################################
# STEP 4 - Accuracy ##############
##################################
table(predicted=pred,Actual=cancerDF$Class)

accuracy <- nrow(newdf[newdf$Class==newdf$pred,]) / nrow(newdf)

cat("Your model is ",accuracy*100,"% correct")

plot(newdf$Cell.size,newdf$Class,col="blue",pch=16)
points(newdf$Cell.size,newdf$pred,col="red")
# 
# cancerDF <- sapply(cancerDF, function(x) as.factor(x))
# 
# cancerDF <- sapply(cancerDF, function(x) as.numeric(as.character(x)))
# 
# 
# 
# for(i in 1:nrow(cancerDF)){
#   if(cancerDF$Class[i]=="benign")
#     cancerDF$Class[i]==0
#   else cancerDF$Class[i]==1
# }
# levels(cancerDF)
# levels(cancerDF) <- c(0,1)


