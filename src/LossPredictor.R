########################################################
#           installing required packages               #
########################################################
install.packages('caret', repos='http://cran.rstudio.com/',dependencies=TRUE)
install.packages('e1071', dependencies=TRUE)
########################################################
#           loading libararies                         #
########################################################
library('caret')
library(randomForest)
library(rpart)

########################################################
#           loading data                               #
########################################################
data <- read.table("../resources/train_v2.csv", header=TRUE, sep=",")


#set seed to make random sample equal each time
set.seed(3103)
########################################################
#           Interresting Features                       #
########################################################
#find correlation of all columns with loss class attribute
correlation=cor(data[,1:ncol(data)-1],data$loss)
correlation[is.na(correlation)] = 0    #replace NA with 0 in correlation matrix
colnames(correlation)=c("loss") #assigning column name to correlation matrix
df=as.data.frame(correlation)   #convert matrix into data frame
df["attribute"]=NULL    #add a new column to store attribute name
df["attribute"]=colnames(data[1:ncol(data)-1])    #initialize column attribute with column names of data
sf=df[order(-df$loss),] #order data in descending order
head(sf$attribute,5)  #display top five highly correlated features with class attribute
########################################################
#           Data Preprocessing steps                   #
########################################################

#Step 01: remove id
rdata<-dataset[,2:ncol(dataset)]
#Step 02: remove duplicated columns
rdata<-rdata[!duplicated(as.list(rdata))]
#Step 03: missing values columns
rdata<-rdata[!colSums(is.na(rdata)) > 0]
#Step 04: remove all the columns who have same value for all records
markDelete<-as.logical(c(1:ncol(rdata)-1))
for(i in ncol(rdata)-1){
  markDelete[i]=length(unique(rdata[,i]))==1
}
rdata=rdata[,-markDelete]
#Step 05: remove near zero variance attributes
size=ncol(rdata) #number of columns in data
nzeroVariesFields <- nearZeroVar(rdata[,1:size-1])  #find new zero variance fields
rdata <- rdata[, -nzeroVariesFields] #take subset of columns without zero variance attributes

#convert loss in 0 or 1
rdata$loss=ifelse(rdata$loss>0,1,0)
#converting into factors
rdata$loss=as.factor(rdata$loss)

#devide data into training and testing
tsize=floor(nrow(rdata)*.7)
indices=c(1:tsize)
train=rdata[indices,]
test=rdata[-indices,]

#remove biasedness of model by minimizing 0's instances from the data
zeros=train[which(train$loss==0),]
ones=train[which(train$loss==1),]
index <- 1:nrow(zeros)
zindex <- sample(index, nrow(ones)*2, replace = FALSE)
zeros=train[zindex,]
train.data=rbind(zeros,ones)
#training and predicting using Decision Tree Model
DecisionTreePredictor<- function(train, test) {
        # Build decision tree model on given training dataset
        data_tree <- rpart(loss~., data=train, method="class",control=rpart.control(minsplit=2, minbucket=1, cp=0.001))
        plot(data_tree, uniform=TRUE, main="Classification Tree for X") # We can plot the decision tree model
        text(data_tree, use.n=TRUE, all=TRUE, cex=.8) # We can label the plot using this code
        
        # Predict's results using decision tree model build earlier and return the value
        return(predict(data_tree, newdata = test, type="class"))
}

#training and predicting using Random Forest
RandomForestPredictor <- function(train, test) {
        # Build decision tree model on given training dataset
        forest_model <- randomForest(y=train$loss, x=train[,1:ncol(train)-1],do.trace=F)
        # Predict's results using random forest model build earlier and return the value
        return(predict(forest_model,test))
}
#Decision Tree construction and prediction
pred1<-DecisionTreePredictor(train.data,test[,1:ncol(test)-1])
#Generatin confusion matrix
table(pred1,test$loss)
#accuracy
sum(as.character(pred1) == test$loss) / nrow(test)

#learning and prediction using randomForest
pred2<-RandomForestPredictor(train.data,test[,1:ncol(test)-1])
#Generatin confusion matrix
table(pred2,test$loss)
#accuracy
sum(as.character(pred2) == test$loss) / nrow(test)

