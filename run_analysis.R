setwd("C:\\Users\\IBM_ADMIN\\Documents\\Datascience\\gettingAndCleaningData\\UCI HAR Dataset")
convert.characters.to.numeric<-function(dataframe)
{
  class.data  <- sapply(dataframe, class)
  character.vars <- class.data[class.data == "character"]
  for (colname in names(character.vars))
  {
    dataframe[,colname] <- as.numeric(dataframe[,colname]) 
  }
  return (dataframe)
}
features<-read.csv("features.txt",sep = "",header = F,col.names=c("s.no","Features"),stringsAsFactors=F)
features<-features[,c("Features")]
test_x<-read.csv2("C:\\Users\\IBM_ADMIN\\Documents\\Datascience\\gettingAndCleaningData\\UCI HAR Dataset\\test\\X_test.csv", sep = "",header=F,col.names=features,stringsAsFactors=F)
test_x<-test_x[,grep("mean|std",colnames(test_x),ignore.case=T)]
test_label<-read.csv2("C:\\Users\\IBM_ADMIN\\Documents\\Datascience\\gettingAndCleaningData\\UCI HAR Dataset\\test\\y_test.csv", sep = "",header=F,col.names=c("label"),stringsAsFactors=F)
test_subject<-read.csv2("C:\\Users\\IBM_ADMIN\\Documents\\Datascience\\gettingAndCleaningData\\UCI HAR Dataset\\test\\y_test.csv", sep = "",header=F,col.names=c("subjects"),stringsAsFactors=F)
test<-cbind(test_subject,test_label,test_x)


train_x<-read.csv2("C:\\Users\\IBM_ADMIN\\Documents\\Datascience\\gettingAndCleaningData\\UCI HAR Dataset\\train\\X_Train.txt",header=F,sep="",col.names=features,stringsAsFactors=F)
train_x<-train_x[,grep("mean|std",colnames(train_x),ignore.case=T)]
train_label<-read.csv2("C:\\Users\\IBM_ADMIN\\Documents\\Datascience\\gettingAndCleaningData\\UCI HAR Dataset\\train\\y_train.txt",header=F,col.names="label",sep="",stringsAsFactors=F)
train_subject<-read.csv2("C:\\Users\\IBM_ADMIN\\Documents\\Datascience\\gettingAndCleaningData\\UCI HAR Dataset\\train\\subject_train.txt",header=F,col.names=('subjects'),sep="",stringsAsFactors=F)
train<-cbind(train_subject,train_label,train_x)

test_train<-rbind(test,train)

activity_label<-read.csv2("C:\\Users\\IBM_ADMIN\\Documents\\Datascience\\gettingAndCleaningData\\UCI HAR Dataset\\activity_labels.txt",header=F,sep="",col.names=c("label","activity"))
test_train_with_label<-merge(x=activity_label,y=test_train,by="label")
list1<-split(test_train_with_label,test_train_with_label[,c("activity","subjects")])
list1<-lapply(list1,function(x)convert.characters.to.numeric(x[,-grep("subjects|label|activity",names(x))]))
summary<-lapply(list1,function(x)colMeans(x))
names(summary)<-sub("\\.","\tfor Subject ID:",names(summary),ignore.case=T)
summary<-rapply( summary, f=function(x) ifelse(is.nan(x),0,x), how="replace" )
summary<-as.data.frame(summary)
summary<-cbind(Features_asked_for=rownames(summary),summary)
rownames(summary)<-NULL
print(names(summary))
View(summary)
write.table(summary,"summary.txt",row.names = F)