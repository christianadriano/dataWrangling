## Utility functions

#Replace values
replaceValues <- function (dataframe,currentValue,newValue){
  
  dataframe <- lapply(dataframe, function(x){
                      gsub(currentValue,newValue,x)
  })
}

#Remove all lines from the dataframe for which a
#provided column matches the value
removeLinesWithValue <- function (dataframe, columNumber, value){
  lines <- which(dataframe[,columNumber]==value)
  dataframe <- dataframe[-lines,]
}

#Remove all lines from the dataframe for which a
#provided column has content larger the provided the value
removeLinesAboveValue <- function (dataframe, columNumber, value){
  lines <- which(dataframe[,columNumber]>value)
  dataframe <- dataframe[-lines,]
}

#Remove lines if we now the name of the column
dataframe<-dataframe[!(dataframe$columnName==value) ,]

#Remove lines for which the difference between two columns is above the 
removeLinesColDiffSmallerThanValue <- function (dataframe, columNumber1,columnNumber2,){
  diff = as.vector(dataframe[,columNumber1]) - as.vector(dataframe[,columnNumber2]);
  lines <- which(diff<value)
  dataframe<- dataframe[-lines,]
}

#Test data
column1<- c("prof","hobby")
column2<- c(15,10)
column3<- c(25,31)

df<- data.frame(column1,column2,column3)
names<-c("worker.profession","worker.yoe","age")
colnames (df) <- names
df$worker.yoe <- lapply(df$worker.yoe,function(x){as.numeric(as.character(x))})
df$age <- lapply(df$age,function(x){as.numeric(as.character(x))})



