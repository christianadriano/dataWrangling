## Remove lines with duplicated entries in a file

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

#Remove lines if we know the name of the column
remLinesDataFrame <- function(dataframe,dataframeColumn,value){
  #dataframe<-dataframe[!(dataframe$columnName==value) ,]
  dataframe<-dataframe[!(dataframeColumn==value) ,];
  return(dataframe);
}

#Remove lines for which the difference between two columns is above the 
removeLinesColDiffSmallerThanValue <- function (dataframe, columNumber1,columnNumber2,value){
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

#Utility functions
#################################################################################                                                                

# Function counts the number of words in a column of the data frame.
#Words are provided in a vector.
countWordsInDataFrameColumn<- function(wordVector,dataframe,columNumber){
  resultVector <-is.na(c(1:length(dataframe[,columNumber])))
  for (word in wordVector) {
    count <- countWords(word,dataframe,columNumber)
    resultVector <- resultVector + count;
  }
  return(resultVector);
}

# Function counts the number of words in a vector 
#Words are provided in a vector.
countWordsInVector<- function(wordVector,vector){
  resultVector <-is.na(c(1:length(vector)))
  for (word in wordVector) {
    count <- countWords(word,vector);
    resultVector <- resultVector + count;
  }
  return(resultVector);
}

# Function counts the number of words in a column of the data frame
countWords <- function(word, vector){
  booleanVector <- grepl(word, vector);
  return(booleanVector*1);
}

# Function counts the proportion of numbers in a vector
countNumbers<- function(vector){
  tab <- table(vector);
  df <- as.data.frame(tab);
  total<-sum(df$Freq);
  percent<-(df$Freq/total)*100
  return (cbind(df,percent));
}


##Remove empty columns
removeEmptyElements <- function(vector){
  columns <- which(vector[]=="");
  vector <- vector[-columns];
}


##################################################################################


