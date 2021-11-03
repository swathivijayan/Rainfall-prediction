# ************************************************
# This work is the Course Work for Practical Business Analytics Module
# Topic : Australia Next Day Rain Prediction
# ************************************************
#
# Team: Tesseract
# MSc Data Science
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# ************************************************
#
# UPDATE
# 1.00      28/3/2021    Initial Version
#

# ************************************************

print("loaded Project_Function.R")

# ************************************************
# TreadDataset() :
#
# Read a CSV file from working directory
#
# INPUT: string - csvFilename - CSV filename
#
# OUTPUT : data frame - contents of the headed CSV file
# ************************************************

TreadDataset<-function(csvFilename){
  
  # Read the data from the csv file to a data frame
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  
  print(paste("CSV dataset",csvFilename,"has been read wit Number of Records=",nrow(dataset)))
  return(dataset)
  
}

# ************************************************
# findFieldTypes() :
#
# Test each field for NUMERIC or SYMBOLIC
#
# INPUT: Data Frame - dataset - data
#
# OUTPUT : Vector - Vector of types {NUMERIC, SYMBOLIC}
# ************************************************
findFieldTypes<-function(dataset){
  field_types<-vector()
  for(field in 1:(ncol(dataset))){
    
    if (is.numeric(dataset[,field])) {
      field_types[field]<-TYPE_NUMERIC
    }
    else{
      
       field_types[field]<-TYPE_SYMBOLIC
    }
  }
  
  
  numeric_fields<-names(dataset)[field_types=="NUMERIC"]
  
  
  symbolic_fields<-names(dataset)[field_types=="SYMBOLIC"]
  
  #Print symbolic fields to the console
  
  print("Symbolic Fields are:") 
  for (i in symbolic_fields){
    print(paste(i,sum(is.na(dataset[,i]))))
  }
  #Print Numerical fields to the console 
  print("")
  print("Numeric Fields are:")
  for (i in numeric_fields){
    print(paste(i,sum(is.na(dataset[,i]))))
  }
  return(field_types)
}

# ************************************************
# TimputeMissingData() :
#
# Clean the data by substituting Null values for 
# numerical data
#
# INPUT: data frame 
#
# OUTPUT : data frame 
#
# Requires the library: mice
#*************************************************
#*
TimputeMissingData<-function(dataset,numerical_fields){
 
  
  # Impute Numerical column in the dataset using MICE
  temp_data<-mice(dataset[,numerical_fields],m=2,maxit=3,meth='pmm',seed=500)
  dataset[,numerical_fields]<-complete(temp_data,1)
  return(dataset)
}


# ************************************************
# Tvisualize_Dataset()
# Referenced from Prof.Nick's Lab3.R
# Output simple dataset field analysis results as a table in "Viewer"
#
# REQUIRES: formattable
#
# INPUT: data frame    - dataset, full dataset used for train/test
#                      - Each row is one record, each column in named
#                      - Values are not scaled or encoded
#        String - OPTIONAL string which is used in table as a header
#
# OUTPUT : none
#
# Requires the library: PerformanceAnalytics
#                       formattable
# ************************************************
Tvisualize_Dataset<-function(dataset,...){
  
  params <- list(...)
  #print(params)
  
  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)
  
  if (length(params)>0){
    names(tidyTable)[1]<-params[1]
  }
  #print(names(tidyTable)[1])
  
  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }
  
  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}

# ************************************************
# Tdiscrete_ordinal_split() :
#
# Referenced from Prof.Nick's Lab3
#
# Split NUMERIC field to DISCRETE or ORDINAL
#
# INPUT: data frame      - dataset     - input data
#        vector strings  - field_types - Types per field, either {NUMERIC, SYMBOLIC}
#        int             - cutoff      - Number of empty bins needed to determine discrete (1-10)
#
# OUTPUT : vector strings - Updated with types per field {DISCRETE, ORDINAL}
# ************************************************
# Plots histogram for visulisation
# ************************************************
Tdiscrete_ordinal_split<-function(dataset,field_types,cutoff){
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {
      
      #use R hist() function to create 10 bins
      histogramAnalysis<-hist(dataset[,field], breaks = 10, plot=FALSE)
      bins<-histogramAnalysis$counts/length(dataset[,field])*100  # Convert to %
      
      graphTitle<-"AUTO:"
      
      #If the number of bins with less than 1% of the values is greater than the cutoff
      #then the field is deterimed to be a discrete value
      
      if (length(which(bins<1.0))>cutoff)
        field_types[field]<-TYPE_DISCRETE
      else
        field_types[field]<-TYPE_ORDINAL
      
      #Type of field is the chart name
      hist(dataset[,field], breaks = 10, plot=TRUE,
           main=paste(graphTitle,field_types[field]),
           xlab=names(dataset[field]),ylab="Number of Records",
           yaxs="i",xaxs="i",border = NA)
      
    } #endif numeric types
  } #endof for
  return(field_types)
}

# ************************************************
# TplotOutliers() :
#
# Scatter plot of field values and colour outliers in red
#
# INPUT: Vector - sorted    -  points to plot as literal values
#        Vector - outliers  - list of above points that are considered outliers
#        String - fieldName - name of field to plot
#
# OUTPUT : None
# ************************************************

TplotOutliers<-function(sorted,outliers,fieldName){
  
  boxplot(sorted,xlab="Outliers",ylab=paste(fieldName))
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}


# ************************************************
# remove_outliers() :
#
# Remove outlier with respect to IQR
#
# INPUT: dataframe - ordinals    -  dataframe with ordinal data
#        
# OUTPUT : dataframe - ordinals - dataframe with outliers removed
# ************************************************

remove_outliers <-function(ordinals){
  qnt <- quantile(ordinals, probs=c(.25, .75), na.rm = T)
  caps <- quantile(ordinals, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(ordinals, na.rm = T)
  ordinals[ordinals < (qnt[1] - H)] <- caps[1]
  ordinals[ordinals > (qnt[2] + H)] <- caps[2]
  return(ordinals)
 }



# ************************************************
# TordinalProcess_outlier() :
#
# Determine if a value of a record is an outlier for each field
#
# INPUT:   data frame - ordinals   - numeric fields only
#          double     - confidence - Confidence above which is determined an outlier [0,1]
#                                  - Set to negative Confidence if NOT remove outliers
#
# OUTPUT : data frame - ordinals with any outlier values replaced with IQR
# ************************************************
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdf


TordinalProcess_outlier<-function(ordinals,confidence){
  
  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){
    
    sorted_data<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted_data,type="chisq",prob=abs(confidence)))
    TplotOutliers(sorted_data,outliers,colnames(ordinals)[field])
    
    #If found records with outlier values
    if ((length(outliers>0))){
      
      # Replace outlier value with regarding to IQR
      if (confidence>0){
        ordinals[,field]<-remove_outliers(ordinals[,field])
        sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
        TplotOutliers(sorted,vector(),colnames(ordinals)[field])
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with IQR"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }
    
  }
  return(ordinals)
}


# ************************************************
# Tnormalise() :
# Referenced from prof.Nick's Lab3
#
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
# ************************************************
Tnormalise<-function(input){
  
  minv<-min(input)
  maxv<-max(input)
  return((input-minv)/(maxv-minv))
}

# ************************************************
# TNormaliseDataframe() :
# Referenced from prof.Nick's Lab3\
# scale the entire dataframe to [0.0,1.0]
#
# INPUT:   data frame - dataset - numeric data frame
#
# OUTPUT : data frame - dataframe with values between 0 and 1
# ************************************************
TNormaliseDataframe<-function(dataset){
  
  scaled<-sapply(as.data.frame(dataset),Tnormalise)
  return(scaled)
}

# ************************************************
# One_hot_encoding():
# Using one-hot encoding technique using the dummyVars function 
# in the caret package
#
# INPUT: data frame - dataset -containing symbolic and discrete fields
#
# OUTPUT : Frame - dataset with any symbolic fields encoded
# ************************************************
# Uses   library(caret)
# https://cran.r-project.org/web/packages/caret/index.html


Tonehot_Encoding <- function(dataset){
  
  # Number of symbolic fields before encoding
  nonNumericbefore<-length(dataset)
  
  dmy <- dummyVars(" ~ .", data = dataset)
  dataset_Onehot_encode <- data.frame(predict(dmy, newdata = dataset))
  
  nonNumericAfter<-length(dataset_Onehot_encode)
  # Display the difference in length of the symbolic fields before and after encoding
  print(paste("Symbolic fields. Before encoding=",nonNumericbefore,"After encoding=",nonNumericAfter))
 
  # Print the new field names after Encoding
  print(formattable::formattable(data.frame(field=1:nonNumericAfter,encoded=names(dataset_Onehot_encode))))
  
  return(dataset_Onehot_encode)
}


# ************************************************
# Tremove_redundantFields() :
#
# Referenced from Prof.Nicks lab4
#
# Determine if an entire field is redundant
# Uses LINEAR correlation,
#
# INPUT: data frame - dataset - numeric values only
#        double     - cutoff  - Value above which is determined redundant [0,1]
#
# OUTPUT : Frame - dataset with any fields removed
# ************************************************

Tremove_redundantFields<-function(dataset,fields,cutoff){
  
  print(paste("Before checking correlation count is =",ncol(dataset)))

  #Remove any fields that have a stdev of zero (i.e. they are all the same)
  xx<-which(apply(dataset, 2, function(x) sd(x, na.rm=TRUE))==0)+1
  
  if (length(xx)>0L)
    dataset<-dataset[,-xx]
  
  #Find the correlation between each variables in the dataset
  cr<-cor(dataset, use="everything")
  
  #cr[(which(cr<0))]<-0 #Positive correlation coefficients only
  #corr_simple(cr,cutoff)
  
  correlated<-which(abs(cr)>=cutoff,arr.ind = TRUE)
  correlated_fields<-correlated[which(correlated[,1]!=correlated[,2]),]

  #Plot correlation matrix for correlated fields
  TPLOT_correlFields(dataset,correlated_fields)
  #Check for cross corelation and remove corelated data
  if (nrow(correlated_fields)>0){
    
    print("Following fields are correlated")
    print(correlated_fields)
    
    # print names of the corelated fields
    for (i in 1:nrow(correlated_fields)){
      print(paste(names(dataset)[correlated_fields[i,1]],"~", names(dataset)[correlated_fields[i,2]]))
    }
    
    # Check corelation of one field with another and remove one of the field
    cor_vec<-vector()
    numcor<-nrow(correlated_fields)
    for (i in 1:numcor){
      if (length(which(correlated_fields[i,1]==correlated_fields[i:numcor,2]))==0) {
        cor_vec<-append(cor_vec,correlated_fields[i,1])
      }
    }
    print("Following fields are removed")
    print(names(dataset)[cor_vec])
    print(paste("After correlation count is=",ncol(dataset[,-cor_vec])))
    return(dataset[,-cor_vec]) #Remove the first field that is correlated with another
  }
  return(dataset)
}



# ************************************************
# TPLOT_correlFields() :
#
# Plots Correlation plot of closely correlated fields
#
# INPUT: data frame - dataset 
#        list       - correlated_fields - list of 
#                     closely correlated fields
# OUTPUT : None
# plot absolute values only
# ************************************************
TPLOT_correlFields<-function(dataset,correlated_fields){
  
  # get the dataset with closely correlated feature alone
  corelated_data<-dataset[,row.names(correlated_fields)]
  # find the corelation matrix
  cr<-cor(corelated_data,use = 'everything')
  
  corrplot::corrplot(abs(cr),method="square",
                     order="FPC",
                     cl.ratio=0.2,
                     cl.align="r",
                     tl.cex = 0.6,cl.cex = 0.6,
                     cl.lim = c(0, 1),
                     mar=c(1,1,1,1),bty="n",
                     addCoef.col = "black",
                     title = "Correlation Plot for Most Correlated Fields"
                     )
}

#########################

#LogisticFormula()

#Used to get the formula for passing into the regressor for modelling

# INPUT :  data frame  - data  - Dataset to be trained

# OUTPUT : language    -formula- Formula of input and output parameters

#################3
LogisticFormula<-function(data){
  
  inputs<-paste(names(data)[which(names(data)!=OUTPUT_FIELD)],collapse = "+")
  
  output<-paste(OUTPUT_FIELD,"~")
  
  formula<-as.formula(paste(output,inputs))
  print(typeof(formula))
  
  return (formula)
}

#*******************************************
#*TplotConfusion()
#
#Used to plot the confusion matrix of the model 

# INPUT: vector - expectedClass  - {0,1}, Expected outcome from each row (labels)
#        vector - predictedClass - {0,1}, Predicted outcome from each row (labels)

# OUTPUT : None
#*
#******************************************
TplotConfusion<- function(expectedClass,predictedClass){
  
  #predictedClass <- model %>% predict_classes(as.matrix(test_df))
  resulttab <- table(expectedClass,predictedClass)
  results <- confusionMatrix(resulttab)
  
  #Plot Results
  print(ggplot(as.data.frame(results$table), aes(predictedClass,expectedClass, fill=Freq)) +
          geom_tile() + geom_text(aes(label=Freq)) + ggtitle("Confusion Matrix") +
          theme(plot.title = element_text(size = 15, hjust=0.5)) +
          scale_fill_gradient(low="white", high="#009194") +
          labs(x = "Prediction",y = "Actual") +
          scale_x_discrete(labels=c("No Rain","Rain")) +
          scale_y_discrete(labels=c("No Rain","Rain")))
  
  #return(results)
}

# ************************************************
# NcalcMeasures() :
#
# Evaluation measures for a confusion matrix
#
# INPUT: numeric  - TP, FN, FP, TN
#
# OUTPUT: A list with the following entries:
#        TP        - double - True Positive records
#        FP        - double - False Positive records
#        TN        - double - True Negative records
#        FN        - double - False Negative records
#        accuracy  - double - accuracy measure
#        pgood     - double - precision for "good" (values are 1) measure
#        pbad      - double - precision for "bad" (values are 1) measure
#        FPR       - double - FPR measure
#        TPR       - double - FPR measure
#        TNR       - double - TNR measure
#        MCC       - double - Matthew's Correlation Coeficient
#
# 080819NRT added TNR measure
# 260221NRT rounded to sensible number of digits
# ************************************************
NcalcMeasures<-function(TP,FN,FP,TN){
  
  accuracy<-round(100.0*((TP+TN)/(TP+FP+FN+TN)), digits=2)
  pgood   <-round(100.0*(TP/(TP+FP)),digits=2)
  pbad    <-round(100.0*(TN/(FN+TN)),digits=2)
  fpr     <-round(100.0*(FP/(FP+TN)),digits=2)
  tpr     <-round(100.0*(TP/(TP+FN)),digits=2)
  tnr     <-round(100.0*(TN/(FP+TN)),digits=2)
  mcc     <-round( ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)),digits=3)
  
  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=accuracy,
                  "pgood"=   pgood,
                  "pbad"=    pbad,
                  "FPR"=     fpr,
                  "TPR"=     tpr,
                  "TNR"=     tnr,
                  "MCC"=     mcc
  )
  return(retList)
}

# ************************************************
# NprintMeasures()
# Function refered from Prof. Nick's Lab4
# Output measures to the Viewer
#
# INPUT:    list -   results - results from TcalcConfusion()
#           string - title   - title of the table
#
# OUTPUT :  NONE
#
# 070819NRT updated to output table to viewer only
# 171019NRT added column name "Metric"
# 241019NRT added title
# ************************************************
NprintMeasures<-function(results,title){
  
  #This outputs our results into the "Viewer" in RStudio
  tidyTable<-data.frame(t(t(results)))
  names(tidyTable)[1]<-title
  
  t<-formattable::formattable(tidyTable,list(
    TP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TP)),
    FN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FN)),
    TN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TN)),
    FP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FP))))
  print(t)
}


#*******************************************
#*TcalcConfusion()
#*
#*Referenced from Prof.Nick's Lab3.R
#*Calculate a confusion matrix for 2-class classifier
#*
# INPUT: vector - expectedClass  - {0,1}, Expected outcome from each row (labels)
#        vector - predictedClass - {0,1}, Predicted outcome from each row (labels)
#
#                    ACTUAL
#               ------------------
# PREDICTED     Rain=1   |  NoRain=0
#               ------------------
#     Rain=1      TP     |    FP
#               ==================
#     NoRain=0       FN     |    TN

# OUTPUT: A list with the  entries from NcalcMeasures()
#*
#******************************************
TcalcConfusion<-function(expectedClass,predictedClass,plot_conf=FALSE){
  
  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))
  
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  if (plot_conf==TRUE) {
    
    TplotConfusion(expectedClass,predictedClass)
  }
  
  
  return(NcalcMeasures(TP,FN,FP,TN))
  
} #endof TcalcConfusion()



#***************************************************
#*TLogisticRegression():
#*
#*Use Logistic Regression model to evaluate the accuracy.
#*
# INPUT   :   Data Frame        - training_data - Dataset to train
#             Data Frame        - testing_data  - Dataset to evaluate
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability and accuracy
#
#
#*************************************************

TLogisticRegression<-function(training_data,testing_data){
  # Calling the logistic formula
  build_formula<-LogisticFormula(training_data)
  
  # Building the logistic regression classifier
  logisticModel<-stats::glm(build_formula,data=training_data,family=quasibinomial)
  
  #Getting probabilities of being class 1 from the classifier
  #probabilities<-predict(logisticModel, testing_data,type="response")
  predicted <- plogis(predict(logisticModel, testing_data))
  
  # ************************************************
  #Evaluating the classifier on test dataset
  threshold<-0.7
  test_predicted<-as.numeric(predicted)
  test_expected<-testing_data[,OUTPUT_FIELD]
  
  #results<-myEvaluateClassifier(probs=predicted,
   #                             testing_data=testing_data,
    #                            threshold=threshold,plot_conf = TRUE)
  
  
  results<-NEvaluateClassifier(test_predicted,
                               test_expected,
                               threshold,plot_conf=FALSE)
  
  
  results$AUC<-auroc(score=test_predicted,bool=test_expected) # Estimate the AUC
  # This outputs our results into the "Viewer" in RStudio
  NprintMeasures(results,title="Value")
  
  # Plot FPR/TPR through threshold range
 # results<-myPerformancePlot(probs=predicted,testing_data=testing_data)
  results<-NdetermineThreshold(test_predicted=test_predicted,test_expected=test_expected)
  #NprintMeasures(results)
  return(results)
}




#***********************************************************************************************
# ************************************************
# NEvaluateClassifier() :
#
# Use dataset to generate predictions from model
# Evaluate as classifier using threshold value
#
# INPUT   :   vector double     - probs        - probability of being class 1
#             Data Frame        - testing_data - Dataset to evaluate
#             double            - threshold     -cutoff (probability) for classification
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# ************************************************
NEvaluateClassifier<-function(test_predicted,test_expected,threshold,plot_conf=FALSE) {
  
  predictedClass<-ifelse(test_predicted<threshold,0,1)
  
  results<-TcalcConfusion(expectedClass=test_expected,
                          predictedClass=predictedClass,plot_conf=plot_conf)
  
  return(results)
} #endof NEvaluateClassifier()


# ************************************************
# Nauroc() :
#
# Calculate the Area Under Curve (AUC) for ROC
#
# INPUT   :   vector double     - score            - probability of being class 1
#             vector double     - bool             - Expected class of 0 or 1
#
# OUTPUT  :   double   - AUC
#
# ************************************************
# By Miron Kursa https://mbq.me
# See https://stackoverflow.com/questions/4903092/calculate-auc-in-r

auroc <- function(score, bool) {
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}

# ************************************************
# NdetermineThreshold() :
#
# For the range of threholds [0,1] calculate a confusion matrix
# and classifier metrics.
# Deterime "best" threshold based on either distance or Youdan
# Plot threshold chart and ROC chart
#
# Plot the results
#
# INPUT   :   vector double  - test_predicted   - probability of being class 1
#         :   vector double  - test_expected    - dataset to evaluate
#         :   boolean        - plot             - TRUE=output charts
#         :   string         - title            - chart title
#
# OUTPUT  :   List       - Named evaluation measures from confusion matrix
#                        - Threshold at min Euclidean distance
#                        - AUC - area under the ROC curve
#                        - Predicted class probability
#
# 241019NRT - added plot flag and title for charts
# 311019NRT - added axis bound checks in abline plots
# 191020NRT - Updated to use own ROC plot & calculate AUC
# ************************************************
NdetermineThreshold<-function(test_predicted,
                              test_expected,
                              plot=TRUE,
                              title=""){
  toPlot<-data.frame()
  
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-NEvaluateClassifier(test_predicted=test_predicted,
                                 test_expected=test_expected,
                                 threshold=threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }
  
  # the Youden index is the vertical distance between the 45 degree line
  # and the point on the ROC curve.
  # Higher values of the Youden index are better than lower values.
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5082211/
  # Youdan = sensitivty + specificity -1
  #        = TPR + (1-FPR) -1
  
  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1
  
  # 121020NRT - max Youdan
  # use which.max() to return a single index to the higest value in the vector
  maxYoudan<-toPlot$x[which.max(toPlot$youdan)]
  
  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2)
  # To the top left (i.e. perfect classifier)
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2))
  
  # 121020NRT - Euclidean distance to "perfect" classifier (smallest the best)
  # use which.min() to return a single index to the lowest value in the vector
  minEuclidean<-toPlot$x[which.min(toPlot$distance)]
  
  # ************************************************
  # Plot threshold graph
  
  if (plot==TRUE){
    # Sensitivity (TPR)
    plot(toPlot$x,toPlot$tpr,
         xlim=c(0, 1), ylim=c(0, 100),
         type="l",lwd=3, col="blue",
         xlab="Threshold",
         ylab="%Rate",
         main=paste("Threshold Perfomance Classifier Model",title))
    
    # Plot the specificity (1-FPR)
    lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)
    
    # The point where specificity and sensitivity are the same
    crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
    
    if (!is.na(crosspoint)){
      if ((crosspoint<1) & (crosspoint>0))
        abline(v=crosspoint,col="red",lty=3,lwd=2)
    }
    
    # Plot the Euclidean distance to "perfect" classifier (smallest the best)
    lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)
    
    # Plot the min distance, as might be more (311019NRT check it is within range)
    if ((minEuclidean<1) & (minEuclidean>0))
      abline(v=minEuclidean,col="green",lty=3,lwd=2)
    
    # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
    lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
    
    if ((maxYoudan<1) & (maxYoudan>0))
      abline(v=maxYoudan,col="purple",lty=3,lwd=2)
    
    legend("bottom",c("TPR","1-FPR","Distance","Youdan"),col=c("blue","red","green","purple"),lty=1:2,lwd=2)
    text(x=0,y=50, adj = c(-0.2,2),cex=1,col="black",paste("THRESHOLDS:\nEuclidean=",minEuclidean,"\nYoudan=",maxYoudan))
    
    # ************************************************
    # 121020NRT ROC graph
    
    sensitivityROC<-toPlot$tpr[which.min(toPlot$distance)]
    specificityROC<-100-toPlot$fpr[which.min(toPlot$distance)]
    auc<-auroc(score=test_predicted,bool=test_expected) # Estimate the AUC
    
    # Set origin point for plotting
    toPlot<-rbind(toPlot,data.frame(x=0,fpr=0,tpr=0, youdan=0,distance=0))
    
    plot(100-toPlot$fpr,toPlot$tpr,type="l",lwd=3, col="black",
         main=paste("ROC:",title),
         xlab="Specificity (1-FPR) %",
         ylab="Sensitivity (TPR) %",
         xlim=c(100,0),
         ylim=c(0,100)
    )
    
    axis(1, seq(0.0,100,10))
    axis(2, seq(0.0,100,10))
    
    #Add crosshairs to the graph
    abline(h=sensitivityROC,col="red",lty=3,lwd=2)
    abline(v=specificityROC,col="red",lty=3,lwd=2)
    
    annotate<-paste("Threshold: ",round(minEuclidean,digits=4L),
                    "\nTPR: ",round(sensitivityROC,digits=2L),
                    "%\n1-FPR: ",round(specificityROC,digits=2L),
                    "%\nAUC: ",round(auc,digits=2L),sep="")
    
    text(x=specificityROC, y=sensitivityROC, adj = c(-0.2,1.2),cex=1, col="red",annotate)
    
  } # endof if plotting
  
  # Select the threshold - I have choosen distance
  
  myThreshold<-minEuclidean      # Min Distance should be the same as analysis["threshold"]
  
  #Use the "best" distance threshold to evaluate classifier
  results<-NEvaluateClassifier(test_predicted=test_predicted,
                               test_expected=test_expected,
                               threshold=myThreshold,
                               plot_conf = TRUE)
  
  results$threshold<-myThreshold
  results$AUC<-auroc(score=test_predicted,bool=test_expected) # Estimate the AUC
  
  return(results)
} #endof myPerformancePlot()

# ************************************************
# NprintDTRules() :
#
# INPUT: text - filename
#
# OUTPUT : Frame - dataset
# ************************************************
NprintDTRules<-function(dtrules, filename){
  
  sink(filename)
  
  for (r in 1:nrow(dtrules)){
    print(paste("[",r,"]",dtrules$Rule[r],"==",(ifelse(dtrules$Class[r]==0,"No Rain","Rain"))))
  }
  sink()
}
# ************************************************
# DECISION TREE CONVERT DT RULES TO ASCII FORMATTED RULES
#
# <anticedent 1> AND <anticedent 2> ...
# Each anticedent is: [field][comparision][value]
#
# INPUT: Object - tree - Trained tree
#
# OUTPUT: data frame of rules, class and anticedents
# ************************************************
NDT5RuleOutput<-function(tree){
  #library(stringr)
  x<-summary(tree)[1]
  x<-substr(x,regexpr("Rules:",x)[1]+8,nchar(x))
  x<-substr(x,1,regexpr("Evaluation on training data",x)[1]-1)
  x<-gsub("[\n\t]", "*", x)
  df_of_rules<-data.frame(matrix(ncol=3,nrow=tree$size),stringsAsFactors = FALSE)
  df_of_rules<-setNames(df_of_rules,c("Rule","Class","Anti"))
  
  numberofrules<-tree$size
  # 271019 allow for multiple trees (i.e. boosted)
  if (length(numberofrules)>1){
    numberofrules<-numberofrules[1]
    warning("Prof Nick says: More than one tree found. Extracting rules for just the first")
  }
  
  totalAnticedents<-0
  for (ruleNumber in 1:numberofrules){
    start<-regexpr("\\*\\*",x)[1]+2
    end<-regexpr("->",x)[1]-3
    onerule<-substr(x,start,end) #Single rule, anticedents seperated by '**'
    onerule<-gsub("\\*\\*"," AND ",onerule) #Rule now has "AND" between anticedents
    #onerule<-convertNormalisedDTRuleToRealWorld(onerule)
    NumAnticedents<-str_count(onerule,"AND")+1
    totalAnticedents=totalAnticedents+NumAnticedents
    classpos<-regexpr("class ",x)+6
    classID<-as.numeric(substr(x,classpos,classpos))  #This has the class of the rule, i.e. {0,1}
    df_of_rules$Rule[ruleNumber]<-onerule
    df_of_rules$Class[ruleNumber]<-ifelse(classID==0,"No Rain","Rain") # Convert class to label
    df_of_rules$Anti[ruleNumber]<-NumAnticedents
    x<-substr(x,classpos,nchar(x))
    st<-regexpr("\\*\\*",x)[1]+2 #move past the rule ID
    x<-substr(x,st,nchar(x))
  }
  return(df_of_rules)
}
