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

#  clears all objects in "global environment"
rm(list=ls())

# ************************************************
# Global Environment variables
# - i.e. available to all functions

# ************************************************
# Constants are placed in the variable at the beginning of R code
# We use UPPERCASE to identify these in our code


DATASET_FILENAME  <- "weatherAUS.csv"          # Name of input dataset file
OUTPUT_FIELD      <- "RainTomorrow"           # Field name of the output class to predict

HOLDOUT           <- 70                   # % split to create TRAIN dataset

SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage
OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection
CORR_CUTOFF       <- 0.90
# Set to negative means analyze but do not replace outliers

TYPE_DISCRETE     <- "DISCRETE"           # field is discrete (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

DISCRETE_BINS     <- 60                 # Number of empty bins to determine discrete
MAX_LITERALS      <- 200               # Maximum number of 1-hot ecoding new fields

PDF_FILENAME      <- "tree.pdf"           # Name of PDF with graphical tree diagram
RULES_FILENAME    <- "rules.txt"          # Name of text file with rules saved
RESULTS_FILENAME  <- "results.csv"        # Name of the CSV results file
KFOLDS          <- 2   

NODE_LEVEL        <- 1                    # The number is the node level of the tree to print
BOOST             <- 20                   # Number of boosting iterations. 1=single model
FOREST_SIZE       <- 100                 # Number of trees in the forest
SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage



# Define and load libraries used in this project

# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.53
# formattable 	         0.2.0.1
# stats                  4.0.3
# PerformanceAnalytics   2.0.4
# stringr                1.4.0
# partykit               1.2.8
# C50                    0.1.3.1
# randomForest           4.6.14
# h2o                    3.32.0.4
# keras                  2.3.0.0




MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "PerformanceAnalytics",
               "C50",
               "mice",
               "VIM",
               "dplyr",
               "stringr",
               "partykit",
               "randomForest",
               "keras",
               "h2o",
               "caret")
# User defined functions are next

# ************************************************
# dataCleaning() :
#
# Create formula for clean the input datset
#
# INPUT   :   Data frame - dataset  - weatherAUS dataset
#         :   Char       - numerical_fields-field type numeric
#             Char       - symbolic_fields -field type symbolic
#
# OUTPUT  :   Dataframe - dataset-dataset after NA values removed
#
# ************************************************
dataCleaning<-function(dataset,numerical_fields,symbolic_fields){
  

  #There are a lot of NA values in both Symbolic and Numeric fields 
  # Display percentage of missing data in each column 
  pmiss<- function(x){sum(is.na(x))/length(x)*100}
  print('Percentage of NA in each fields before data cleaning')
  print(apply(dataset,2,pmiss))
  
  #Symbolic fields that have NA values are the fields related to Wind
  #replace NA values with 'No Wind' for symbolic fields
 
   for (field in symbolic_fields) {
    if (sum(is.na(dataset[,field]))>0) {
      dataset[,field][which(is.na(dataset[,field]))]<-'No Wind'
    }
    
   }
  
  # Imnpute NA values of numeric data using mice method
  dataset<-TimputeMissingData(dataset,numerical_fields)
  
  # Display percentage of missing data in each column after cleaning
  pmiss<- function(x){sum(is.na(x))/length(x)*100}
  print('Percentage of NA values after data cleaning')
  print(apply(dataset,2,pmiss))
  
  return(dataset)
  
}


# ************************************************
# process_ordinals() :
#
# Create formula for clean the input datset
#
# INPUT   :   Data frame - dataset  - Cleaned dataset after NA removal
#         :   Char       -field_type- Field types checked
#
# OUTPUT  :   Dataframe - dataset -dataset after outlier replacement
#
# ************************************************
process_ordinals<-function(dataset,field_types){
  
  ordinal_data<-dataset[,which(field_types==TYPE_ORDINAL)] 
  
  # Determine if any ordinals are outliers and replace with IQR
  # Null hypothesis is there are no outliers
  ordinal_data<-TordinalProcess_outlier(ordinals=ordinal_data,confidence=OUTLIER_CONF)
  
  #centers/scale the columns of the ordinal data
  scaled_ordinals<-as.data.frame(scale(ordinal_data,center=TRUE, scale=TRUE))
  
  #Normalise the dataframe to values between 0 and 1
  #final ordinal fields with values between 0 and 1
  ordinals_final<-TNormaliseDataframe(scaled_ordinals)
  
  return(ordinals_final)
  
}









# ************************************************
# allocateFoldID() :
#Referenced from Prof.Nick's Lab4.R
# 
# Append a column called "foldID" that indicates the fold number
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
allocateFoldID<-function(dataset){
  recordsPerFold<-ceiling(nrow(dataset)/KFOLDS)
  
  foldIds<-rep(seq(1:KFOLDS),recordsPerFold)
  
  foldIds<-foldIds[1:nrow(dataset)]
  
  dataset$foldId<-foldIds
  
  return(dataset)
} #endof allocateFoldID()





# ************************************************
# stratifiedDataset() :

##Referenced from Prof.Nick's Lab4.R

# Split dataset by the class (assume 2-class)
# Calculate the number of records that will appear in each fold
# Give each of these blocks a unique foldID
# combine the datasets & randomise
# The dataset now has a foldID from which we can select the data
# for the experiments
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
stratifiedDataset<-function(originalDataset){
  
  positionClassOutput=which(names(originalDataset)==OUTPUT_FIELD)
  
  # Get the unique class values
  classes<-unique(originalDataset[,positionClassOutput])
  
  # Split dataset into the two classes (so as to keep the class balance the same in the datasets)
  indexClass1<-which(originalDataset[,positionClassOutput]==classes[1])
  split1<-originalDataset[indexClass1,]
  split2<-originalDataset[-indexClass1,]
  
  # Append a column that indicates the fold number for each class
  split1<-allocateFoldID(split1)
  split2<-allocateFoldID(split2)
  
  # Combine the two datasets
  
  newDataset<-rbind(split1,split2)
  
  #Randomise the classes
  newDataset<-newDataset[order(runif(nrow(newDataset))),]
  
  return(newDataset)
}


# ************************************************
# stratifiedSplit() :
# Referenced from Prof.Nick's Lab4.R
# Generate the TRAIN and TEST dataset based on the current fold
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   list               - train & test datasets
# ************************************************

stratifiedSplit<-function(newDataset,fold){
  
  test<-subset(newDataset, subset= foldId==fold, select=-foldId)
  train<-subset(newDataset, subset= foldId!=fold,select=-foldId)
  
  return(list(
    train=train,
    test=test))
}


# ************************************************
# Log_Reg_Model() :
# Referenced from Prof.Nick's Lab4.R
#
# INPUT   :   data frame         - dataset        - dataset
#             object function    - FUN            - name of function
#             ...                - optional        - parameters are passed on
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
Log_Reg_Model<-function(dataset){
  
  allResults<-data.frame()
  
  for (k in 1:KFOLDS){
    
    print(paste("Processing fold #",k))
    
    splitData<-stratifiedSplit(newDataset=dataset,fold=k)
  
    measures<-TLogisticRegression(training_data = splitData$train,
                        testing_data = splitData$test)
    
    allResults<-rbind(allResults,data.frame(measures))
  } #endof for()
  
  # Return the means from all the experiments back as a list
  # round metrics to 2 decimal places
  getMeans<-round(colMeans(allResults),digits=2)
  
  getMeans[1:4]<-as.integer(getMeans[1:4])  # TP, FN, TN, FP are rounded to ints
  
  
  #290520NRT return the above with the rounded values
  return(as.list(getMeans))
} #endof runExperiment()



#*****************************************************************************************************

# ************************************************
# runExperiment() :
#
#
# INPUT   :   data frame         - dataset        - dataset
#             object function    - FUN            - name of function
#             ...                - optional        - parameters are passed on
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
runExperiment<-function(dataset,FUN, ...){
  
  allResults<-data.frame()
  
  for (k in 1:KFOLDS){
    
    print(paste("Processing fold #",k))
    
    splitData<-stratifiedSplit(newDataset=dataset,fold=k)
    
    measures<-FUN(train=splitData$train,
                  test=splitData$test,
                  plot=(k==KFOLDS),...)
    
    allResults<-rbind(allResults,data.frame(measures))
  } #endof for()
  
  # Return the means from all the experiments back as a list
  # 260221NRT round metrics to 2 decimal places
  getMeans<-round(colMeans(allResults),digits=2)
  
  getMeans[1:4]<-as.integer(getMeans[1:4])  # TP, FN, TN, FP are rounded to ints
  
  
  #290520NRT return the above with the rounded values
  return(as.list(getMeans))
} #endof runExperiment()

# ************************************************
# getTreeClassifications() :
#
# Put in test dataset and get out class predictions of the decision tree
# Determine the threshold, plot the results and calculate metrics
#
# INPUT   :   object         - myTree        - tree
#         :   Data Frame     - testDataset - dataset to evaluate
#         :   string         - title        - string to plot as the chart title
#         :   int            - classLabel   - lable given to the positive (TRUE) class
#         :   boolean        - plot         - TRUE to output results/charts
#
# OUTPUT  :   List       - Named evaluation measures
#
# ************************************************
getTreeClassifications<-function(myTree,
                                 testDataset,
                                 title,
                                 classLabel=1,
                                 plot=TRUE){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (No rain) and column 2 is for class 1 (Rain)
  
  testPredictedClassProbs<-predict(myTree,test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)

  # Get the probabilities for classifying rainfall
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)
  
  if (plot==TRUE)
    NprintMeasures(results=measures,title=title)
  
  return(measures)
} #endof getTreeClassifications()



# ************************************************
# fullDT() :
#
# Create C5 Decision Tree on pre-processed dataset
#
# INPUT   :
#             Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             int            - boost       - number of trees to boost
#             boolean        - plot        - TRUE = plot charts
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
fullDT<-function(train,test,boost=1,plot=TRUE){
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  # ************************************************
  # Create a standard Decision Tree using the C5.0 algorithm
  # Uses library C50
  # Outputs the tree in the format of rules
  
  myTitle<-"Preprocessed Dataset. DT C5.0"
  if (boost>1)
    myTitle<-paste(myTitle,"BOOSTED=",boost)
  
  print(myTitle)
  
  tree<-C50::C5.0(x=train_inputs,
                  factor(train_expected),
                  rules=TRUE,
                  trials=boost)
  
  # Use the created decision tree with the test dataset
  # to determine best classification threshold & calculate metrics
  measures<-getTreeClassifications(myTree = tree,
                                   testDataset = test,
                                   title=myTitle,
                                   plot=plot)
  
  if (plot==TRUE){
    
    print(summary(tree))
    
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]
    
    print(formattable::formattable(importance))
    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main=myTitle)
    
    # ************************************************
    # We can visualise the tree
    
    #Function to output the tree as rules to a file
    dftreerules<-NDT5RuleOutput(tree)
    print(formattable::formattable(dftreerules))
    
    NprintDTRules(dftreerules, paste(myTitle, RULES_FILENAME))
    
    # ************************************************
    # Creates C5.0 decision tree & plot as a tree structure
    # The "partykit" library requires the variables (wrongly) to be global
    print("Plot decision tree to file called tree.pdf")
    
    Global_train_inputs<<-train_inputs
    Global_train_expected<<-train_expected
    
    # :: is used to specify a function within the named package to avoid confusion
    tree<-C50::C5.0(x=Global_train_inputs,
                    factor(Global_train_expected),
                    trials=boost)
    
    # ::: is used to directly access a member of a package that is internal
    graphtree<-C50:::as.party.C5.0(tree)
    
    # The plot is large - so print to a big PDF file
    pdf(PDF_FILENAME, width=100, height=50, paper="special", onefile=F)
    
    # The number is the node level of the tree to print
    plot(graphtree[NODE_LEVEL])
    
    #This closes the PDF file
    dev.off()
    
  }
  return(measures)
} #endof fullDT()



# ************************************************
# randomForest() :
#
# Create Random Forest on pre-processed dataset
#
# INPUT   :
#         :   Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
randomForest<-function(train,test,plot=TRUE){
  
  myTitle<-(paste("Preprocessed Dataset. Random Forest=",FOREST_SIZE,"trees"))
  print(myTitle)
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  rf<-randomForest::randomForest(train_inputs,
                                 factor(train_expected),
                                 ntree=FOREST_SIZE ,
                                 importance=TRUE,
                                 mtry=sqrt(ncol(train_inputs)))
  
  
  # ************************************************
  # Use the created decision tree with the test dataset
  measures<-getTreeClassifications(myTree = rf,
                                   testDataset = test,
                                   title=myTitle,
                                   plot=plot)
  
  if (plot==TRUE){
    # Get importance of the input fields
    importance<-randomForest::importance(rf,scale=TRUE,type=1)
    importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
    
    colnames(importance)<-"Strength"
    
    barplot(t(importance),las=2, border = 0,
            cex.names =0.7,
            main=myTitle)
    
    print(formattable::formattable(data.frame(importance)))
  }
  
  return(measures)
} #endof randomForest()




#********************************************************************************************






# ************************************************
# main() :
#
# Entry point to execute our Project
#
# INPUT:  None
#
# OUTPUT :None
# ************************************************

main<-function(){
  # Read the data using the readDataset function
  weatherAUS<-TreadDataset(DATASET_FILENAME)
  
  weatherAUS$Date<-as.Date(weatherAUS$Date) # fix date otherwise it will read as character
  
  # Assign 0 and 1 values to the field RainToday and RainTommorrow
  # instead of yes and No
  weatherAUS <- weatherAUS%>%dplyr::mutate(RainToday = ifelse(RainToday == "No",0,1))
  weatherAUS <- weatherAUS%>%dplyr::mutate(RainTomorrow = ifelse(RainTomorrow == "No",0,1))
  
  # Determine Numerical and symbolic field data in the dataset 
  field_types<- findFieldTypes(weatherAUS)
  numerical_fields<-names(weatherAUS)[field_types=="NUMERIC"]
  symbolic_fields<-names(weatherAUS)[field_types=="SYMBOLIC"]
  
  # Clean the dataset by removing NA fields an
  weatherAUS<-dataCleaning(weatherAUS,numerical_fields,symbolic_fields)
  
  # Summarise the datatable after cleaning in an attractive way  
  Tvisualize_Dataset(weatherAUS)
  
  # Split the numerical fields to discrete and ordinal
  fieldtypes_final<-Tdiscrete_ordinal_split(dataset=weatherAUS,
                                               field_types=field_types,
                                               cutoff=DISCRETE_BINS)
 
  fields_table<-data.frame(Column_Names=names(weatherAUS),Initial_Field_Type=field_types,Final_Field_Type=fieldtypes_final)
  print(formattable::formattable(fields_table))
  
  # Process ordinal fields by finding and removing outliers, and scale them
  ordinals_finals<-process_ordinals(weatherAUS,fieldtypes_final)
  
  #Process the categorical fields to fit the machine learning algoithm
  #Process fields containing symbolic and discrete fields
  categorical_fields<-names(weatherAUS)[which(fieldtypes_final==TYPE_SYMBOLIC | fieldtypes_final==TYPE_DISCRETE)]
  categorical_data<-weatherAUS[,categorical_fields]
  #Process the categorical (symbolic/discrete) fields using 1-hot-encoding
  categorical_final<-Tonehot_Encoding(dataset=categorical_data)
  
  dataset_final<-cbind(ordinals_finals,categorical_final)
  
  dataset_final<-Tremove_redundantFields(dataset=dataset_final,fields=fieldtypes_final,cutoff=CORR_CUTOFF)
  
  

  
  #stratified cross-validation
  allResults<-NULL
  
  dataset<-stratifiedDataset(dataset_final)
  
  #Logistic Regression
  measures<-Log_Reg_Model(dataset = dataset)
  allResults<-data.frame(DT_logReg=unlist(measures))
  print(allResults)
  
  # ************************************************
  #C5 decision tree and preprocessed dataset
  
  measures<-runExperiment(dataset = dataset,FUN = fullDT)
  allResults<-cbind(allResults,data.frame(DT_preprocess=unlist(measures)))
  print(allResults)
  
  
  
  #Boosted Decision tree
  measures<-runExperiment(dataset = dataset,FUN = fullDT,boost=BOOST)
  # Keep a note of all our results - append to the all results
  allResults<-cbind(allResults,data.frame(DT_boost=unlist(measures)))
  print(allResults)
  
  # ************************************************
  #Random forest model
  
  measures<-runExperiment(dataset = dataset,FUN = randomForest)
  
  # Keep a note of all our results - append to the all results
  allResults<-cbind(allResults,data.frame(RandomForest=unlist(measures)))
  
 
  
  allResults<-data.frame(t(allResults))
  
  # Output results to compare all classifiers
  allResults[,1:4]<-sapply(allResults[,1:4], as.integer)
  allResults$folds<-KFOLDS
  print(formattable::formattable(allResults))
  
  #print(measures)
  
 
  
  
  
  
  # Write frame to a CSV files
  write.csv(allResults,file=RESULTS_FILENAME)
  
  
  
  #******************************************************************************************
  
  
  print("pause")
  # Clear all warning messages
  assign("last.warning", NULL, envir = baseenv())
} #endof main()

# ************************************************
# R program execution starts here

gc() # Release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# Clear all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")

print("Start Australia Rain Prediction")

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Load additional R script files provide for this lab
source("Project_Function.R")

set.seed(123)

# ************************************************
main()

print("end")