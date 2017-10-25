Data import and preparation
---------------------------

In the following section the data was prepared for the machine learning
process. The 20 observations for the final quizz evaluation were stored
in a dataframe labeled "final\_test", while the rest of the data was
stored in the variable "training". Finally, columns where all entries
were missing values (NA) were removed from the "trainig" DF.

    suppressPackageStartupMessages(suppressWarnings(library(caret))) 
    suppressPackageStartupMessages(suppressWarnings(library(data.table))) 
    suppressPackageStartupMessages(suppressWarnings(library(dtplyr))) 
    suppressPackageStartupMessages(suppressWarnings(library(dplyr))) 
    suppressPackageStartupMessages(suppressWarnings(library(tidyr))) 

    # Data import and setup ---------------------------------------------------

    #Download of training file
        #download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml_training.csv","pml_training") 
        training <- read.csv("C:\\Users\\Hp\\Documents\\pml_training",
                             na.strings = c("","NA","#DIV/0!"))
    #Download of testing file
        #download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml_testing.csv","pml_test")
        final_test <- read.csv("C:\\Users\\Hp\\Documents\\pml_test",
            na.strings = c("","NA","#DIV/0!"))
        #NOTE: 

    # Data cleaning -----------------------------------------------------------
    #Remove unnecessary columns
        # Convert to data.table
        training <- training %>% tbl_dt()
        training_raw <- copy(training)
        training <- copy(training_raw)
        final_test <- final_test %>% tbl_dt()
        
        # Inspect missing values
        #sapply(training, function(x) mean(!is.na(x))) %>% sort()
        
        #Remove counter and "all-zero" column
        training <- training %>% 
            dplyr::select(-X, -kurtosis_yaw_belt, -cvtd_timestamp,
                          -skewness_yaw_belt, -kurtosis_yaw_dumbbell,
                            -skewness_yaw_dumbbell, -kurtosis_yaw_forearm, -skewness_yaw_forearm)

Data partition
--------------

A test set with 20% of the observations (disting from the 20-observation
"final\_test" dataframe) was built by taking 3923 rows at random out of
the training set. The new "training" variable now contains therefore,
3923 less observations.

    # Subdivide training set ------------------------------------------------
        # Create data partition in order to estimate out-of-sample error
        index <- createDataPartition(training$classe, p = 0.8, list = F)
        test <- training[-index]
        training <- training[index]

Model building and assessment
-----------------------------

The training set was preprocessed through the removal of the
zero-variance ("zv") and nea-zero-variance ("nzv") columns. After that,
the median value for each column was imputed in rows where values were
missing ("medianImpute""). Finally, the principal components were
obtained from the dataset ("pca").

The ML model was built using a *randomforest*. The cross-validation
method selected was a 3-fold CV, as indicated in train\_control.

    # Model -------------------------------------------------------------------
        # PreProcess the data
        preProcess_model <- preProcess(x = training, 
                                       method = c("zv","nzv","medianImpute", "pca"))
        prep_training <- predict(preProcess_model, training)    
        browser()

    ## Called from: eval(expr, envir, enclos)

        # Apply Cross-Validation
        train_control <- trainControl(method = "cv", number = 3)
    #Initial basic model
        model <- train(classe~.,data = prep_training,method="rf", 
                       trainControl = train_control)

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    # Model assessment
    (train_asessment <- confusionMatrix(training$classe, predict(model, prep_training)))

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 4464    0    0    0    0
    ##          B    0 3038    0    0    0
    ##          C    0    0 2738    0    0
    ##          D    0    0    0 2573    0
    ##          E    0    0    0    0 2886
    ## 
    ## Overall Statistics
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9998, 1)
    ##     No Information Rate : 0.2843     
    ##     P-Value [Acc > NIR] : < 2.2e-16  
    ##                                      
    ##                   Kappa : 1          
    ##  Mcnemar's Test P-Value : NA         
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1838
    ## Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1838
    ## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000

    prep_test <- predict(preProcess_model, test)
    (test_asessment <- confusionMatrix(test$classe, predict(model, prep_test)))

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1102    5    3    3    3
    ##          B   10  745    2    0    2
    ##          C    4    8  666    4    2
    ##          D    1    2   18  618    4
    ##          E    0    1    4    3  713
    ## 
    ## Overall Statistics
    ##                                         
    ##                Accuracy : 0.9799        
    ##                  95% CI : (0.975, 0.984)
    ##     No Information Rate : 0.2847        
    ##     P-Value [Acc > NIR] : <2e-16        
    ##                                         
    ##                   Kappa : 0.9745        
    ##  Mcnemar's Test P-Value : 0.0181        
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9866   0.9790   0.9610   0.9841   0.9848
    ## Specificity            0.9950   0.9956   0.9944   0.9924   0.9975
    ## Pos Pred Value         0.9875   0.9816   0.9737   0.9611   0.9889
    ## Neg Pred Value         0.9947   0.9949   0.9917   0.9970   0.9966
    ## Prevalence             0.2847   0.1940   0.1767   0.1601   0.1846
    ## Detection Rate         0.2809   0.1899   0.1698   0.1575   0.1817
    ## Detection Prevalence   0.2845   0.1935   0.1744   0.1639   0.1838
    ## Balanced Accuracy      0.9908   0.9873   0.9777   0.9882   0.9912

In this case, the training set accuracy and the test set accurracy (i.e.
the out-of sample accuracy estimate) were calculated in the tables
above.
