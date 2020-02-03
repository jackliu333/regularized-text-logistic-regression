process.data <- function(current_repre,current_thres){
  library(tm)
  Sys.setlocale("LC_ALL", "English")
  
  #Read raw data
  data <- read.csv("Restaurant TripAdvisor Review Raw Data.csv", 
                   stringsAsFactors = FALSE)
  
  #Correct naming
  data$Restaurant <- ifelse(substr(data$Restaurant,1,2) == "db",
                            "DB Bistro & Oyster Bar", data$Restaurant)
  data$Restaurant <- ifelse(data$Restaurant == "RISE", "Rise", data$Restaurant)
  cat(paste("\nUnique restaurants:\n"))
  cat(paste(unique(data$Restaurant),"\n"))
  
  #Column formatting
  data$Restaurant <- as.factor(data$Restaurant)
  data$Rating.Score <- as.numeric(substr(data$Rating.Score,1,1))
  cat("\nScore frequency:\n")
  cat(table(data$Rating.Score))
  data$Sentiment <- ifelse(data$Rating.Score > 3, "positive",
                           ifelse(data$Rating.Score < 3, "negative","none"))
  cat("\nSentiment frequency in raw data:\n")
  cat(table(data$Sentiment))
  data <- data[data$Sentiment != "none",]
  data$Sentiment <- as.factor(data$Sentiment)
  data <- data[!is.na(data$Sentiment),]
  data <- data[,c("Date","Review","Sentiment","Rating.Score")]
  data$Date <- as.Date(data$Date,"%d/%m/%Y")
  cat("\nSentiment frequency after processing:\n")
  cat(table(data$Sentiment))
  cat("\nDataframe dimension:\n")
  cat(dim(data))
  #Add length - number of words in review
  data$ReviewLength <- sapply(gregexpr("\\W+", data$Review), length) + 1
  cat("\nSummary of review length:\n")
  cat(summary(data$ReviewLength))
  #Remove unmeaningful reviews
  cat("\n=============== Remove unmeaningful reviews ============\n")
  data <- data[data$ReviewLength > 5,]
  #Standardize review length
  head(data$ReviewLength)
  data$ReviewLength <- (data$ReviewLength-mean(data$ReviewLength))/sd(data$ReviewLength)
  head(data$ReviewLength)
  #View(data[order(data$ReviewLength),])
  temp.backup <- data
  data <- data[,c("Date","Review","Sentiment","ReviewLength")]
  cat("\nDataframe dimension:\n")
  cat(dim(data))
  cat("\nSummary of review length:\n")
  cat(summary(data$ReviewLength))
  backupData <- data
  
  #Build corpus
  corpus <- Corpus(VectorSource(data$Review))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  #Build DTM
  if(current_repre == "FF"){
    dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))
  } else if(current_repre == "FP"){
    dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightBin))
  } else{
    dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  }
  
  #Set sparsity threshold
  dtm <- removeSparseTerms(dtm, current_thres)
  # dtm

  #Build dataset with features
  important_words_df <- as.data.frame(as.matrix(dtm))
  colnames(important_words_df) <- make.names(colnames(important_words_df))
  important_words_df <- important_words_df[,!duplicated(colnames(important_words_df))]
  #Adjust feature names by keeping those without special characters inside
  important_words_df <- important_words_df[,!grepl('[^[:alnum:]]', colnames(important_words_df))]
  important_words_df <- important_words_df[,grepl('[[:alpha:]]', colnames(important_words_df))]
  important_words_df <- important_words_df[, !grepl("\\d",colnames(important_words_df))]
  
  # important_words_df$test <- rowSums(important_words_df)
  # summary(important_words_df$test)
  # test <- important_words_df[important_words_df$test==0,]
  # View(test)
  # dim(data)
  # dim(important_words_df)
  # test2 <- data[116,]
  
  
  data <- cbind(data, important_words_df)
  
  #head(data[,!(colnames(data) %in% c("Review"))],5)
  #head(data,5)
  
  #Split data into training and testing set
  library(caTools)
  set.seed(1234)
  
  spl <- sample.split(data$Sentiment, .8)
  insample_data <- data[spl==T,]
  outofsample_data <- data[spl==F,]
  
  insample_data$Review <- NULL
  insample_data$Date <- NULL
  
  # Add back rating
  backupData2 <- data
  backupData2$Rating.Score <- temp.backup$Rating.Score
  
  total.data <- list()
  total.data$data <- data
  total.data$backupData <- backupData
  total.data$backupData2 <- backupData2
  total.data$spl <- spl
  total.data$dtm <- dtm
  total.data$insample_data <- insample_data
  total.data$outofsample_data <- outofsample_data
  
  return(total.data)
}

train.model.cv <- function(insample_data){
  library(ncvreg)
  library(MLmetrics)
  
  X = insample_data[,!names(insample_data) %in% c("Sentiment")]
  y = insample_data$Sentiment
  
  #Range of values for f and gamma
  k_all <- c(5:20)
  gamma_all <- seq(2.1,4,0.1)
  result <- NULL
  #Parallelization
  # library(parallel)
  # cl <- makeCluster(4)
  
  for(i in 1:length(k_all)){
    #i=1
    for(j in 1:length(gamma_all)){
      #j=1
      #Cross Validation for SCAD
      CV_SCAD_fit <- cv.ncvreg(as.matrix(X),
                               y,
                               family = "binomial", penalty = "SCAD",
                               gamma = gamma_all[j],
                               # gamma = 3.7,
                               alpha = 1,
                               nlambda = 100, lambda.min=0.05,
                               #                         lambda = 0.02654098,
                               eps = 0.001, max.iter = 1000,
                               convex = TRUE, 
                               nfolds=k_all[i],
                               # nfolds=10,
                               # cluster=cl,
                               seed = 1234, trace=TRUE)
      
      #The value of lambda with the minimum cross-validation error
      optLambda <- CV_SCAD_fit$lambda.min
      
      #Min cv error
      minCV <- CV_SCAD_fit$cve[CV_SCAD_fit$min]
      
      temp <- c(k_all[i],gamma_all[j],optLambda,minCV)
      result <- rbind(result, temp)
      
      cat(paste("k is",k_all[i],"and gamma is",gamma_all[j]))
    }
  }
  
  result <- as.data.frame(result)
  colnames(result) <- c("NumOfFolds","Gamma","OptLambda","CVE")
  result$NumOfFolds <- as.factor(result$NumOfFolds)
  
  return(result)
}

apply.model.opt <- function(total.data,model.cv){
  insample_data <- total.data$insample_data
  outofsample_data <- total.data$outofsample_data
  X = insample_data[,!names(insample_data) %in% c("Sentiment")]
  y = insample_data$Sentiment
  
  #Fit SCAD using optimal parameters
  optLambda <- model.cv[model.cv$CVE == min(model.cv$CVE),"OptLambda"]
  optNumOfFolds <- as.numeric(as.character(model.cv[model.cv$CVE == min(model.cv$CVE),"NumOfFolds"]))
  optGamma <- model.cv[model.cv$CVE == min(model.cv$CVE),"Gamma"]
  
  #-------------SCAD penalized logistic regression---------------
  #Train using in sample data 
  SCAD_fit <- ncvreg(as.matrix(X),
                     y,
                     family = "binomial", penalty = "SCAD",
                     gamma = optGamma, alpha = 1,
                     nlambda=100,
                     eps = 0.001, max.iter = 1000,
                     convex = FALSE)
  # plot <- NULL
  # par(mfrow=c(1,1))
  # plot(SCAD_fit, log.l = TRUE,main="TF-IDF representation with review length")
  # 
  #In sample accuracy with optimal lambda
  log_pred_train_SCAD <- predict(SCAD_fit, as.matrix(X), 
                                 type="response", lambda=optLambda)
  log_pred_train_SCAD_result <- as.factor(ifelse(log_pred_train_SCAD >.5,"positive","negative"))
  
  #Calculate metrics
  Models_InSample <- NULL
  
  confusion.table1 <- table(log_pred_train_SCAD_result,insample_data$Sentiment)
  confusion.table1 <- as.data.frame(confusion.table1)
  colnames(confusion.table1) <- c("Prediction","True","Freq")
  confusion.table1$Prediction <- as.character(confusion.table1$Prediction)
  confusion.table1$True <- as.character(confusion.table1$True)
  a1=confusion.table1[confusion.table1$Prediction=="positive" & confusion.table1$True=="positive","Freq"]
  b1=confusion.table1[confusion.table1$Prediction=="positive" & confusion.table1$True=="negative","Freq"]
  c1=confusion.table1[confusion.table1$Prediction=="negative" & confusion.table1$True=="positive","Freq"]
  d1=confusion.table1[confusion.table1$Prediction=="negative" & confusion.table1$True=="negative","Freq"]
  
  a1=ifelse(length(a1)==0,0,a1)
  b1=ifelse(length(b1)==0,0,b1)
  c1=ifelse(length(c1)==0,0,c1)
  d1=ifelse(length(d1)==0,0,d1)
  
  #Store in dataframe
  table.values.train <- NULL
  Insample_NumFeatures <- dim(insample_data)[2]-1
  SCAD.table.train <- c(a1,b1,c1,d1,Insample_NumFeatures)
  table.values.train <- rbind(table.values.train,SCAD.table.train)
  colnames(table.values.train) <- c("a","b","c","d",Insample_NumFeatures)
  
  
  SCAD_InSample_Accuracy <- (a1+d1)/(a1+b1+c1+d1)
  SCAD_InSample_TPR <- a1/(a1+c1)
  SCAD_InSample_TNR <- d1/(b1+d1)
  SCAD_InSample_PPV <- a1/(a1+b1)
  SCAD_InSample_NPV <- d1/(c1+d1)
  SCAD_InSample_F1 <- (2 * SCAD_InSample_PPV * SCAD_InSample_TPR) / (SCAD_InSample_PPV + SCAD_InSample_TPR)
  
  SCAD_InSample <- c(SCAD_InSample_Accuracy, 
                     SCAD_InSample_TPR, 
                     SCAD_InSample_TNR,
                     SCAD_InSample_PPV,
                     SCAD_InSample_NPV,
                     SCAD_InSample_F1,
                     Insample_NumFeatures)
  Models_InSample <- rbind(Models_InSample, SCAD_InSample)
  colnames(Models_InSample) <- c("Accuracy","TPR","TNR","PPV","NPV","F1","Insample_NumFeatures")
  #Models_InSample
  
  #Check coefficients
  coefficients <- coef(SCAD_fit, lambda=optLambda)
  #Keep non-zero coefficieints
  coefficients <- coefficients[coefficients!=0]
  coefficients <- coefficients[order(coefficients)]
  # head(coefficients, 10)
  # tail(coefficients, 11)
  names <- names(coefficients)
  coefficients <- as.data.frame(coefficients)
  #row.names(coefficients) <- names
  coefficients$feature <- names
  #Change features starting with X
  coefficients$feature <- ifelse(substr(coefficients$feature,1,1)=="X",
                                 substr(coefficients$feature,2,nchar(coefficients$feature)),coefficients$feature)
  
  #write.csv(round(coefficients,4),"test.csv")
  
  # #Non-zero coefficients
  # predict(SCAD_fit, type="vars", lambda=optLambda)
  cat("\nTotal number of features:\n")
  cat(dim(X)[2])
  cat("\nNumber of non-zero coefficients:\n")
  cat(as.numeric(predict(SCAD_fit, type="nvars", lambda=optLambda)))
  cat("\nRatio of non-zero coefficients:\n")
  cat(as.numeric(predict(SCAD_fit, type="nvars", lambda=optLambda))/dim(X)[2])
  
  #Predict on test set
  log_pred_test_SCAD <- predict(SCAD_fit, as.matrix(outofsample_data
                                                    [, !names(outofsample_data) %in% c("Date","Review","Sentiment")]), 
                                type="response", lambda=optLambda)
  
  log_pred_test_SCAD_result <- as.factor(ifelse(log_pred_test_SCAD >.5,"positive","negative"))
  
  #Calculate metrics
  Models_OutOfSample <- NULL
  
  confusion.table2 <- table(log_pred_test_SCAD_result,outofsample_data$Sentiment)
  
  confusion.table2 <- as.data.frame(confusion.table2)
  colnames(confusion.table2) <- c("Prediction","True","Freq")
  confusion.table2$Prediction <- as.character(confusion.table2$Prediction)
  confusion.table2$True <- as.character(confusion.table2$True)
  a2=confusion.table2[confusion.table2$Prediction=="positive" & confusion.table2$True=="positive","Freq"]
  b2=confusion.table2[confusion.table2$Prediction=="positive" & confusion.table2$True=="negative","Freq"]
  c2=confusion.table2[confusion.table2$Prediction=="negative" & confusion.table2$True=="positive","Freq"]
  d2=confusion.table2[confusion.table2$Prediction=="negative" & confusion.table2$True=="negative","Freq"]
  
  a2=ifelse(length(a2)==0,0,a2)
  b2=ifelse(length(b2)==0,0,b2)
  c2=ifelse(length(c2)==0,0,c2)
  d2=ifelse(length(d2)==0,0,d2)
  
  #Store in dataframe
  table.values.test <- NULL
  OutofSample_NumFeatures <- dim(outofsample_data)[2]-3
  SCAD.table.test <- c(a2,b2,c2,d2,OutofSample_NumFeatures)
  table.values.test <- rbind(table.values.test,SCAD.table.test)
  colnames(table.values.test) <- c("a","b","c","d","OutofSample_NumFeatures")
  
  SCAD_OutOfSample_Accuracy <- (a2+d2)/(a2+b2+c2+d2)
  SCAD_OutOfSample_TPR <- a2/(a2+c2)
  SCAD_OutOfSample_TNR <- d2/(b2+d2)
  SCAD_OutOfSample_PPV <- a2/(a2+b2)
  SCAD_OutOfSample_NPV <- d2/(c2+d2)
  SCAD_OutOfSample_F1 <- (2 * SCAD_OutOfSample_PPV * SCAD_OutOfSample_TPR) / 
    (SCAD_OutOfSample_PPV + SCAD_OutOfSample_TPR)
  
  SCAD_OutOfSample <- c(SCAD_OutOfSample_Accuracy, 
                        SCAD_OutOfSample_TPR, 
                        SCAD_OutOfSample_TNR,
                        SCAD_OutOfSample_PPV,
                        SCAD_OutOfSample_NPV,
                        SCAD_OutOfSample_F1,
                        OutofSample_NumFeatures)
  Models_OutOfSample <- rbind(Models_OutOfSample, SCAD_OutOfSample)
  colnames(Models_OutOfSample) <- c("Accuracy","TPR","TNR","PPV","NPV","F1","OutofSample_NumFeatures")
  #Models_OutOfSample
  
  model.opt <- list()
  model.opt$optLambda <- optLambda
  model.opt$optNumOfFolds <- optNumOfFolds
  model.opt$optGamma <- optGamma
  model.opt$SCAD_fit <- SCAD_fit
  model.opt$confusion.table1 <- confusion.table1
  model.opt$confusion.table2 <- confusion.table2
  model.opt$table.values.train <- table.values.train
  model.opt$Models_InSample <- Models_InSample
  model.opt$table.values.test <- table.values.test
  model.opt$Models_OutOfSample <- Models_OutOfSample
  model.opt$coefficients <- coefficients
  
  return(model.opt)
}

apply.model.lr <- function(total.data,model.opt){
  
  #---------------Logistic Regression-------------
  insample_data <- total.data$insample_data
  outofsample_data <- total.data$outofsample_data
  X = insample_data[,!names(insample_data) %in% c("Sentiment")]
  y = insample_data$Sentiment
  table.values.train <- model.opt$table.values.train
  Models_InSample <- model.opt$Models_InSample
  table.values.test <- model.opt$table.values.test
  Models_OutOfSample <- model.opt$Models_OutOfSample
  
  #Train
  log_model <- glm(Sentiment~., data=insample_data, family=binomial)
  #  summary(log_model)
  
  #In sample accuracy
  log_pred <- predict(log_model, newdata=insample_data
                      [,!names(insample_data) %in% c("Sentiment")], 
                      type="response")
  
  log_pred_result <- as.factor(ifelse(log_pred >.5,"positive","negative"))
  
  #Calculate metrics
  confusion.table1 <- table(log_pred_result,insample_data$Sentiment)
  
  confusion.table1 <- as.data.frame(confusion.table1)
  colnames(confusion.table1) <- c("Prediction","True","Freq")
  confusion.table1$Prediction <- as.character(confusion.table1$Prediction)
  confusion.table1$True <- as.character(confusion.table1$True)
  a1=confusion.table1[confusion.table1$Prediction=="positive" & confusion.table1$True=="positive","Freq"]
  b1=confusion.table1[confusion.table1$Prediction=="positive" & confusion.table1$True=="negative","Freq"]
  c1=confusion.table1[confusion.table1$Prediction=="negative" & confusion.table1$True=="positive","Freq"]
  d1=confusion.table1[confusion.table1$Prediction=="negative" & confusion.table1$True=="negative","Freq"]
  
  a1=ifelse(length(a1)==0,0,a1)
  b1=ifelse(length(b1)==0,0,b1)
  c1=ifelse(length(c1)==0,0,c1)
  d1=ifelse(length(d1)==0,0,d1)
  
  Insample_NumFeatures <- dim(insample_data)[2]-1
  LR.table.train <- c(a1,b1,c1,d1,Insample_NumFeatures)
  temp_name <- paste0("LR.table.train_",current_thres)
  table.values.train <- rbind(table.values.train,LR.table.train)
  colnames(table.values.train) <- c("a","b","c","d","Insample_NumFeatures")
  rownames(table.values.train)[nrow(table.values.train)] <- temp_name
  
  
  LR_InSample_Accuracy <- (a1+d1)/(a1+b1+c1+d1)
  LR_InSample_TPR <- a1/(a1+c1)
  LR_InSample_TNR <- d1/(b1+d1)
  LR_InSample_PPV <- a1/(a1+b1)
  LR_InSample_NPV <- d1/(c1+d1)
  LR_InSample_NPV_F1 <- (2 * LR_InSample_PPV * LR_InSample_TPR) / 
    (LR_InSample_PPV + LR_InSample_TPR)
  
  LR_InSample <- c(LR_InSample_Accuracy, 
                   LR_InSample_TPR, 
                   LR_InSample_TNR,
                   LR_InSample_PPV,
                   LR_InSample_NPV,
                   LR_InSample_NPV_F1,
                   Insample_NumFeatures)
  temp_name <- paste0("LR_InSample_",current_thres)
  Models_InSample <- rbind(Models_InSample, LR_InSample)
  rownames(Models_InSample)[nrow(table.values.train)] <- temp_name
  #  Models_InSample
  
  
  #Save to list
  coefficients_LR <- log_model$coefficients
  coefficients_LR <- coefficients_LR[order(coefficients_LR)]
  names_LR <- names(coefficients_LR)
  coefficients_LR <- as.data.frame(coefficients_LR)
  colnames(coefficients_LR) <- "Estimate"
  coefficients_LR$Term <- names_LR
  coefficients_LR <- coefficients_LR[,c("Term","Estimate")]
  #  write.csv(round(coefficients_LR,4),"test.csv")
  coefficients_LR_All <- coefficients_LR
  
  temp <- data.frame(summary(log_model)$coef[summary(log_model)$coef[,4] <= .1, ])
  colnames(temp) <- c("Estimate","StdErr","zValue","pValue")
  temp$Term <- rownames(temp)
  temp <- temp[c("Term","Estimate","StdErr","zValue","pValue")]
  summary_LR_All <- temp
  
  
  #Out of sample
  log_pred_test_LR <- predict(log_model, newdata=outofsample_data
                              [, !names(outofsample_data) %in% c("Date","Review","Sentiment")], 
                              type="response")
  
  log_pred_test_LR_result <- as.factor(ifelse(log_pred_test_LR >.5,"positive","negative"))
  
  #Calculate metrics
  confusion.table2 <- table(log_pred_test_LR_result,outofsample_data$Sentiment)
  
  confusion.table2 <- as.data.frame(confusion.table2)
  colnames(confusion.table2) <- c("Prediction","True","Freq")
  confusion.table2$Prediction <- as.character(confusion.table2$Prediction)
  confusion.table2$True <- as.character(confusion.table2$True)
  a2=confusion.table2[confusion.table2$Prediction=="positive" & confusion.table2$True=="positive","Freq"]
  b2=confusion.table2[confusion.table2$Prediction=="positive" & confusion.table2$True=="negative","Freq"]
  c2=confusion.table2[confusion.table2$Prediction=="negative" & confusion.table2$True=="positive","Freq"]
  d2=confusion.table2[confusion.table2$Prediction=="negative" & confusion.table2$True=="negative","Freq"]
  
  a2=ifelse(length(a2)==0,0,a2)
  b2=ifelse(length(b2)==0,0,b2)
  c2=ifelse(length(c2)==0,0,c2)
  d2=ifelse(length(d2)==0,0,d2)
  
  OutofSample_NumFeatures <- dim(outofsample_data)[2]-3
  LR.table.test <- c(a2,b2,c2,d2,OutofSample_NumFeatures)
  LR.table.test[is.na(LR.table.test)] <- 0
  temp_name <- paste0("LR.table.train_",current_thres)
  table.values.test <- rbind(table.values.test,LR.table.test)
  colnames(table.values.test) <- c("a","b","c","d","OutofSample_NumFeatures")
  rownames(table.values.test)[nrow(table.values.train)] <- temp_name
  
  
  LR_OutOfSample_Accuracy <- (a2+d2)/(a2+b2+c2+d2)
  LR_OutOfSample_TPR <- a2/(a2+c2)
  LR_OutOfSample_TNR <- d2/(b2+d2)
  LR_OutOfSample_PPV <- a2/(a2+b2)
  LR_OutOfSample_NPV <- d2/(c2+d2)
  LR_OutOfSample_F1 <- (2 * LR_OutOfSample_PPV * LR_OutOfSample_TPR) / 
    (LR_OutOfSample_PPV + LR_OutOfSample_TPR)
  
  LR_OutOfSample <- c(LR_OutOfSample_Accuracy, 
                      LR_OutOfSample_TPR, 
                      LR_OutOfSample_TNR,
                      LR_OutOfSample_PPV,
                      LR_OutOfSample_NPV,
                      LR_OutOfSample_F1,
                      OutofSample_NumFeatures)
  temp_name <- paste0("LR_OutOfSample_",current_thres)
  Models_OutOfSample <- rbind(Models_OutOfSample, LR_OutOfSample)
  rownames(Models_OutOfSample)[nrow(table.values.train)] <- temp_name
  
  #  Models_OutOfSample
  
  
  Models_InSample <- as.data.frame(Models_InSample)
  Models_OutOfSample <- as.data.frame(Models_OutOfSample)
  table.values.train <- as.data.frame(table.values.train)
  table.values.test <- as.data.frame(table.values.test)
  # 
  # Models_InSample$threshold <- c("NA",thresholds)
  # Models_OutOfSample$threshold <- c("NA",thresholds)
  # table.values.train$threshold <- c("NA",thresholds)
  # table.values.test$threshold <- c("NA",thresholds)
  
  Models_InSample[,c(1:5)] <- round(Models_InSample[,c(1:5)],4)
  Models_OutOfSample[,c(1:5)] <- round(Models_OutOfSample[,c(1:5)],4)
  
  # rownames(Models_InSample) <- c("SCAD",thresholds)
  
  Models_InSample$Model <- rownames(Models_InSample)
  Models_InSample$Model[1] <- 1
  Models_InSample <- Models_InSample[,c("Model","Accuracy","TPR","TNR","PPV","NPV","F1","Insample_NumFeatures")]
  
  rownames(Models_OutOfSample) <- c("SCAD",current_thres)
  Models_OutOfSample$Model <- rownames(Models_OutOfSample)
  Models_OutOfSample$Model[1] <- 1
  Models_OutOfSample <- Models_OutOfSample[,c("Model","Accuracy","TPR","TNR","PPV","NPV","F1","OutofSample_NumFeatures")]
  
  mode.lr <- list()
  mode.lr$Models_InSample <- Models_InSample
  mode.lr$Models_OutOfSample <- Models_OutOfSample
  mode.lr$table.values.train <- table.values.train
  mode.lr$table.values.test <- table.values.test
  mode.lr$coefficients_LR_All <- coefficients_LR_All
  mode.lr$summary_LR_All <- summary_LR_All
  
  return(mode.lr)
}

apply.model.svm <- function(total.data,model.lr){
  insample_data <- total.data$insample_data
  outofsample_data <- total.data$outofsample_data
  X = insample_data[,!names(insample_data) %in% c("Sentiment")]
  y = insample_data$Sentiment
  
  table.values.train <- model.lr$table.values.train
  Models_InSample <- model.lr$Models_InSample
  table.values.test <- model.lr$table.values.test
  Models_OutOfSample <- model.lr$Models_OutOfSample
  a1=b1=c1=d1=a2=b2=c2=d2=NULL
  
  library(e1071)
  # svm_model <- svm(Sentiment~., data=insample_data)
  svm_model <- tune(svm, train.x=X, train.y=y,
                    kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
  svm_model.backup <- svm_model
  # svm_pred <- predict(svm_model,insample_data
  #                     [,!names(insample_data) %in% c("Sentiment")])
  # svm_pred_result <- as.factor(ifelse(svm_pred >.5,"positive","negative"))
  svm_pred_result <- predict(svm_model$best.model,insample_data[,!names(insample_data) %in% c("Sentiment")])
  
  
  confusion.table1 <- table(svm_pred_result,insample_data$Sentiment)
  confusion.table1 <- as.data.frame(confusion.table1)
  colnames(confusion.table1) <- c("Prediction","True","Freq")
  confusion.table1$Prediction <- as.character(confusion.table1$Prediction)
  confusion.table1$True <- as.character(confusion.table1$True)
  a1=confusion.table1[confusion.table1$Prediction=="positive" & confusion.table1$True=="positive","Freq"]
  b1=confusion.table1[confusion.table1$Prediction=="positive" & confusion.table1$True=="negative","Freq"]
  c1=confusion.table1[confusion.table1$Prediction=="negative" & confusion.table1$True=="positive","Freq"]
  d1=confusion.table1[confusion.table1$Prediction=="negative" & confusion.table1$True=="negative","Freq"]
  
  a1=ifelse(length(a1)==0,0,a1)
  b1=ifelse(length(b1)==0,0,b1)
  c1=ifelse(length(c1)==0,0,c1)
  d1=ifelse(length(d1)==0,0,d1)
  
  #Store in dataframe
  table.values.train <- NULL
  Insample_NumFeatures <- dim(insample_data)[2]-1
  svm.table.train <- c(a1,b1,c1,d1,Insample_NumFeatures)
  table.values.train <- rbind(table.values.train,svm.table.train)
  colnames(table.values.train) <- c("a","b","c","d",Insample_NumFeatures)
  
  
  svm_InSample_Accuracy <- (a1+d1)/(a1+b1+c1+d1)
  svm_InSample_TPR <- a1/(a1+c1)
  svm_InSample_TNR <- d1/(b1+d1)
  svm_InSample_PPV <- a1/(a1+b1)
  svm_InSample_NPV <- d1/(c1+d1)
  svm_InSample_F1 <- (2 * svm_InSample_PPV * svm_InSample_TPR) / (svm_InSample_PPV + svm_InSample_TPR)
  
  svm_InSample <- c("SVM",svm_InSample_Accuracy, 
                    svm_InSample_TPR, 
                    svm_InSample_TNR,
                    svm_InSample_PPV,
                    svm_InSample_NPV,
                    svm_InSample_F1,
                    Insample_NumFeatures)
  
  Models_InSample <- rbind(Models_InSample, svm_InSample)
  colnames(Models_InSample) <- c("Model","Accuracy","TPR","TNR","PPV","NPV","F1","Insample_NumFeatures")
  temp_name <- "SVM"
  rownames(Models_InSample)[nrow(Models_InSample)] <- temp_name
  
  
  #test set - Out of sample
  log_pred_test_svm_result <- predict(svm_model$best.model, newdata=outofsample_data
                                      [, !names(outofsample_data) %in% c("Date","Review","Sentiment")], 
                                      type="response")
  
  # log_pred_test_svm_result <- as.factor(ifelse(log_pred_test_svm >.5,"positive","negative"))
  
  #Calculate metrics
  confusion.table2 <- table(log_pred_test_svm_result,outofsample_data$Sentiment)
  
  confusion.table2 <- as.data.frame(confusion.table2)
  colnames(confusion.table2) <- c("Prediction","True","Freq")
  confusion.table2$Prediction <- as.character(confusion.table2$Prediction)
  confusion.table2$True <- as.character(confusion.table2$True)
  a2=confusion.table2[confusion.table2$Prediction=="positive" & confusion.table2$True=="positive","Freq"]
  b2=confusion.table2[confusion.table2$Prediction=="positive" & confusion.table2$True=="negative","Freq"]
  c2=confusion.table2[confusion.table2$Prediction=="negative" & confusion.table2$True=="positive","Freq"]
  d2=confusion.table2[confusion.table2$Prediction=="negative" & confusion.table2$True=="negative","Freq"]
  
  a2=ifelse(length(a2)==0,0,a2)
  b2=ifelse(length(b2)==0,0,b2)
  c2=ifelse(length(c2)==0,0,c2)
  d2=ifelse(length(d2)==0,0,d2)
  
  OutofSample_NumFeatures <- dim(outofsample_data)[2]-3
  svm.table.test <- c(a2,b2,c2,d2,OutofSample_NumFeatures)
  svm.table.test[is.na(svm.table.test)] <- 0
  # temp_name <- paste0("svm.table.train_",thresholds[i])
  temp_name <- "svm.table.train"
  table.values.test <- rbind(table.values.test,svm.table.test)
  colnames(table.values.test) <- c("a","b","c","d","OutofSample_NumFeatures")
  rownames(table.values.test)[nrow(table.values.train)] <- temp_name
  
  
  svm_OutOfSample_Accuracy <- (a2+d2)/(a2+b2+c2+d2)
  svm_OutOfSample_TPR <- a2/(a2+c2)
  svm_OutOfSample_TNR <- d2/(b2+d2)
  svm_OutOfSample_PPV <- a2/(a2+b2)
  svm_OutOfSample_NPV <- d2/(c2+d2)
  svm_OutOfSample_F1 <- (2 * svm_OutOfSample_PPV * svm_OutOfSample_TPR) / 
    (svm_OutOfSample_PPV + svm_OutOfSample_TPR)
  
  svm_OutOfSample <- c("SVM",
                       svm_OutOfSample_Accuracy, 
                       svm_OutOfSample_TPR, 
                       svm_OutOfSample_TNR,
                       svm_OutOfSample_PPV,
                       svm_OutOfSample_NPV,
                       svm_OutOfSample_F1,
                       OutofSample_NumFeatures)
  # temp_name <- paste0("svm_OutOfSample_",thresholds[i])
  temp_name <- "svm_OutOfSample"
  Models_OutOfSample <- rbind(Models_OutOfSample, svm_OutOfSample)
  rownames(Models_OutOfSample)[nrow(Models_OutOfSample)] <- temp_name
  
  
  model.svm <- list()
  model.svm$Models_InSample <- Models_InSample
  model.svm$Models_OutOfSample <- Models_OutOfSample
  model.svm$table.values.train <- table.values.train
  model.svm$table.values.test <- table.values.test
  # mode.svm$coefficients_svm_All <- coefficients_svm_All
  # mode.svm$summary_svm_All <- summary_svm_All
  
  return(model.svm)
}

apply.model.knn <- function(total.data,model.svm){
  insample_data <- total.data$insample_data
  outofsample_data <- total.data$outofsample_data
  table.values.train <- model.svm$table.values.train
  Models_InSample <- model.svm$Models_InSample
  table.values.test <- model.svm$table.values.test
  Models_OutOfSample <- model.svm$Models_OutOfSample
  a1=b1=c1=d1=a2=b2=c2=d2=NULL
  
  
  #Train
  library(caret)
  ctrl <- trainControl(method="repeatedcv",repeats = 3)  
  knn_model <- train(Sentiment ~ ., data = insample_data, method = "knn", 
                     trControl = ctrl, preProcess = c("center","scale"))
  
  knn_model.backup <- knn_model
  # knn_pred <- predict(knn_model,insample_data
  #                     [,!names(insample_data) %in% c("Sentiment")])
  # knn_pred_result <- as.factor(ifelse(knn_pred >.5,"positive","negative"))
  library(class)
  knn_model_result <-  knn(insample_data[,!names(insample_data) %in% c("Sentiment")],
                           insample_data[,!names(insample_data) %in% c("Sentiment")],
                           insample_data$Sentiment, k=as.numeric(knn_model$bestTune))
  
  # knn_pred_result <- predict(knn_model$finalModel,insample_data[,!names(insample_data) %in% c("Sentiment")])
  
  confusion.table1 <- table(knn_model_result,insample_data$Sentiment)
  confusion.table1 <- as.data.frame(confusion.table1)
  colnames(confusion.table1) <- c("Prediction","True","Freq")
  confusion.table1$Prediction <- as.character(confusion.table1$Prediction)
  confusion.table1$True <- as.character(confusion.table1$True)
  a1=confusion.table1[confusion.table1$Prediction=="positive" & confusion.table1$True=="positive","Freq"]
  b1=confusion.table1[confusion.table1$Prediction=="positive" & confusion.table1$True=="negative","Freq"]
  c1=confusion.table1[confusion.table1$Prediction=="negative" & confusion.table1$True=="positive","Freq"]
  d1=confusion.table1[confusion.table1$Prediction=="negative" & confusion.table1$True=="negative","Freq"]
  
  a1=ifelse(length(a1)==0,0,a1)
  b1=ifelse(length(b1)==0,0,b1)
  c1=ifelse(length(c1)==0,0,c1)
  d1=ifelse(length(d1)==0,0,d1)
  
  #Store in dataframe
  table.values.train <- NULL
  Insample_NumFeatures <- dim(insample_data)[2]-1
  knn.table.train <- c(a1,b1,c1,d1,Insample_NumFeatures)
  table.values.train <- rbind(table.values.train,knn.table.train)
  colnames(table.values.train) <- c("a","b","c","d",Insample_NumFeatures)
  
  
  knn_InSample_Accuracy <- (a1+d1)/(a1+b1+c1+d1)
  knn_InSample_TPR <- a1/(a1+c1)
  knn_InSample_TNR <- d1/(b1+d1)
  knn_InSample_PPV <- a1/(a1+b1)
  knn_InSample_NPV <- d1/(c1+d1)
  knn_InSample_F1 <- (2 * knn_InSample_PPV * knn_InSample_TPR) / (knn_InSample_PPV + knn_InSample_TPR)
  
  knn_InSample <- c("KNN",knn_InSample_Accuracy, 
                    knn_InSample_TPR, 
                    knn_InSample_TNR,
                    knn_InSample_PPV,
                    knn_InSample_NPV,
                    knn_InSample_F1,
                    Insample_NumFeatures)
  Models_InSample <- rbind(Models_InSample, knn_InSample)
  colnames(Models_InSample) <- c("Model","Accuracy","TPR","TNR","PPV","NPV","F1","Insample_NumFeatures")
  temp_name <- "KNN"
  rownames(Models_InSample)[nrow(Models_InSample)] <- temp_name
  
  
  #test set - Out of sample
  # log_pred_test_knn_result <- predict(knn_model$best.model, newdata=outofsample_data
  #                                     [, !names(outofsample_data) %in% c("Date","Review","Sentiment")], 
  #                                     type="response")
  log_pred_test_knn_result <-  knn(insample_data[,!names(insample_data) %in% c("Sentiment")], 
                                   outofsample_data[, !names(outofsample_data) %in% c("Date","Review","Sentiment")],
                                   insample_data$Sentiment, k=as.numeric(knn_model$bestTune))
  
  # log_pred_test_knn_result <- as.factor(ifelse(log_pred_test_knn >.5,"positive","negative"))
  
  #Calculate metrics
  confusion.table2 <- table(log_pred_test_knn_result,outofsample_data$Sentiment)
  
  confusion.table2 <- as.data.frame(confusion.table2)
  colnames(confusion.table2) <- c("Prediction","True","Freq")
  confusion.table2$Prediction <- as.character(confusion.table2$Prediction)
  confusion.table2$True <- as.character(confusion.table2$True)
  a2=confusion.table2[confusion.table2$Prediction=="positive" & confusion.table2$True=="positive","Freq"]
  b2=confusion.table2[confusion.table2$Prediction=="positive" & confusion.table2$True=="negative","Freq"]
  c2=confusion.table2[confusion.table2$Prediction=="negative" & confusion.table2$True=="positive","Freq"]
  d2=confusion.table2[confusion.table2$Prediction=="negative" & confusion.table2$True=="negative","Freq"]
  
  a2=ifelse(length(a2)==0,0,a2)
  b2=ifelse(length(b2)==0,0,b2)
  c2=ifelse(length(c2)==0,0,c2)
  d2=ifelse(length(d2)==0,0,d2)
  
  OutofSample_NumFeatures <- dim(outofsample_data)[2]-3
  knn.table.test <- c(a2,b2,c2,d2,OutofSample_NumFeatures)
  knn.table.test[is.na(knn.table.test)] <- 0
  # temp_name <- paste0("knn.table.train_",thresholds[i])
  temp_name <- "knn.table.train"
  table.values.test <- rbind(table.values.test,knn.table.test)
  colnames(table.values.test) <- c("a","b","c","d","OutofSample_NumFeatures")
  rownames(table.values.test)[nrow(table.values.train)] <- temp_name
  
  
  knn_OutOfSample_Accuracy <- (a2+d2)/(a2+b2+c2+d2)
  knn_OutOfSample_TPR <- a2/(a2+c2)
  knn_OutOfSample_TNR <- d2/(b2+d2)
  knn_OutOfSample_PPV <- a2/(a2+b2)
  knn_OutOfSample_NPV <- d2/(c2+d2)
  knn_OutOfSample_F1 <- (2 * knn_OutOfSample_PPV * knn_OutOfSample_TPR) / 
    (knn_OutOfSample_PPV + knn_OutOfSample_TPR)
  
  knn_OutOfSample <- c("knn",
                       knn_OutOfSample_Accuracy, 
                       knn_OutOfSample_TPR, 
                       knn_OutOfSample_TNR,
                       knn_OutOfSample_PPV,
                       knn_OutOfSample_NPV,
                       knn_OutOfSample_F1,
                       OutofSample_NumFeatures)
  # temp_name <- paste0("knn_OutOfSample_",thresholds[i])
  temp_name <- "knn_OutOfSample"
  Models_OutOfSample <- rbind(Models_OutOfSample, knn_OutOfSample)
  rownames(Models_OutOfSample)[nrow(Models_OutOfSample)] <- temp_name
  
  
  model.knn <- list()
  model.knn$Models_InSample <- Models_InSample
  model.knn$Models_OutOfSample <- Models_OutOfSample
  model.knn$table.values.train <- table.values.train
  model.knn$table.values.test <- table.values.test
  # mode.knn$coefficients_knn_All <- coefficients_knn_All
  # mode.knn$summary_knn_All <- summary_knn_All
  
  return(model.knn)
}

apply.model.nb <- function(total.data,model.knn){
  insample_data <- total.data$insample_data
  outofsample_data <- total.data$outofsample_data
  table.values.train <- model.knn$table.values.train
  Models_InSample <- model.knn$Models_InSample
  table.values.test <- model.knn$table.values.test
  Models_OutOfSample <- model.knn$Models_OutOfSample
  a1=b1=c1=d1=a2=b2=c2=d2=NULL
  
  
  #Train
  library(caret)
  train_control <- trainControl(method="cv", number=10)
  nb_model <- train(Sentiment ~ ., data = insample_data, 
                    method = "nb",trControl=train_control,preProcess = c("center","scale"))
  
  nb_model.backup <- nb_model
  
  nb_pred <- predict(nb_model$finalModel,insample_data
                     [,!names(insample_data) %in% c("Sentiment")])
  
  nb_model_result <- nb_pred$class
  # nb_pred_result <- as.factor(ifelse(nb_pred >.5,"positive","negative"))
  
  confusion.table1 <- table(nb_model_result,insample_data$Sentiment)
  confusion.table1 <- as.data.frame(confusion.table1)
  colnames(confusion.table1) <- c("Prediction","True","Freq")
  confusion.table1$Prediction <- as.character(confusion.table1$Prediction)
  confusion.table1$True <- as.character(confusion.table1$True)
  a1=confusion.table1[confusion.table1$Prediction=="positive" & confusion.table1$True=="positive","Freq"]
  b1=confusion.table1[confusion.table1$Prediction=="positive" & confusion.table1$True=="negative","Freq"]
  c1=confusion.table1[confusion.table1$Prediction=="negative" & confusion.table1$True=="positive","Freq"]
  d1=confusion.table1[confusion.table1$Prediction=="negative" & confusion.table1$True=="negative","Freq"]
  
  a1=ifelse(length(a1)==0,0,a1)
  b1=ifelse(length(b1)==0,0,b1)
  c1=ifelse(length(c1)==0,0,c1)
  d1=ifelse(length(d1)==0,0,d1)
  
  #Store in dataframe
  table.values.train <- NULL
  Insample_NumFeatures <- dim(insample_data)[2]-1
  nb.table.train <- c(a1,b1,c1,d1,Insample_NumFeatures)
  table.values.train <- rbind(table.values.train,nb.table.train)
  colnames(table.values.train) <- c("a","b","c","d",Insample_NumFeatures)
  
  
  nb_InSample_Accuracy <- (a1+d1)/(a1+b1+c1+d1)
  nb_InSample_TPR <- a1/(a1+c1)
  nb_InSample_TNR <- d1/(b1+d1)
  nb_InSample_PPV <- a1/(a1+b1)
  nb_InSample_NPV <- d1/(c1+d1)
  nb_InSample_F1 <- (2 * nb_InSample_PPV * nb_InSample_TPR) / (nb_InSample_PPV + nb_InSample_TPR)
  
  nb_InSample <- c("NB",nb_InSample_Accuracy, 
                   nb_InSample_TPR, 
                   nb_InSample_TNR,
                   nb_InSample_PPV,
                   nb_InSample_NPV,
                   nb_InSample_F1,
                   Insample_NumFeatures)
  Models_InSample <- rbind(Models_InSample, nb_InSample)
  colnames(Models_InSample) <- c("Model","Accuracy","TPR","TNR","PPV","NPV","F1","Insample_NumFeatures")
  temp_name <- "NB"
  rownames(Models_InSample)[nrow(Models_InSample)] <- temp_name
  
  
  #test set - Out of sample
  log_pred_test_nb_result <- predict(nb_model$finalModel,
                                     outofsample_data[, !names(outofsample_data) %in% c("Date","Review","Sentiment")])
  
  # log_pred_test_nb_result <- as.factor(ifelse(log_pred_test_nb >.5,"positive","negative"))
  
  #Calculate metrics
  confusion.table2 <- table(log_pred_test_nb_result$class,outofsample_data$Sentiment)
  
  confusion.table2 <- as.data.frame(confusion.table2)
  colnames(confusion.table2) <- c("Prediction","True","Freq")
  confusion.table2$Prediction <- as.character(confusion.table2$Prediction)
  confusion.table2$True <- as.character(confusion.table2$True)
  a2=confusion.table2[confusion.table2$Prediction=="positive" & confusion.table2$True=="positive","Freq"]
  b2=confusion.table2[confusion.table2$Prediction=="positive" & confusion.table2$True=="negative","Freq"]
  c2=confusion.table2[confusion.table2$Prediction=="negative" & confusion.table2$True=="positive","Freq"]
  d2=confusion.table2[confusion.table2$Prediction=="negative" & confusion.table2$True=="negative","Freq"]
  
  a2=ifelse(length(a2)==0,0,a2)
  b2=ifelse(length(b2)==0,0,b2)
  c2=ifelse(length(c2)==0,0,c2)
  d2=ifelse(length(d2)==0,0,d2)
  
  OutofSample_NumFeatures <- dim(outofsample_data)[2]-3
  nb.table.test <- c(a2,b2,c2,d2,OutofSample_NumFeatures)
  nb.table.test[is.na(nb.table.test)] <- 0
  # temp_name <- paste0("nb.table.train_",thresholds[i])
  temp_name <- "nb.table.train"
  table.values.test <- rbind(table.values.test,nb.table.test)
  colnames(table.values.test) <- c("a","b","c","d","OutofSample_NumFeatures")
  rownames(table.values.test)[nrow(table.values.train)] <- temp_name
  
  
  nb_OutOfSample_Accuracy <- (a2+d2)/(a2+b2+c2+d2)
  nb_OutOfSample_TPR <- a2/(a2+c2)
  nb_OutOfSample_TNR <- d2/(b2+d2)
  nb_OutOfSample_PPV <- a2/(a2+b2)
  nb_OutOfSample_NPV <- d2/(c2+d2)
  nb_OutOfSample_F1 <- (2 * nb_OutOfSample_PPV * nb_OutOfSample_TPR) / 
    (nb_OutOfSample_PPV + nb_OutOfSample_TPR)
  
  nb_OutOfSample <- c("nb",
                      nb_OutOfSample_Accuracy, 
                      nb_OutOfSample_TPR, 
                      nb_OutOfSample_TNR,
                      nb_OutOfSample_PPV,
                      nb_OutOfSample_NPV,
                      nb_OutOfSample_F1,
                      OutofSample_NumFeatures)
  # temp_name <- paste0("nb_OutOfSample_",thresholds[i])
  temp_name <- "nb_OutOfSample"
  Models_OutOfSample <- rbind(Models_OutOfSample, nb_OutOfSample)
  rownames(Models_OutOfSample)[nrow(Models_OutOfSample)] <- temp_name
  
  model.nb <- list()
  model.nb$Models_InSample <- Models_InSample
  model.nb$Models_OutOfSample <- Models_OutOfSample
  model.nb$table.values.train <- table.values.train
  model.nb$table.values.test <- table.values.test
  # mode.nb$coefficients_nb_All <- coefficients_nb_All
  # mode.nb$summary_nb_All <- summary_nb_All
  
  return(model.nb)
}

plot_insample <- function(result_all_insample){
  library(ggplot2)
  #Replace NaN with 0
  temp_data <- result_all_insample
  temp_data[temp_data == "NaN"] <- 0
  temp_data[,c(2:7)] <- apply(temp_data[,c(2:7)],2,as.numeric)
  #Order model type
  temp_data$Model <- factor(temp_data$Model, levels=c("SCAD", "LR", "SVM", "KNN", "NB"), ordered=TRUE)
  #Accuracy
  plot_accuracy <- ggplot(temp_data, aes(x=Threshold, 
                    y=Accuracy,  group=Model)) +
    geom_point(aes(color=Model,shape=Model),size=2) + 
    geom_line(aes(color=Model)) +
    facet_grid(. ~ Representation) +
    ggtitle("Accuracy") +
    theme(plot.title = element_text(hjust = 0.5))
  plot_accuracy + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))
  #TPR
  plot_tpr <- ggplot(temp_data, aes(x=Threshold, 
                                         y=TPR,  group=Model)) +
    geom_point(aes(color=Model,shape=Model),size=2) + 
    geom_line(aes(color=Model)) +
    facet_grid(. ~ Representation) +
    ggtitle("True Positive Rate") +
    theme(plot.title = element_text(hjust = 0.5))
  plot_tpr + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))
  #TNR
  plot_tnr <- ggplot(temp_data, aes(x=Threshold, 
                                    y=TNR,  group=Model)) +
    geom_point(aes(color=Model,shape=Model),size=2) + 
    geom_line(aes(color=Model)) +
    facet_grid(. ~ Representation) +
    ggtitle("True Negative Rate") +
    theme(plot.title = element_text(hjust = 0.5))
  plot_tnr + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))
  #PPV
  plot_ppv <- ggplot(temp_data, aes(x=Threshold, 
                                    y=PPV,  group=Model)) +
    geom_point(aes(color=Model,shape=Model),size=2) + 
    geom_line(aes(color=Model)) +
    facet_grid(. ~ Representation) +
    ggtitle("Positive Predictive Value") +
    theme(plot.title = element_text(hjust = 0.5))
  plot_ppv + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))

  #NPV
  plot_npv <- ggplot(temp_data, aes(x=Threshold, 
                                    y=NPV,  group=Model)) +
    geom_point(aes(color=Model,shape=Model),size=2) + 
    geom_line(aes(color=Model)) +
    facet_grid(. ~ Representation) +
    ggtitle("Negative Predictive Value") +
    theme(plot.title = element_text(hjust = 0.5))
  plot_npv + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  #NPV
  plot_f1 <- ggplot(temp_data, aes(x=Threshold, 
                                    y=F1,  group=Model)) +
    geom_point(aes(color=Model,shape=Model),size=2) + 
    geom_line(aes(color=Model)) +
    facet_grid(. ~ Representation) +
    ggtitle("F1 Score") +
    theme(plot.title = element_text(hjust = 0.5))
  plot_f1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  multiplot(plot_accuracy,plot_tpr,plot_tnr,plot_ppv,plot_npv,plot_f1,cols=2)
}

plot_outofsample <- function(result_all_outofsample){
  library(ggplot2)
  #Replace NaN with 0
  temp_data <- result_all_outofsample
  temp_data[temp_data == "NaN"] <- 0
  temp_data[,c(2:7)] <- apply(temp_data[,c(2:7)],2,as.numeric)
  #Order model type
  temp_data$Model <- factor(temp_data$Model, levels=c("SCAD", "LR", "SVM", "KNN", "NB"), ordered=TRUE)
  #Accuracy
  plot_accuracy <- ggplot(temp_data, aes(x=Threshold, 
                                         y=Accuracy,  group=Model)) +
    geom_point(aes(color=Model,shape=Model),size=2) + 
    geom_line(aes(color=Model)) +
    facet_grid(. ~ Representation) +
    ggtitle("Accuracy") +
    theme(plot.title = element_text(hjust = 0.5))
  plot_accuracy + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  #TPR
  plot_tpr <- ggplot(temp_data, aes(x=Threshold, 
                                    y=TPR,  group=Model)) +
    geom_point(aes(color=Model,shape=Model),size=2) + 
    geom_line(aes(color=Model)) +
    facet_grid(. ~ Representation) +
    ggtitle("True Positive Rate") +
    theme(plot.title = element_text(hjust = 0.5))
  plot_tpr + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))
  #TNR
  plot_tnr <- ggplot(temp_data, aes(x=Threshold, 
                                    y=TNR,  group=Model)) +
    geom_point(aes(color=Model,shape=Model),size=2) + 
    geom_line(aes(color=Model)) +
    facet_grid(. ~ Representation) +
    ggtitle("True Negative Rate") +
    theme(plot.title = element_text(hjust = 0.5))
  plot_tnr + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))
  #PPV
  plot_ppv <- ggplot(temp_data, aes(x=Threshold, 
                                    y=PPV,  group=Model)) +
    geom_point(aes(color=Model,shape=Model),size=2) + 
    geom_line(aes(color=Model)) +
    facet_grid(. ~ Representation) +
    ggtitle("Positive Predictive Value") +
    theme(plot.title = element_text(hjust = 0.5))
  plot_ppv + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  #NPV
  plot_npv <- ggplot(temp_data, aes(x=Threshold, 
                                    y=NPV,  group=Model)) +
    geom_point(aes(color=Model,shape=Model),size=2) + 
    geom_line(aes(color=Model)) +
    facet_grid(. ~ Representation) +
    ggtitle("Negative Predictive Value") +
    theme(plot.title = element_text(hjust = 0.5))
  plot_npv + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  #NPV
  plot_f1 <- ggplot(temp_data, aes(x=Threshold, 
                                   y=F1,  group=Model)) +
    geom_point(aes(color=Model,shape=Model),size=2) + 
    geom_line(aes(color=Model)) +
    facet_grid(. ~ Representation) +
    ggtitle("F1 Score") +
    theme(plot.title = element_text(hjust = 0.5))
  plot_f1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  multiplot(plot_accuracy,plot_tpr,plot_tnr,plot_ppv,plot_npv,plot_f1,cols=2)
}


#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}






