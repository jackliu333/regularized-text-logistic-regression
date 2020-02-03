# Use tf-idf representation with review length
source("helper.R")
Sys.setlocale("LC_ALL", "English")

# Loop through all combinations of statistical representation and threholds
stat_representation <- c("FF","FP","TF-IDF")
thresholds <- seq(0.8,0.99,0.01)
result_all_insample <- NULL
result_all_outofsample <- NULL
for(stat in 1:3){
  #stat=2
  for(thres in 1:length(thresholds)){
    #thres=1
    current_repre <- stat_representation[stat]
    current_thres <- thresholds[thres]
    total.data <- process.data(current_repre,current_thres)
    model.cv <- train.model.cv(total.data$insample_data)
    row.names(model.cv) <- NULL
    model.opt <- apply.model.opt(total.data,model.cv)
    model.lr <- apply.model.lr(total.data,model.opt)
    model.svm <- apply.model.svm(total.data,model.lr)
    model.knn <- apply.model.knn(total.data,model.svm)
    model.nb <- apply.model.nb(total.data,model.knn)
    model.nb$Representation <- current_repre
    model.nb$Threshold <- current_thres
    
    temp_result_insample <- model.nb[[1]]
    temp_result_outofsample <- model.nb[[2]]
    temp_result_insample$Model <- c("SCAD","LR","SVM","KNN","NB")
    temp_result_insample$Representation <- current_repre
    temp_result_insample$Threshold <- current_thres
    row.names(temp_result_insample) <- NULL
    temp_result_outofsample$Model <- c("SCAD","LR","SVM","KNN","NB")
    temp_result_outofsample$Representation <- current_repre
    temp_result_outofsample$Threshold <- current_thres
    row.names(temp_result_outofsample) <- NULL
    result_all_insample <- rbind(result_all_insample,temp_result_insample)
    result_all_outofsample <- rbind(result_all_outofsample,temp_result_outofsample)
  }
}

plot_insample(result_all_insample)
plot_outofsample(result_all_outofsample)
