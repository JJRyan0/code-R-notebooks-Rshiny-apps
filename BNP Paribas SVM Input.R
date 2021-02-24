svmdataset <- read.csv("C:/Users/John/Documents/AI/Trainsvm.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
svmdataset
summary(svmdataset)
# Build the training + tests.
set.seed(1234) 
svmnobs <- nrow(svmdataset)
svmsample <- svmtrain <- sample(nrow(svmdataset), 0.7*svmnobs)
svmvalidation <- sample(setdiff(seq_len(nrow(svmdataset)), svmtrain), 0.15*svmnobs) # 112 observations
svmtest <- setdiff(setdiff(seq_len(nrow(svmdataset)), svmtrain), svmvalidation) # 114 observations

# The following variable selections have been noted.

svm_input <- c("v1", "v2", "v4", "v5",
               "v6", "v7", "v8", "v9",
               "v10", "v11", "v12", "v13",
               "v14", "v15", "v16", "v17",
               "v18", "v19", "v20", "v21",
               "v22", "v23", "v24", "v25",
               "v26", "v27", "v28", "v29",
               "v30", "v31", "v32", "v33",
               "v34", "v35", "v36", "v37",
               "v38", "v39", "v40", "v41",
               "v42", "v43", "v44", "v45",
               "v46", "v47", "v48", "v49",
               "v50", "v51", "v52", "v53",
               "v54", "v55", "v56", "v57",
               "v58", "v59", "v60", "v61",
               "v62", "v63", "v64", "v65",
               "v66", "v67", "v68", "v69",
               "v70", "v71", "v72", "v73",
               "v74", "v75", "v76", "v77",
               "v78", "v79", "v80", "v81",
               "v82", "v83", "v84", "v85",
               "v86", "v87", "v88", "v89",
               "v90", "v91", "v92", "v93",
               "v94", "v95", "v96", "v97",
               "v98", "v99", "v100", "v101",
               "v102", "v103", "v104", "v105",
               "v106", "v107", "v108", "v109",
               "v110", "v111", "v112", "v113",
               "v114", "v115", "v116", "v117",
               "v118", "v119", "v120", "v121",
               "v122", "v123", "v124", "v125",
               "v126", "v127", "v128", "v129",
               "v130", "v131")

svm_numeric <- c("v1", "v2", "v4", "v5",
                 "v6", "v7", "v8", "v9",
                 "v10", "v11", "v12", "v13",
                 "v14", "v15", "v16", "v17",
                 "v18", "v19", "v20", "v21",
                 "v23", "v25", "v26", "v27",
                 "v28", "v29", "v32", "v33",
                 "v34", "v35", "v36", "v37",
                 "v38", "v39", "v40", "v41",
                 "v42", "v43", "v44", "v45",
                 "v46", "v48", "v49", "v50",
                 "v51", "v53", "v54", "v55",
                 "v57", "v58", "v59", "v60",
                 "v61", "v62", "v63", "v64",
                 "v65", "v67", "v68", "v69",
                 "v70", "v72", "v73", "v76",
                 "v77", "v78", "v80", "v81",
                 "v82", "v83", "v84", "v85",
                 "v86", "v87", "v88", "v89",
                 "v90", "v92", "v93", "v94",
                 "v95", "v96", "v97", "v98",
                 "v99", "v100", "v101", "v102",
                 "v103", "v104", "v105", "v106",
                 "v108", "v109", "v111", "v114",
                 "v115", "v116", "v117", "v118",
                 "v119", "v120", "v121", "v122",
                 "v123", "v124", "v126", "v127",
                 "v128", "v129", "v130", "v131")

svm_categoric <- c("v22", "v24", "v30", "v31",
                   "v47", "v52", "v56", "v66",
                   "v71", "v74", "v75", "v79",
                   "v91", "v107", "v110", "v112",
                   "v113", "v125")

svmtarget  <- "target"
svmrisk    <- NULL
svmidentity <- "ID"
svmignore  <- "v3"
svmweight <- NULL


install.packages("kernlab")
library(kernlab, quietly=TRUE)

# Building the SVM model.

set.seed(1234)
train_ksvm <- ksvm(as.factor(target) ~ .,data=svmdataset[svmtrain,c(svm_input, svmtarget)], kernel="rbfdot", prob.model=TRUE)

train_ksvm
install.packages("caret")
library(caret)
confusionMatrix(train_ksvm)

# Evaluation
# Predicted v Observed plot for ksvm model on Trainsvm.csv [validate].

svm_predict <- kernlab::predict(train_ksvm, newdata=na.omit(svmdataset[svmvalidation, c(svm_input, svmtarget)]), type="probabilities")[,2]

omitted <- attr(na.omit(svmdataset[svmvalidation, c(svm_input, svmtarget)]), "na.action")

obs <- subset(svmdataset[svmvalidation,][-omitted,], select=svmtarget)

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(target=obs)
rownames(obs) <- obs.rownames

# Put observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=svm_predict))

# correlation.

fitcorrelation <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)
op <- par(c(lty="solid", col="red"))

plot(jitter(fitpoints[[1]]), fitpoints[[2]], asp=1, xlab="target", ylab="Predicted")

# linear model fit predicted VS observed.

predictfitline <- lm(fitpoints[,2] ~ fitpoints[,1])

abline(predictfitline)

par(c(lty="dashed", col="black"))
abline(0, 1)

# pseudo R-square

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

title (main="Predicted vs. Observed
      SVM Model", sub=paste("BNP Paribas"))
grid()

svm_predict <- kernlab::predict(trainksvm, newdata=na.omit(svmdataset[svmvalidation, c(svm_input, svmtarget)]), type="probabilities")[,2]
evaluate <- evaluateRisk(svm_predict, na.omit(svmdataset[svmvalidation, c(svm_input, svmtarget)])$target)
print(riskchart(svm_predict, na.omit(svmdataset[svmvalidation, c(svm_input, svmtarget)])$target, title="SVM Performance Chart", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))






