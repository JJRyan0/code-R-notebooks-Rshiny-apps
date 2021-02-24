
#https://www.kaggle.com/c/bnp-paribas-cardif-claims-management

svmdataset <- read.csv("C:/Users/John/Documents/AI/Trainsvm.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
svmdataset
summary(svmdataset)
# Build the training + tests.
set.seed(1234) 
svmnobs <- nrow(svmdataset)
svmsample <- svmtrain <- sample(nrow(svmdataset), 0.7*svmnobs)
svmvalidation <- sample(setdiff(seq_len(nrow(svmdataset)), svmtrain), 0.15*svmnobs) # 112 observations
svmtest <- setdiff(setdiff(seq_len(nrow(svmdataset)), svmtrain), svmvalidation) # 114 observations

# The following variable selections.

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

#Program to prepare date for input to SVM model
#JJryan0

#KNN - normailize

summary(data[c(1:133)])
       ID             Target             v1               v2         v3     
 Min.   :  0.00   Min.   :0.0000   Min.   :0.0000   Min.   : 1.330    :108  
 1st Qu.: 49.25   1st Qu.:0.5000   1st Qu.:0.9473   1st Qu.: 5.354   C:189  
Median : 98.50   Median :1.0000   Median :1.3956   Median : 7.175          
 Mean   : 98.57   Mean   :0.7475   Mean   :1.6619   Mean   : 7.425          
 3rd Qu.:147.75   3rd Qu.:1.0000   3rd Qu.:2.1990   3rd Qu.: 9.247          
 Max.   :201.00   Max.   :1.0000   Max.   :5.1633   Max.   :14.960          
 NA's   :99       NA's   :198      NA's   :187      NA's   :187             
       v4              v5               v6               v7        
 Min.   :1.182   Min.   : 2.487   Min.   :0.4855   Min.   :0.9749  
 1st Qu.:3.645   1st Qu.: 7.186   1st Qu.:2.0345   1st Qu.:2.1097  
 Median :4.325   Median : 8.547   Median :2.4095   Median :2.4788  
 Mean   :4.275   Mean   : 8.383   Mean   :2.4677   Mean   :2.5102  
 3rd Qu.:4.993   3rd Qu.: 9.550   3rd Qu.:2.9104   3rd Qu.:2.8767  
 Max.   :8.599   Max.   :14.687   Max.   :4.3227   Max.   :4.0782  
 NA's   :187     NA's   :181      NA's   :187      NA's   :187     
       v8                 v9              v10               v11       
 Min.   : 0.00716   Min.   : 3.503   Min.   :0.06565   Min.   :13.14  
 1st Qu.: 0.08497   1st Qu.: 7.694   1st Qu.:1.05033   1st Qu.:14.97  
Median : 0.32945   Median : 8.980   Median :1.31291   Median :15.47  
 Mean   : 1.71100   Mean   : 8.986   Mean   :1.88758   Mean   :15.44  
 3rd Qu.: 1.74514   3rd Qu.:10.426   3rd Qu.:2.09519   3rd Qu.:15.91  
 Max.   :17.29778   Max.   :12.667   Max.   :7.26477   Max.   :17.04  
 NA's   :181        NA's   :187      NA's   :99        NA's   :187   

> normalize <- function (x) { + return ( (x - min(x))/(max(x) - min(x)))}
> normalize(c(1:133))
  [1] 0.000000000 0.007575758 0.015151515 0.022727273 0.030303030 0.037878788 0.045454545 0.053030303 0.060606061 0.068181818 0.075757576 0.083333333 0.090909091
 [14] 0.098484848 0.106060606 0.113636364 0.121212121 0.128787879 0.136363636 0.143939394 0.151515152 0.159090909 0.166666667 0.174242424 0.181818182 0.189393939
 [27] 0.196969697 0.204545455 0.212121212 0.219696970 0.227272727 0.234848485 0.242424242 0.250000000 0.257575758 0.265151515 0.272727273 0.280303030 0.287878788
 [40] 0.295454545 0.303030303 0.310606061 0.318181818 0.325757576 0.333333333 0.340909091 0.348484848 0.356060606 0.363636364 0.371212121 0.378787879 0.386363636
 [53] 0.393939394 0.401515152 0.409090909 0.416666667 0.424242424 0.431818182 0.439393939 0.446969697 0.454545455 0.462121212 0.469696970 0.477272727 0.484848485
 [66] 0.492424242 0.500000000 0.507575758 0.515151515 0.522727273 0.530303030 0.537878788 0.545454545 0.553030303 0.560606061 0.568181818 0.575757576 0.583333333
 [79] 0.590909091 0.598484848 0.606060606 0.613636364 0.621212121 0.628787879 0.636363636 0.643939394 0.651515152 0.659090909 0.666666667 0.674242424 0.681818182
 [92] 0.689393939 0.696969697 0.704545455 0.712121212 0.719696970 0.727272727 0.734848485 0.742424242 0.750000000 0.757575758 0.765151515 0.772727273 0.780303030
[105] 0.787878788 0.795454545 0.803030303 0.810606061 0.818181818 0.825757576 0.833333333 0.840909091 0.848484848 0.856060606 0.863636364 0.871212121 0.878787879
[118] 0.886363636 0.893939394 0.901515152 0.909090909 0.916666667 0.924242424 0.931818182 0.939393939 0.946969697 0.954545455 0.962121212 0.969696970 0.977272727
[131] 0.984848485 0.992424242 1.000000000
