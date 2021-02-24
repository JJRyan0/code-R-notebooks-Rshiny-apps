
#Simple Multilayer Neural net using Oceanic wave data to predict the 
#"PeakPeriodSec","PeakDirectionDegrees", "UpcrossPeriodSec", "SignificantWaveHeightcm",
#"TemperatureDegrees", "Hmax_cm", "Thmax_sec", "MeanCurDirToDegrees",
#"MeanCurSpeedMperSec"


building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 


# Load the data.

crs$dataset <- read.csv("file:///C:/Clusters/WaveTrain.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
summary(crs$dataset))
 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 847993 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 593595 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 127198 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 127200 observations

# The following variable selections have been noted.

crs$input <- c("PeakPeriodSec", "PeakDirectionDegrees", "UpcrossPeriodSec", "SignificantWaveHeightcm",
               "eaTemperatureDegrees", "Hmax_cm", "Thmax_sec", "MeanCurDirToDegrees",
               "MeanCurSpeedMperSec", "Year", "Month")

crs$numeric <- c("PeakPeriodSec", "PeakDirectionDegrees", "UpcrossPeriodSec", "SignificantWaveHeightcm",
                 "eaTemperatureDegrees", "Hmax_cm", "Thmax_sec", "MeanCurDirToDegrees",
                 "MeanCurSpeedMperSec", "Year", "Month")

crs$categoric <- NULL

crs$target  <- "weekday"
crs$risk    <- NULL
crs$ident   <- "id"
crs$ignore  <- NULL
crs$weights <- NULL


# Load the data.

crs$dataset <- read.csv("file:///C:/Users/John/Documents/Big Data/trainwave.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

 

# Build the training/validate/test datasets.

set.seed(40) 
crs$nobs <- nrow(crs$dataset) # 169599 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 118719 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 25439 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 25441 observations

# The following variable selections have been noted.

crs$input <- c("X0", "X1", "X2", "X3",
               "X4", "X5", "X6", "X7",
               "X8", "X9", "X10")

crs$numeric <- c("X0", "X1", "X2", "X3",
                 "X4", "X5", "X6", "X7",
                 "X8", "X9", "X10")

crs$categoric <- NULL

crs$target  <- "X11"
crs$risk    <- NULL
crs$ident   <- "id"
crs$ignore  <- NULL
crs$weights <- NULL

 

# Build the training/validate/test datasets.

set.seed(40) 
crs$nobs <- nrow(crs$dataset) # 169599 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 118719 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 25439 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 25441 observations

# The following variable selections have been noted.

crs$input <- c("X0", "X1", "X2", "X3",
               "X4", "X5", "X6", "X7",
               "X8", "X9", "X10", "X11")

crs$numeric <- c("X0", "X1", "X2", "X3",
                 "X4", "X5", "X6", "X7",
                 "X8", "X9", "X10", "X11")

crs$categoric <- NULL

crs$target  <- NULL
crs$risk    <- NULL
crs$ident   <- "id"
crs$ignore  <- NULL
crs$weights <- NULL



# Build the training/validate/test datasets.

set.seed(40) 
crs$nobs <- nrow(crs$dataset) # 169599 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 118719 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 25439 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 25441 observations

# The following variable selections have been noted.

crs$input <- c("X0", "X1", "X2", "X3",
               "X4", "X5", "X6", "X7",
               "X8", "X9", "X11")

crs$numeric <- c("X0", "X1", "X2", "X3",
                 "X4", "X5", "X6", "X7",
                 "X8", "X9", "X11")

crs$categoric <- NULL

crs$target  <- "X10"
crs$risk    <- NULL
crs$ident   <- "id"
crs$ignore  <- NULL
crs$weights <- NULL



# Build the training/validate/test datasets.

set.seed(40) 
crs$nobs <- nrow(crs$dataset) # 169599 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 118719 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 25439 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 25441 observations

# The following variable selections have been noted.

crs$input <- c("X0", "X2", "X3", "X4",
               "X5", "X6", "X7", "X8",
               "X9", "X10", "X11")

crs$numeric <- c("X0", "X2", "X3", "X4",
                 "X5", "X6", "X7", "X8",
                 "X9", "X10", "X11")

crs$categoric <- NULL

crs$target  <- "X1"
crs$risk    <- NULL
crs$ident   <- "id"
crs$ignore  <- NULL
crs$weights <- NULL



# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(X1 ~ .,
                 data=crs$dataset[crs$sample,c(crs$input, crs$target)],
                 size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
            paste(crs$nnet$n, collapse="-"),
            length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
            paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
            names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
            sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')



# Evaluate model performance. 

# NNET: Generate a Predicted v Observed plot for nnet model on trainwave.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Obtain the observed output for the dataset.

obs <- subset(crs$dataset[crs$test, c(crs$input, crs$target)], select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(X1=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="X1", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
      Neural Net Model
      trainwave.csv [test]",
      sub=paste("Model", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()



# Evaluate model performance. 

# NNET: Generate a Predicted v Observed plot for nnet model on trainwave.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Obtain the observed output for the dataset.

obs <- subset(crs$dataset[crs$test, c(crs$input, crs$target)], select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(X1=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="X1", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
      Neural Net Model
      trainwave.csv [test]",
      sub=paste("Model", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()



# Evaluate model performance. 

# NNET: Generate a Predicted v Observed plot for nnet model on trainwave.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Obtain the observed output for the dataset.

obs <- subset(crs$dataset[crs$test, c(crs$input, crs$target)], select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(X1=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="X1", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
      Neural Net Model
      trainwave.csv [test]",
      sub=paste("Model", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()


# Score a dataset. 

# Obtain predictions for the Neural Net model on trainwave.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- crs$dataset[crs$test,]

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="C:\Users\John\Documents\Big Data\trainwave_test_score_all.csv", row.names=FALSE)

library(devtools)
source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')
install.packages("nnet")
library(nnet)
#plot each model
plot.nnet(crs$nnet)
