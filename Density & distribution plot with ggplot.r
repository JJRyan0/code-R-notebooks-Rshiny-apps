#Creating Density & Distribution plots

library(ggplot2)
d %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
    facet_wrap(~ key, scales = "free") +   # In separate panels
    geom_density() 

#option 2
# Multiple histograms
par(mfrow=c(3, 3))
colnames <- dimnames(crime.new)[[2]]
for (i in 2:8) {
    hist(crime[,i], xlim=c(0, 3500), breaks=seq(0, 3500, 100), main=colnames[i], probability=TRUE, col="gray", border="white")
}


# Density plot
par(mfrow=c(3, 3))
colnames <- dimnames(crime.new)[[2]]
for (i in 2:8) {
    d <- density(crime[,i])
    plot(d, type="n", main=colnames[i])
    polygon(d, col="red", border="gray")
}

# Histograms and density lines
par(mfrow=c(3, 3))
colnames <- dimnames(crime.new)[[2]]
for (i in 2:8) {
    hist(crime[,i], xlim=c(0, 3500), breaks=seq(0, 3500, 100), main=colnames[i], probability=TRUE, col="gray", border="white")
    d <- density(crime[,i])
    lines(d, col="red")
}

# Box plots for all crime rates
boxplot(crime.new[,-1], horizontal=TRUE, main="Crime Rates in US")
