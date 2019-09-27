NCI - Two-way ANOVA lab

John Ryan 2016

#Let’s work with a built in data set:
data( ToothGrowth )
#Don’t ask ‘what is this?’ – check!
help(ToothGrowth)
#(We are entering the wonderful world of ‘The Effect of Vitamin C on Tooth Growth in Guinea Pigs’).
head( ToothGrowth )

#We can actually do something useful with plot here:
plot( len ~ supp + dose, data=ToothGrowth)

#This is creating two plots so lets make space for them:
par(mfrow=c(2,1))
plot( len ~ supp + dose, data=ToothGrowth)

#Why is the second plot different to the first? Let’s investigate…
str(ToothGrowth)

#How many factors should we have?
plot( len ~ supp + factor( dose), data=ToothGrowth)

#Ok, we should check the interaction plots first but let’s look at some different functions for performing the two way ANOVA:
teeth.aov <- aov( len ~ supp * factor(dose), data=ToothGrowth)
summary( teeth.aov )

#Is there an interaction? Check the appropriate p-value.
interaction.plot( ToothGrowth$supp, factor( ToothGrowth$dose ), ToothGrowth$len)
interaction.plot( factor( ToothGrowth$dose ),ToothGrowth$supp, ToothGrowth$len)

#Ok, we can also get a two-way ANOVA with a the function we used to build regression models. Compare the output of  ‘summary( teeth.aov )’ to ‘anova( teeth.lm )’ after building the lm model:
teeth.lm <- lm( len ~ supp + factor(dose) + supp * factor(dose), data=ToothGrowth)
anova( teeth.lm )

#Notice that in aov we use ‘supp * factor(dose)’. Now see what happens when you use ‘supp + factor(dose)’:
teeth.aov <- aov( len ~ supp + factor(dose), data=ToothGrowth)
summary( teeth.aov )
