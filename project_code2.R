# Course Project - part 2: Basic Inferential Data Analysis

library(datasets)
library(ggplot2)
library(dplyr)


# 1. Load the ToothGrowth data and perform some basic exploratory data analyses
# 2. Provide a basic summary of the data.


data <- ToothGrowth
# info: https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html

# data about toothgrowth in 60 subjects that were given vitamin C
# len: tooth length
# supp: method of vitamin C delivery
    # VC: ascorbic acid
    # OJ: orange juice
# dose: dose level of vitamin C (in mg/day)
    # .5, 1, 2

summary(data)

# how many measurements per supp & dose
table(data$supp, data$dose)

# compute mean and sd per supp & dose
data %>% group_by(supp, dose) %>% summarise(mean = mean(len), sd = sd(len))

# boxplots for each supp & dose
ggplot(data, aes(x=supp, y=len)) + geom_boxplot(aes(color=supp)) + facet_grid(. ~ dose) +
    labs(y='length', title='Tooth growth by dose and delivery method') +
    theme(legend.position = 'none')
    # axis...x in theme removes numbers and label in xaxis




# 3. Use confidence intervals and/or hypothesis tests to compare tooth growth
# by supp and dose. (Only use the techniques from class, even if there's other
# approaches worth considering)

# 4. State your conclusions and the assumptions needed for your conclusions.


# TEST1: mean(length) by supp

# From the boxplots it looks like the average length on subjects with supp=VC
# is smaller than on subjects with supp=OJ.

# H0: mu_OJ = mu_VC
# H0: mu_OJ > mu_VC

# Assume tooth growth length follows a normal dist. in OJ and VC populations,
# with different (unknown) variances.
# Perform 2-sample t-test for diff variances (Welch's test):

t.test(len ~ supp, data=data, paired=FALSE, var.equal=FALSE,
       alternative='greater')
    # p-value=.03



# TEST2: for each dose, mean(length) by supp

# Let's perform 3 hypothesis tests as before, for each value of dose:

lapply( split(data, data$dose) , function(df)
    t.test(len ~ supp, data=df, paired=FALSE, var.equal=FALSE,
           alternative='greater')$p.value
)

# results:

# $`0.5`
#
# Welch Two Sample t-test
#
# data:  len by supp
# t = 3.1697, df = 14.969, p-value = 0.003179
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#     2.34604     Inf
# sample estimates:
#     mean in group OJ mean in group VC 
# 13.23             7.98

# $`1`
# 
# Welch Two Sample t-test
# 
# data:  len by supp
# t = 4.0328, df = 15.358, p-value = 0.0005192
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#     3.356158      Inf
# sample estimates:
#     mean in group OJ mean in group VC 
# 22.70            16.77 

# $`2`
# 
# Welch Two Sample t-test
# 
# data:  len by supp
# t = -0.046136, df = 14.04, p-value = 0.5181
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#     -3.1335     Inf
# sample estimates:
#     mean in group OJ mean in group VC 
# 26.06            26.14 


# For doses .5 and 1, there's significant evidence that method OJ is
# more efficient than method VC.
# No evidence for dose 2
# For dose=2, mu_OJ and mu_VC are too close to conclude they're different



# TEST 3: for each supp, mean(length) by dose

# However, it seems that on average, the tooth growth is higher for dose=2 then
# for dose=1 and lower for dose=1 

# H0: mu_1=mu_2
# H1: mu_1<mu_2

data2 <- data[data$dose %in% c(1,2), ]

lapply( split(data2, data2$supp) , function(df)
    t.test(len ~ dose, data=df, paired=FALSE, var.equal=FALSE,
           alternative='less')$p.value
)

# H0: mu_0.5=mu_1
# H1: mu_0.5<mu_1

data3 <- data[data$dose %in% c(.5,1),]

lapply( split(data3, data3$supp) , function(df)
    t.test(len ~ dose, data=df, paired=FALSE, var.equal=FALSE,
           alternative='less')$p.value
)



