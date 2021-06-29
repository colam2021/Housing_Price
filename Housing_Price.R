#' ---
#' ####
#' title: Factors that affecting housing price in the Saratoga County, New York.
#' author: C.O.Lam
#' date: June 2021
#' ---

#+
library(easypackages)
libraries("tidyverse", "VIM", "skimr", "mosaicData", "ggpubr")

data("SaratogaHouses")
attach(SaratogaHouses)

options(scipen = 999)
#'
#' ### Data Preparation 
#' 
#' Checking Data
#+
chk = is.na(SaratogaHouses)
table(chk)
#' Apparently, there is no Null in the data set.
#+
missing_visual<-summary(aggr(SaratogaHouses, sortVar=TRUE))$combinations

#'  There is no missing data in the data set.
#' Other data issue?
#+
  str(SaratogaHouses)
#'  
#' Each variable has the correct data type 
#'   
#' ### DEA 
#+  
  summary (SaratogaHouses[1])
  skim (SaratogaHouses)
  
#'  There are 16 variables and 1728 observations in the data set. 
#'  The data set contains 3 types of data: 6 variables are factors, 2 are numerics, and 
#'  the rest are integer.
  
#'  In this data set, price is likely be the dependable variable.
#'  In the Saratoga County, New York, the minimum house price is $5000 and the maximum is $775000.  
#'  On average, the house price is $211967, and the median housing price is $189900.  
#'  
#'  Next, we will look at the histogram. Here, we will use the Sturges method to find out the "breaks".
#'  From the breaks, we will know the number of bin and the binwidth. 
#'   
#+  
  breaks <- pretty(range(SaratogaHouses$price),
                   n = nclass.Sturges(SaratogaHouses$price),
                   min.n = 1)
  
  ggplot(SaratogaHouses, aes(x=price)) + 
     geom_histogram(binwidth = 50000,  boundary =25000)+
     stat_bin(binwidth = 50000, geom="text", aes(label=..count..) , 
              vjust = -1) +
     scale_x_continuous(breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 
                                 350000, 400000, 450000, 500000, 550000, 600000, 
                                 650000,700000, 750000, 800000), 
                        labels = c("0", "50000", "100000", "150000", "200000", "250000", "300000", 
                                   "350000", "400000", "450000", "500000", "550000", "600000", 
                                   "650000", "700000", "750000", "800000") )
  
#'  The distribution of price is right-skewed. Most housing prices are within $100k to $250k.
#'  The most common housing price is around $150k with 477 houses within that price range.
#'  
#' ### The impact of fuel type on the price  
#'  
#+
  ggplot(SaratogaHouses, aes(x= fuel, y=price))+
    geom_boxplot( ) 

#' Overall, the three boxplots shares some similarities. For instance, 
#' they are slightly skewed to the right (positively skewed) based on the position of the median within the interquartile range (the box). 
#' They all have a lot of outliers beyond the top whisker. 
#' 
#' From the plots, it seems that the median housing prices are different among the three fuel type groups.
#' The median house price within the gas group (the first boxplot on the left) seems 
#' to be the highest among the three, followed by oil group (boxplot on the right) and electric group (boxplot in the middle).
#' Nevertheless, an ANOVA is needed to determine if the difference exists.   

#+  
AOV1 <- aov(price ~ fuel, data = SaratogaHouses)
summary(AOV1)

#' The null hypothesis in an Anova states that there is no significant difference among the group means. 
#' In the SaratogaHouses case, Ho means the three fuel types will have the same impact on house prices.
#' 
#' From the AOV1 summary, the p-value for fuel is <0.0000000000000002 which is significantly smaller than the critical value of 0.05 (alpha).
#' Therefore, we will reject the null hypothesis (there is no difference among the group means).  
#' We will accept the alternative hypothesis that there is a difference at least among two group means
#' in terms of their impacts on house price.
#' 
#' Since fuel type has a significantly impact on the housing price in Saratoga,
#' we may want to find out which fuel type(s) has impact on housing price.
#+
SaratogaHouses %>% group_by(fuel) %>% summarise(mean(price))
pairwise.t.test(x=SaratogaHouses$price, g=SaratogaHouses$fuel, p.adj="bonf")
 
#' The mean housing prices of gas, electric, and oil are $228535, $164938, $188734 respectively. 
#' The results show that there is statistically significant difference between 
#' gas and electric, gas and oil, and electric and oil.
#' 
#' ### Waterfront location and housing price 
#' 
#' Another feature that might have impact on housing price is waterfront location.
#' Make a box plot of price by waterfront type using boxplot()
#+ 
ggplot(SaratogaHouses, aes(x=waterfront, y=price))+
  geom_boxplot()
#' The two boxplots do not look the same. The boxplot of the Yes (Waterfront location) group is clearly right-skewed 
#' with one outlier beyond the upper whisker. Its median housing price is higher than the No waterfront location group.
#' However, there are lots of price variation on the high end among the houses within the no waterfront group.

#' To find out the impact of waterfront location on housing price, we will first do a t-test,
#' and then we will use an ANOVA.
#' 
#' ### Assumptions of Normality and Equal Variance
#' 
#' In a t-test, the null hypothesis states that there is no difference among the means. 
#' In other words, Group A and Group B have no difference.
#+
PWithWater <- SaratogaHouses$price[SaratogaHouses$waterfront== "Yes"]
PWoutWater <-SaratogaHouses$price[SaratogaHouses$waterfront=="No"]

length(PWithWater)
length(PWoutWater)

#' There are only 15 houses with waterfront location.
#' But there are 1713 houses without waterfront location.

#' Using PWithWater and PWoutWater, perfrom a t-test using t.test()
#+
ttest <- t.test(PWithWater, PWoutWater)

#' the p-value is 0.001119 which suggests there is a statistically significant difference among the means.
#' Now, let's test the assumptions.
#' **Normality Yes -> Equal Variance test; Normality No -> Wilcoxon**
#' **Equal Variance Yes -> standard t-test;  No -> Welch t-test**
#' 
#' **Test of Normality**
#+ 
W_Y <- SaratogaHouses %>% filter ( waterfront =="Yes") 
W_N <-  SaratogaHouses %>% filter ( waterfront =="No") 

#+
ggplot(W_Y, aes(price)) +
  geom_histogram(fill = "white", color = "grey30") 

ggplot(W_N, aes(price)) +
  geom_histogram(fill = "white", color = "grey30") 

ggdensity(W_Y$price, fill = "lightgray")
ggdensity(W_N$price, fill = "lightgray")

ggqqplot(W_Y$price)
ggqqplot(W_N$price)
 
#' The non-waterfront group clearly violates the normality assumption.
#' 
#' **Test of Equal Variance**
#' 
#' If the variances of the two groups of data are equal, the ratio will be 1, and vice versa.
#' In the var test, the null hypothesis is that the variances are equal.
#'
#+ 
var(PWithWater) / var(PWoutWater)
var.test(PWithWater, PWoutWater)

#' The ratio is 2.57 and the p-value from the var.test. That means 
#' the null hypothesis of equal variances is rejected.
#' 
#' In summary, the 2 groups of data violates the normality and equal variance assumptions.
#' Therefore, we have to use the non-parametric Wilcox test.
#' 
#' **Wilcox Test**
#+  
wilcox.test(price ~ waterfront, data= SaratogaHouses ) 

#' Like the standard 2 samples t-test, the non-parametric Wilcox also returns a very low p-value of 0.000007731,
#' which suggests there is a statistically significant difference among the means.

#' **ANOVA**
#' Let's use an ANOVA to look into the impact of waterfront location on housing price at Saratoga.
#+
AOV2 <- aov(price ~ waterfront, data = SaratogaHouses)
summary(AOV2)

#' The p-value is 0.000000000121 which is significantly smaller than the critical value of 0.05 (alpha).
#' It suggests that there is a statistically significant difference at least among two group means
#' in terms of their impacts on house price.
#' In summary, a house with waterfront has a significantly impact on the price of house in Saratoga.
#+
SaratogaHouses %>% group_by(waterfront) %>% summarise(mean(price))

#' The mean housing price of waterfront location is $373992 whereas 
#' the mean housing price of non-waterfront location is $210548
#' 
#' Since there is only 2 groups within the waterfront, it is reasonable to say that 
#' there is a statistically significant difference between the two groups.
#' 
#' 
#' ### Will the fuel type and waterfront location affect housing price even more? 
#+
AVO3 <- aov(price ~ waterfront + fuel, data = SaratogaHouses)
summary(AVO3)

#' The p-value of waterfront is 0.0000000000229, which is significantly smaller than the critical value of 0.05 (alpha).
#' In other words, waterfront will affect housing price in Saratoga.   

#' The p-value of fuel type is < 0.0000000000000002, which is significantly smaller than the critical value of 0.05 (alpha).
#' In other words, fuel type will affect housing price in Saratoga.
#' Will there be an interaction effect?
#+
interaction.plot(SaratogaHouses$waterfront,SaratogaHouses$fuel,SaratogaHouses$price)

#' The interaction plot shows that the oil line and the electric line cross each other.
#' It seems to suggest that there is an interaction effect.
#' Is it really the case?
#+
AVO4 <- aov(price ~ waterfront * fuel, data = SaratogaHouses)
summary(AVO4)


#' The results shows that the p-values for both  waterfront and fuel type are
#' very small (0.0000000000218 and < 0.0000000000000002 ).
#' However, the p-value of the interaction effect is 0.0513. Since it is bigger than 0.05, it is not 
#' significant at the 0.05 significant level. In other words, there is no interaction effect
#' on the housing price in Saratoga.







