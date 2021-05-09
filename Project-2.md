Modeling: Governor’s Party and State Economic Characteristics
================
Alisha Ramdeep
4/17/2021

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

## Introduction

The title of this project is “Modeling:Governor’s Party and State
Economic Characteristics.” I wanted to explore the relationship between
the political party of a state’s governor (or in Washington D.C.’s case,
mayor) and state spending, state collection, median annual household
income, and Supplemental Nutrition Assistance Program (SNAP) usage. I
was really curious about how state spending and taxation affected the
welfare of the population and I thought SNAP would be a good indicator
of welfare, with a lower percentage of users being better for the state.
I also know that spending is an issue often debated between political
parties with Republicans favoring less spending than Democrats. I
acquired governor party, income, spending, collection, and SNAP data
from the Kaiser Family Foundation, while population data was taken from
the US Census Bureau. I expect to see higher taxation among Democratic
states and that it will be difficult to predict goernor’s political
party based on state economic characteristics.

## Tidying

The data is tidy. The 50 states and D.C. are my observations and each
state has its own row. Further, the other variables (governor’s party,
median annual household income, state collection, state spending, SNAP
users and population) each have their own column.

## Importing Datasets

``` r
#Importing all individual datasets
library(readxl)

#Collected from Kaiser Family Foundation state demographic and economic data. https://www.kff.org/state-category/demographics-and-the-economy/
gov <- read_excel(('Governor.xlsx'))
income<- read_excel('Income.xlsx')
tax<- read_excel('Tax.xlsx')
spend<- read_excel('Spending.xlsx')
snap<- read_excel('SNAP.xlsx')

#Collected from US Census Bureau.https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html
pop <- read_excel('Population.xlsx')
```

## Joining Datasets

``` r
#Joining all datasets. All datasets have 'State' in common.
library(dplyr)
data <- left_join(gov, left_join(income, left_join(tax, left_join(spend, left_join(snap, pop,  by = 'State'), by = 'State'), by = 'State'), by = 'State'), by = 'State')

#Creating percentage of average monthly SNAP users variable
State <- data$State
Party <- data$`Governer Political Affiliation`
data1 <- suppressWarnings(data %>% na.omit() %>% select(3:7) %>% rename(SNAP = `Average Monthly SNAP Participants`)%>% mutate_if(is.character, as.numeric))
data1$State<-State
data1$Party<-Party
data1 <- data1 %>% relocate(State, Party)
data1 <- data1 %>% mutate(PercentSnap = (SNAP/Population)*100) %>% select(-6, -7)
head(data1)
```

    ## # A tibble: 6 x 6
    ##   State  Party  `Median Annual H… `State Collectio… `Per Capita Sta… PercentSnap
    ##   <chr>  <chr>              <dbl>             <dbl>            <dbl>       <dbl>
    ## 1 Alaba… Repub…             51734              2262             5578       15.6 
    ## 2 Alaska Repub…             75463              2226            14016       12.6 
    ## 3 Arizo… Repub…             62055              2272             5238       11.6 
    ## 4 Arkan… Repub…             48952              3266             8519       12.3 
    ## 5 Calif… Democ…             80440              4424             6834       10.0 
    ## 6 Color… Democ…             77127              2599             6996        7.81

I chose left join because all of the datasets had the ‘State’ variable
in common and I wanted ‘State’ to be the left-most column. No cases were
dropped, as summary statistics and visualizations can be calcualted/made
with na.omit().

## EDA and Visualizations

``` r
# How do the means of state characteristics differ between Democrat and Republican?
data1 %>%
  group_by(Party) %>% 
  summarize(mean(`Median Annual Household Income`), mean(`State Collections per Capita`), mean(`Per Capita State Spending`,na.rm=T), mean(PercentSnap, na.rm=T))
```

    ## # A tibble: 2 x 5
    ##   Party  `mean(\`Median An… `mean(\`State Co… `mean(\`Per Capi… `mean(PercentSn…
    ## * <chr>               <dbl>             <dbl>             <dbl>            <dbl>
    ## 1 Democ…             69213.             3835.             7382.             12.5
    ## 2 Repub…             62221.             2839.             6374.             11.6

``` r
# How do the standard deviations of state characteristics differ between Democrat and Republican?
data1 %>%
  group_by(Party) %>% 
  summarize(sd(`Median Annual Household Income`), sd(`State Collections per Capita`), sd(`Per Capita State Spending`,na.rm=T), sd(PercentSnap, na.rm=T))
```

    ## # A tibble: 2 x 5
    ##   Party  `sd(\`Median Annu… `sd(\`State Coll… `sd(\`Per Capita… `sd(PercentSnap…
    ## * <chr>               <dbl>             <dbl>             <dbl>            <dbl>
    ## 1 Democ…             11162.             1815.             1831.             3.55
    ## 2 Repub…             10291.              914.             2212.             3.31

``` r
#Correlation Matrix
library(ggplot2)
library(tidyverse)
data_num <- data1 %>% select_if(is.numeric)%>% 
  rename(Income = `Median Annual Household Income`)%>%
  rename(Spending = `Per Capita State Spending`)%>%
  rename(Tax_Collection = `State Collections per Capita`)

cor(data_num, use = "pairwise.complete.obs") %>%
  as.data.frame %>%
  # Row names to explicit variable
  rownames_to_column %>%
  # All correlations in the same column
  pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>%
  # Order alphabetically
  ggplot(aes(rowname, factor(other_var, levels = rev(levels(factor(other_var)))), fill=correlation)) +
  # Heat map with geom_tile
  geom_tile() +
  scale_fill_gradient(low = "darkgoldenrod1", high = "darkorange") +
  # Overlay values
  geom_text(aes(label = round(correlation,2)), color = "black", size = 4) +
  # Give title and labels
  labs(title = "Correlation Heat Map for State Characterisitcs", x = "State Characteristics", y = "State Characteristics") 
```

![](Project-2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
data2 <- data1 %>%  
  rename(Income = `Median Annual Household Income`)%>%
  rename(Spending = `Per Capita State Spending`)%>%
  rename(Tax_Collection = `State Collections per Capita`)

# Represent the means by Party
library(wesanderson)
data2 %>% 
  na.omit() %>%
  select(Party,Income,Spending,Tax_Collection,PercentSnap) %>%
  pivot_longer(-1,names_to='DV', values_to='measure') %>%
  ggplot(aes(Party,measure,fill=Party)) +
  geom_bar(stat="summary", fun = "mean") +
  geom_errorbar(stat="summary", fun.data = "mean_se", width=.5) +
  facet_wrap(~DV, nrow=4) +
  coord_flip() + 
  ylab("") + 
  theme(legend.position = "none") +
  ggtitle('Mean of State Characterisitcs by Governor Party') +
  scale_fill_manual(values = wes_palette("Chevalier1", n = 2))
```

![](Project-2_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

The average median annual income for states with Democratic governors
was 69212.88 dollars, while for states with Republican governors the
mean was 62221.04 dollars. The standard deviation of median annual
income for states with Democratic Governors was 11161.94 while for
states with Republican governors the mean was 10290.52

The average state collections per capita for states with Democratic
governors was 3835.208 dollars while for states with Republican
governors the mean was 2839.185 dollars.The standard deviation for state
collections per capita was 1814.9026 for states with Democratic
governors and 914.4385 for states with Republican governors.

The average per capita state spending for states with Democratic
governors was 7382.043 while for states with Republican governors the
mean was 6374.333. The standard deviation for per capita state spending
was 1831.325 for states with Democratic Governors and 2211.539 for
states with Republican governors.

The average pecentage of SNAP users for states with Democratic governors
was 12.53056 while for states with Republican governors the mean was
11.58847. the standard deviation of percentage of SNAP users was
3.550207 for states with Democratic Governors and 3.312605 for states
with Republican governors.

## MANOVA

``` r
# Perform MANOVA with 2 response variables listed in cbind(). Using data2 in which names were shortened for convenience.
manova_data2 <- manova(cbind( Income, Spending, Tax_Collection, PercentSnap) ~ Party, data = data2)
# Output of MANOVA 
summary(manova_data2)
```

    ##           Df  Pillai approx F num Df den Df  Pr(>F)  
    ## Party      1 0.22862   3.2602      4     44 0.01996 *
    ## Residuals 47                                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# MANOVA was significant (p < 0.05), perform one-way ANOVA for each variable
summary.aov(manova_data2)
```

    ##  Response Income :
    ##             Df     Sum Sq   Mean Sq F value  Pr(>F)  
    ## Party        1  415577624 415577624  3.8638 0.05526 .
    ## Residuals   47 5055105925 107555445                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response Spending :
    ##             Df    Sum Sq  Mean Sq F value Pr(>F)
    ## Party        1  10835719 10835719  2.5594 0.1163
    ## Residuals   47 198983972  4233702               
    ## 
    ##  Response Tax_Collection :
    ##             Df   Sum Sq Mean Sq F value  Pr(>F)  
    ## Party        1  5603866 5603866  6.7844 0.01227 *
    ## Residuals   47 38821922  825998                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response PercentSnap :
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Party        1   7.54  7.5369  0.6433 0.4266
    ## Residuals   47 550.66 11.7161               
    ## 
    ## 2 observations deleted due to missingness

``` r
# State Tax Collection per Capita was significant. Post Hoc: 
pairwise.t.test(data2$Tax_Collection, data2$Party, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  data2$Tax_Collection and data2$Party 
    ## 
    ##            Democrat
    ## Republican 0.015   
    ## 
    ## P value adjustment method: none

``` r
#Bonferroni's Correction
0.05/6
```

    ## [1] 0.008333333

The assumptions for MANOVA are random sample and independent
observations, multivariate normality of the numeric response variables,
homogeneity of within-groups covariance matrices, variance for each
response across groups are equal and covariance between any 2 responses
are equal, linear relationships among response variables but no
multicollinearity, and no extreme univariate or multivariate outliers.
It is not likely that these assumptions were met, as I know D.C. has an
extreme outlier in tax collections per capita, already violating one of
the many assumptions.

The initial MANOVA results showed that at least one of the response
variables differed significantly between parties. The result was
significant (p &lt; 0.05), so one-way ANOVAS were performed for each
variable. State collections per capita were significant (p &lt; 0.05),
so a pairwise t test was conducted.States with Democratic governors and
Republican governors differ significantly in their tax collections (p
&lt; 0.05). After calculating Bonferroni’s Correction Value, nothing
that was significant is significant anymore (p &gt; 0.0083).

## Randomization

``` r
#Randomization
data2 %>% group_by(Party) %>% summarize(avg=mean(Income)) %>% summarize(mean_diff=diff(avg))
```

    ## # A tibble: 1 x 1
    ##   mean_diff
    ##       <dbl>
    ## 1    -6992.

``` r
rand_dist<-vector()

for(i in 1:5000){
  new <- data.frame(inc = sample(data2$Income), affiliation = data2$Party)
  rand_dist[i] <- mean(new[new$affiliation == "Republican",]$inc) - 
  mean(new[new$affiliation == "Democrat",]$inc)
}

mean(rand_dist > -6991.838)*2
```

    ## [1] 1.9768

``` r
{hist(rand_dist, col="darkblue", border="lightsteelblue4", col.lab="sienna3", main="Income Randomization", col.main="gray40", xlim=c(-10000,10000)); abline(v = -6991.838, col = 'red')}
```

![](Project-2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The null hypothesis is that the mean of median annual household income
does not differ among states with Democratic and Republican
governors.The alternative hypothesis is that the mean of median annual
household income does differ among states with Democratic and Republican
governors. The null distribution appears to be normally distributed.The
mean vocabulary scores don’t differ significantly for states with
Democratic and Republican governors (p &gt; 0.10).

## Regression

``` r
# Mean Centering 
data2$Spending_c <- data2$Spending - mean(data2$Spending, na.rm = TRUE)
data2$Tax_Collection_c <- data2$Tax_Collection - mean(data2$Tax_Collection, na.rm = TRUE)

# Predicting Income from state spending, tax collection, and their interaction
fit <- lm(PercentSnap ~ Spending_c + Tax_Collection_c + Income + Spending_c*Tax_Collection_c, data = data2)

# Show Results
summary(fit) 
```

    ## 
    ## Call:
    ## lm(formula = PercentSnap ~ Spending_c + Tax_Collection_c + Income + 
    ##     Spending_c * Tax_Collection_c, data = data2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.1885 -1.8863 -0.3135  1.8334  5.6628 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  2.203e+01  2.892e+00   7.619 1.43e-09 ***
    ## Spending_c                   8.107e-04  2.556e-04   3.171 0.002765 ** 
    ## Tax_Collection_c            -1.042e-03  6.326e-04  -1.647 0.106735    
    ## Income                      -1.622e-04  4.302e-05  -3.771 0.000481 ***
    ## Spending_c:Tax_Collection_c  2.928e-07  2.245e-07   1.304 0.198923    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.754 on 44 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.4021, Adjusted R-squared:  0.3478 
    ## F-statistic: 7.399 on 4 and 44 DF,  p-value: 0.0001198

``` r
# Plot
data2 %>%
ggplot(aes(Spending_c * Tax_Collection_c, PercentSnap))+
geom_point() + 
geom_smooth(method = lm)+
ggtitle('PercentSnap by centered Spending and Tax Collection Interaction')
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 2 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](Project-2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# Check Assumptions

# Normality (H0: Normal)
shapiro.test(fit$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  fit$residuals
    ## W = 0.98012, p-value = 0.5705

``` r
ggplot() + geom_histogram(aes(fit$residuals), bins=20)
```

![](Project-2_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
# Homoskedasticity (H0: Homoskedasticity) and Linearity
bptest(fit)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  fit
    ## BP = 2.3429, df = 4, p-value = 0.673

``` r
plot(fit, which = 1)
```

![](Project-2_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
# Robust standard errors
coeftest(fit, vcov = vcovHC(fit))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                                Estimate  Std. Error t value  Pr(>|t|)    
    ## (Intercept)                  2.2031e+01  3.3153e+00  6.6453 3.792e-08 ***
    ## Spending_c                   8.1067e-04  2.9574e-04  2.7411  0.008816 ** 
    ## Tax_Collection_c            -1.0416e-03  7.6202e-04 -1.3670  0.178580    
    ## Income                      -1.6222e-04  4.9131e-05 -3.3018  0.001913 ** 
    ## Spending_c:Tax_Collection_c  2.9281e-07  1.8955e-07  1.5448  0.129568    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Bootstrapping SEs
samp_distn <- replicate(5000, {
  boot_dat <- data2[sample(nrow(data2),replace=TRUE),]
  fit1<-lm(PercentSnap~Spending_c*Tax_Collection_c, data=boot_dat)
  coef(fit1)
})
samp_distn %>% t %>% as.data.frame %>% summarize_all(sd)
```

    ##   (Intercept)   Spending_c Tax_Collection_c Spending_c:Tax_Collection_c
    ## 1   0.5203367 0.0003435585     0.0005838878                2.428572e-07

When income, state spending, and state collection are 0, SNAP percentage
is 2.2031e+01. When income increases by one dollar, SNAP percentage
decreases by 1.6222e-04, a result which is insignificant. When state
spending increases by one dollar, SNAP percentage increases by
8.1067e-04, which is significant. When tax collection increases by one
dollar, SNAP percentage decreases by 1.0416e-03, which is insignificant.
When state spending and tax collection increase by one dollar on
average, SNAP percentage increases by 2.928e-07, a result which is
insignificant.

The model predicts of 40% variance in the response according to the
multiple R-squared value. After adjusting for the number of predictors,
the model explains 35% of the variance in the response.

Assumptions: The residuals are normally distributed and homoskedastistic
(SW and BP tests’ have p &gt; 0.05).

The recomputed results with robust standard errors are the same and the
same predictors are significant, but the p-values are higher.

The bootstrapping results show smaller coefficients than the robust and
regular computations.

## Logistic Regression

``` r
# Mutating for binary response
data3 <- data2 %>% mutate(outcome=ifelse(Party == "Republican",1,0)) %>% na.omit()
fit2 <- glm(outcome ~ PercentSnap + Income, data = data3, family = binomial(link = 'logit'))
summary(fit2)
```

    ## 
    ## Call:
    ## glm(formula = outcome ~ PercentSnap + Income, family = binomial(link = "logit"), 
    ##     data = data3)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8685  -0.8930   0.6098   0.9383   1.9022  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)  9.345e+00  3.446e+00   2.712  0.00669 **
    ## PercentSnap -2.358e-01  1.141e-01  -2.066  0.03879 * 
    ## Income      -9.715e-05  3.764e-05  -2.581  0.00985 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 67.417  on 48  degrees of freedom
    ## Residual deviance: 58.825  on 46  degrees of freedom
    ## AIC: 64.825
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
exp(coef(fit2))
```

    ##  (Intercept)  PercentSnap       Income 
    ## 1.144209e+04 7.899567e-01 9.999029e-01

``` r
# Confusion Matrix
pr <- predict(fit2, data = data3, type = 'response')
truth <- data3$Party
table(truth = data3$Party, predicted = (pr > 0.5)) %>% addmargins
```

    ##             predicted
    ## truth        FALSE TRUE Sum
    ##   Democrat      14    8  22
    ##   Republican     6   21  27
    ##   Sum           20   29  49

``` r
# Accuracy (correctly classified cases)
(21 + 8)/49 
```

    ## [1] 0.5918367

``` r
# Sensitivity (True Positive Rate, TPR)
21/27
```

    ## [1] 0.7777778

``` r
# Specificity (True Negative Rate, TNR)
8/22
```

    ## [1] 0.3636364

``` r
# Precision (Positive Predictive Value, PPV)
21/29
```

    ## [1] 0.7241379

``` r
# Density plot
data3$logit <- predict(fit2)
data3 %>% ggplot(aes(logit, fill=Party)) + geom_density(alpha=0.3) + geom_vline(xintercept= 0, lty=2) +  scale_fill_manual(values = c("dodgerblue", "indianred1"))
```

![](Project-2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# ROCplot
library(plotROC)
data4 <- data3 %>% mutate(prob = predict(fit2, type="response"), prediction = ifelse(prob > 0.5, 1, 0))
classify <- data4 %>% transmute(prob, prediction, truth = outcome)

ROCplot <- ggplot(classify) + geom_roc(aes(d = truth, m = prob), n.cuts = 0)
ROCplot
```

![](Project-2_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
# AUC
calc_auc(ROCplot)
```

    ##   PANEL group       AUC
    ## 1     1    -1 0.7474747

A one-unit increase in PercentSnap corresponds to changing the odds by a
factor of e^(-2.358e-01 ), which is significant. A one-unit dollar in
income corresponds to changing the odds by a factor of e^(-9.715e-05),
which is significant. When income and PercentSnap are 0, the odds are
e^(9.345e+00), which is significant.

According to the confusion matrix, the proportion of correctly
classified cases (accuracy) is 0.5918367, meaning that cases are
correctly classified 60% of the time. The sensitivity, the proportion of
true positive, is 0.7777778. The specificity, the proportion of true
negatives, is 0.3636364. The precision, the proportion of true positive
prediction is 0.7241379.

The area under the curve shows the overall measure of model performance.
This value shows that 74.7% of the time, the model correctly predicts
the governor’s political party.
