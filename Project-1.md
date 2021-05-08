Governor’s Party Impact on State Economic Characteristrics
================

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

## Introduction

The title of this project is “Governor’s Party Impact on State Economic
Characteristics.” I wanted to explore the relationship between the
political party of a state’s governor (or in Washington D.C.’s case,
mayor) and state spending, state collection, median annual household
income, and Supplemental Nutrition Assistance Program (SNAP) usage. I
was really curious about how state spending and taxation affected the
welfare of the population and I thought SNAP would be a good indicator
of welfare, with a lower percentage of users being better for the state.
I also know that spending is an issue often debated between political
parties with Republicans favoring less spending than Democrats. I
acquired governor party, income, spending, collection, and SNAP data
from the Kaiser Family Foundation, while population data was taken from
the US Census Bureau. I expect to see that higher state spending will
lead to less SNAP usage, since investment in things like public
education and healthcare can reduce economic stress and I am excited to
learn more about the relationships between these variables.

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
gov <- read_excel('Governor.xlsx')
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
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data <- left_join(gov, left_join(income, left_join(tax, left_join(spend, left_join(snap, pop,  by = 'State'), by = 'State'), by = 'State'), by = 'State'), by = 'State')

#Creating percentage of average monthly SNAP users variable
State <- data$State
Party <- data$`Governer Political Affiliation`
data1 <- suppressWarnings(data %>% na.omit() %>% select(3:7) %>% rename(SNAP = `Average Monthly SNAP Participants`)%>% mutate_if(is.character, as.numeric))
data1$State<-State
data1$Party<-Party
data1 <- data1 %>% relocate(State, Party)
head(data1)
```

    ## # A tibble: 6 x 7
    ##   State Party `Median Annual … `State Collecti… `Per Capita Sta…   SNAP
    ##   <chr> <chr>            <dbl>            <dbl>            <dbl>  <dbl>
    ## 1 Alab… Repu…            51734             2262             5578 7.67e5
    ## 2 Alas… Repu…            75463             2226            14016 9.20e4
    ## 3 Ariz… Repu…            62055             2272             5238 8.46e5
    ## 4 Arka… Repu…            48952             3266             8519 3.72e5
    ## 5 Cali… Demo…            80440             4424             6834 3.95e6
    ## 6 Colo… Demo…            77127             2599             6996 4.50e5
    ## # … with 1 more variable: Population <dbl>

I chose left join because all of the datasets had the ‘State’ variable
in common and I wanted ‘State’ to be the left-most column. No cases were
dropped, as summary statistics and visualizations can be calcualted/made
with na.omit(). The clean, merged dataset has 51 rows and 7 columns.

## Summary Statistics

``` r
#Average median annual household income for the US using summarize()
data1 %>% summarize(mean(`Median Annual Household Income`))
```

    ## # A tibble: 1 x 1
    ##   `mean(\`Median Annual Household Income\`)`
    ##                                        <dbl>
    ## 1                                     65511.

``` r
#Maximum and minimum median annual income grouped by governor's party using group_by(), select() and filter()
data1 %>% rename(Income = `Median Annual Household Income`) %>% select(State, Party, Income) %>% group_by(Party) %>% 
  filter(Income == max(Income)|Income == min(Income))
```

    ## # A tibble: 4 x 3
    ## # Groups:   Party [2]
    ##   State                Party      Income
    ##   <chr>                <chr>       <dbl>
    ## 1 District of Columbia Democrat    92266
    ## 2 Louisiana            Democrat    51073
    ## 3 Maryland             Republican  86738
    ## 4 Mississippi          Republican  45792

``` r
#Standard deviation of median annual household income 
data1 %>% summarize(sd(`Median Annual Household Income`))
```

    ## # A tibble: 1 x 1
    ##   `sd(\`Median Annual Household Income\`)`
    ##                                      <dbl>
    ## 1                                   11171.

``` r
#Average state collection per capita using summarize()
data1 %>% summarize(mean(`State Collections per Capita`))
```

    ## # A tibble: 1 x 1
    ##   `mean(\`State Collections per Capita\`)`
    ##                                      <dbl>
    ## 1                                    3308.

``` r
#Maximum and minimum state collection per capita grouped by governor's party using group_by(), select(), and filter()
data1 %>% select(State, Party, `State Collections per Capita`) %>% group_by(Party) %>% 
  filter(`State Collections per Capita`== max(`State Collections per Capita`)|`State Collections per Capita` == min(`State Collections per Capita`))
```

    ## # A tibble: 4 x 3
    ## # Groups:   Party [2]
    ##   State                Party      `State Collections per Capita`
    ##   <chr>                <chr>                               <dbl>
    ## 1 District of Columbia Democrat                            11331
    ## 2 Louisiana            Democrat                             2437
    ## 3 North Dakota         Republican                           5533
    ## 4 South Carolina       Republican                           2075

``` r
#Variance of average state collection 
data1 %>% summarize(var(`State Collections per Capita`))
```

    ## # A tibble: 1 x 1
    ##   `var(\`State Collections per Capita\`)`
    ##                                     <dbl>
    ## 1                                2202104.

``` r
#Average state spending per capita using summarize()
data1 %>% summarize(mean(`Per Capita State Spending`,na.rm=T))
```

    ## # A tibble: 1 x 1
    ##   `mean(\`Per Capita State Spending\`, na.rm = T)`
    ##                                              <dbl>
    ## 1                                            6838.

``` r
#Maximum and minimum spending per capita grouped by governor's party using group_by(), select(), and filter()
data1 %>% na.omit() %>% select(State, Party, `Per Capita State Spending`) %>% group_by (Party) %>% filter(`Per Capita State Spending` == max(`Per Capita State Spending`)| `Per Capita State Spending` == min(`Per Capita State Spending`)) 
```

    ## # A tibble: 4 x 3
    ## # Groups:   Party [2]
    ##   State    Party      `Per Capita State Spending`
    ##   <chr>    <chr>                            <dbl>
    ## 1 Alaska   Republican                       14016
    ## 2 Delaware Democrat                         11235
    ## 3 Florida  Republican                        3696
    ## 4 Nevada   Democrat                          4711

``` r
#n_distinct of state spending per capita 
data1 %>% summarize(n_distinct(`Per Capita State Spending`))
```

    ## # A tibble: 1 x 1
    ##   `n_distinct(\`Per Capita State Spending\`)`
    ##                                         <int>
    ## 1                                          50

``` r
#Average percentage of monthly SNAP users using summarize() and mutate()
data1 <- data1 %>% mutate(PercentSnap = (SNAP/Population)*100) %>% select(-6, -7)
data1 %>% summarize(mean(PercentSnap, na.rm=T))
```

    ## # A tibble: 1 x 1
    ##   `mean(PercentSnap, na.rm = T)`
    ##                            <dbl>
    ## 1                           12.0

``` r
#Maximum and minimum percentage of monthly SNAP users grouped by governor's party using group_by() and select(), and arrange()
Max <- data1 %>% na.omit() %>% select(State, Party, PercentSnap) %>% arrange(desc(PercentSnap)) %>% slice(1:3) %>% slice(-2)
Min <- data1 %>% na.omit() %>% select(State, Party, PercentSnap) %>% arrange(PercentSnap) %>% slice(1:5) %>% slice(-2,-3,-4)
full_join(Max, Min)
```

    ## Joining, by = c("State", "Party", "PercentSnap")

    ## # A tibble: 4 x 3
    ##   State         Party      PercentSnap
    ##   <chr>         <chr>            <dbl>
    ## 1 New Mexico    Democrat         21.8 
    ## 2 West Virginia Republican       17.9 
    ## 3 Wyoming       Republican        5.07
    ## 4 Kansas        Democrat          7.46

The average median household income for the country is 65,511.31
dollars. The maximum median annual household income in states with
Republican governors comes from Maryland and is 86,738 dollars, while
the minimum value comes from Mississippi and is 45,792 dollars.The
maximum median annual household income in states with Democratic
governors comes from District of Columbia and is 92,266 dollars, while
the minimum value comes from Louisiana and is 51,073 dollars. The
standard deviation is 11171.34 dollars.

The average state collection per capita for the country is 3307.90
dollars. The maximum state collection per capita in states with
Republican governors comes from North Dakota and is 5533 dollars, while
the minimum value comes from South Carolina and is 2075 dollars.The
maximum state collection per capita in states with Democratic governors
comes from D.C. and is 11,331 dollars, while the minimum value comes
from Louisiana and is 2437 dollars. The variance is 2202104
dollars-squared.

The average state spending per capita for the country is 6837.88
dollars. The maximum state spending per capita in states with Republican
governors comes from Alaska and is 14016 dollars, while the minimum
value comes from Florida and is 3696 dollars.The maximum state spending
per capita in states with Democratic governors comes from Delaware and
is 11235 dollars, while the minimum value comes from Nevada and is 4711
dollars. The n\_distinct value is 50.

The average percentage of monthly SNAP users for the country is 12.02%.
The maximum percentage of monthly SNAP users in states with Republican
governors comes from West Virginia and is 17.91%, while the minimum
value comes from Wyoming and is 5.07%. The maximum state spending per
capita in states with Democratic governors comes from New Mexico and is
21.76%, while the minimum value comes from Kansas and is 7.46%.

## Visualizations

``` r
#Correlation Matrix
library(ggplot2)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ tibble  3.0.5     ✓ purrr   0.3.4
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
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
  scale_fill_gradient(low = "plum1", high = "purple4") +
  # Overlay values
  geom_text(aes(label = round(correlation,2)), color = "black", size = 4) +
  # Give title and labels
  labs(title = "Correlation Heat Map for State Characterisitcs", x = "State Characteristics", y = "State Characteristics") 
```

![](Project-1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

This correlation heat map shows the correlations between the numeric
variables in the dataset.

It seems that the percentage of percentage of SNAP users is negatively
correlated with median annual household income (-.42). This makes sense
because in states with higher incomes, fewer people will need to utilize
SNAP.

There is also a weak negative correlation between state collection per
capita and SNAP usage (-0.02), which also makes sense because tax money
collected could be used to fund programs like SNAP.

There is a weakly positive correlation between SNAP usage and state
spending (0.18) and between state spending and median annual household
income (0.25). These make sense because states collect more if there is
more income. Also, if states spend more on programs like SNAP, more
people may be able to use them because of higher income limits.

There is a positive correlation between state collection and median
annual household income (0.56) and between state spending and state
collections (0.50). These make sense as higher household income means
there is more income to collect and because the state will spend what it
collects.

``` r
#Bar Graph
data1 %>% na.omit() %>% group_by(Party) %>% mutate(MeanPercentSnap = mean(PercentSnap)) %>%
  ggplot(aes(Party)) +
  geom_bar(aes(y = `Per Capita State Spending`, fill = MeanPercentSnap), stat="summary") +
  ggtitle('Governor Party Relationship with Spending and Mean SNAP Usage') +
  scale_fill_gradient(low = 'lightpink', high = 'deeppink' )
```

    ## No summary function supplied, defaulting to `mean_se()`

![](Project-1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

From this bar chart, it looks like Democrats spend more per capita and
have a higher percentage of monthly SNAP users on average. It could be
that higher state spending goes towards SNAP benefits for more people
(i.e. a higher income limit) so more people make use of this service.
This visualization shows the variables party, mean SNAP percentage, and
per capita state spending.

``` r
#Scatter Plot
data1 %>%ggplot(aes(x=State, y = `State Collections per Capita`, color = Party)) +
  geom_point(size=3) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  ggtitle('Governor Party Relationship with State Collection Per Capita') +
  scale_color_manual(values = c("dodgerblue", "indianred1"))
```

![](Project-1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

It appears that states with Democratic governors and states with
Republican governors often collect similar amounts, though Republican
states seem to be a bit lower. There is one notable outlier for the
Democrats with Washington D.C.. The variables shown in this graph are
state, party, and state collections per capita.

## PCA

``` r
#Scree plot to see how many principal components to use
data1_PCA<- data1 %>% na.omit() %>% select(3:6) %>% mutate_if(is.numeric, scale) %>% prcomp()

library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
fviz_screeplot(data1_PCA,  barfill = "darkgoldenrod1",
  barcolor = "darkgoldenrod1", linecolor = "springgreen", 
  ncp = 10, addlabels = TRUE)
```

![](Project-1_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
get_eigenvalue(data1_PCA)
```

    ##       eigenvalue variance.percent cumulative.variance.percent
    ## Dim.1  1.9724238        49.310596                    49.31060
    ## Dim.2  1.2802270        32.005674                    81.31627
    ## Dim.3  0.4083114        10.207784                    91.52405
    ## Dim.4  0.3390379         8.475946                   100.00000

Based on the scree plot, I can consider two or three principal
components. Since the first two explain over 80% of the variance, I will
use two principal components.

``` r
#See how PCs correlate with variables
data1_nums <- data1 %>% na.omit() %>% select_if(is.numeric) %>% scale
eigen(cor(data1_nums))
```

    ## eigen() decomposition
    ## $values
    ## [1] 1.9724238 1.2802270 0.4083114 0.3390379
    ## 
    ## $vectors
    ##            [,1]       [,2]       [,3]       [,4]
    ## [1,] -0.5973568  0.2329778  0.6246102  0.4458120
    ## [2,] -0.5924862 -0.2382161 -0.7023159  0.3145881
    ## [3,] -0.3807169 -0.6666726  0.2917585 -0.5705079
    ## [4,]  0.3836401 -0.6667249  0.1774583  0.6138459

It appears that PC1 is negatively correlated with median annual income
and state collections per capita. PC2 is negatively correlated with per
capita state spending and percent SNAP usage.

``` r
#Plot states to see reltionship with PC1 and PC2
data2 <- as.data.frame(data1_PCA$x)
data1_x <- data1 %>% na.omit()
data2$State <- data1_x$State
data2$Party <- data1_x$Party
data2%>% ggplot(aes(x = PC1, y = PC2, color = State, shape = Party )) + geom_point() + 
  #Make sure entire legend is showing
  theme(legend.key=element_blank(), legend.key.size=unit(1,"point"))+
  guides(colour=guide_legend(nrow=20))
```

![](Project-1_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

It appears that while PC1 and PC2 explain most of the variance in the
data, Republican and Democrat states do not follow any particular strong
trend. However, there is a pocket of around 10 Republican states that
are hovering below 0 PC2 values and around -1 PC1 values. This means
high income, high collections, high spending and high SNAP. There is a
Democratic state with negative PC1 and very high PC2. This means low
income, low collections, high spending, and high SNAP.
