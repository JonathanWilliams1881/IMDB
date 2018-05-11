IMDB Exploration
================
Jonathan Williams
April 16, 2018

Introduction
============

IMDB is one of the most popular movie critique websites in the world today. Users often interact with the site seeking information about the quality of movies they want to watch, both old and new! This dataset gives an idea of the type of information that IMDB sources out to the public. First, let's look at some of the data provided and then narrow down the focus to variables specific to our questions of interest.

``` r
IMDB <- read.csv(file = "//Users//macintosh//Downloads//imdb.csv", header = TRUE)
head(IMDB[1:12])
```

    ##                              title                                  url
    ## 1 Der Vagabund und das Kind (1921) http://www.imdb.com/title/tt0012349/
    ## 2                Goldrausch (1925) http://www.imdb.com/title/tt0015864/
    ## 3                Metropolis (1927) http://www.imdb.com/title/tt0017136/
    ## 4               Der General (1926) http://www.imdb.com/title/tt0017925/
    ## 5     Lichter der Großstadt (1931) http://www.imdb.com/title/tt0021749/
    ## 6                         M (1931) http://www.imdb.com/title/tt0022100/
    ##   imdbRating ratingCount duration year nrOfWins nrOfNominations nrOfPhotos
    ## 1        8.4       40550     3240 1921        1               0         19
    ## 2        8.3       45319     5700 1925        2               1         35
    ## 3        8.4       81007     9180 1927        3               4         67
    ## 4        8.3       37521     6420 1926        1               1         53
    ## 5        8.7       70057     5220 1931        2               0         38
    ## 6        8.5       73726     7020 1931        1               0         28
    ##   nrOfNewsArticles nrOfUserReviews nrOfGenre
    ## 1               96              85         3
    ## 2              110             122         3
    ## 3              428             376         2
    ## 4              123             219         3
    ## 5              187             186         3
    ## 6                4             254         3

### Motivation

##### Why Do Directors Make Movies?

Before digging into analysis, I wondered about why movies are made in the first place? What is to gain from making a "Blockbuster" movie? The obvious answer is **MONEY!** The highest grossing films of all time reach into the billions. Generating this kind of income, if everyone could be a succesful director, they would do it! However, we know that only a few have the skill and the industry expertise to design the right film to bring in big bucks. I am interested in using the data available to uncover the mystery behind factors that make for successful movies.

Preliminary Exploratory Analysis
--------------------------------

##### Questions of Interest:

-   What effect does the runtime of a movie have on its IMDB ratings?
-   Is there an ideal runtime for a good movie? Does this depend on the genre/year?
-   How do IMDB Ratings predict Oscar nominations/wins?

``` r
str(IMDB[3:12])
```

    ## 'data.frame':    11072 obs. of  10 variables:
    ##  $ imdbRating      : num  8.4 8.3 8.4 8.3 8.7 8.5 8.3 8.6 8.2 8.4 ...
    ##  $ ratingCount     : int  40550 45319 81007 37521 70057 73726 46503 90847 160414 58169 ...
    ##  $ duration        : int  3240 5700 9180 6420 5220 7020 6300 5220 14280 7740 ...
    ##  $ year            : int  1921 1925 1927 1926 1931 1931 1934 1936 1939 1939 ...
    ##  $ nrOfWins        : int  1 2 3 1 2 1 4 3 10 4 ...
    ##  $ nrOfNominations : int  0 1 4 1 0 0 1 1 6 10 ...
    ##  $ nrOfPhotos      : int  19 35 67 53 38 28 40 44 143 34 ...
    ##  $ nrOfNewsArticles: int  96 110 428 123 187 4 183 27 1263 110 ...
    ##  $ nrOfUserReviews : int  85 122 376 219 186 254 211 180 653 226 ...
    ##  $ nrOfGenre       : int  3 3 2 3 3 3 2 2 3 1 ...

After examining the structure of the dataset, I selected the following variables to help answer the above questions: **imdbRating**, **duration**, **year**, **nrOfNominations**, **nrOfGenre**. I will begin with some descriptive plots and statistics of Rating, Duration, and Years of the movies.

``` r
# Turn duration into numeric
IMDB$duration <- as.numeric(IMDB$duration)
IMDB$year <- as.numeric(IMDB$year)

# Create a variable "time_period" based off of grouped years
IMDB$time_period <- IMDB$year
IMDB$time_period[IMDB$time_period <= 1930] <- "Classic"
IMDB$time_period[IMDB$time_period > 1930 &
                   IMDB$time_period <= 1970] <- "Oldie"
IMDB$time_period[IMDB$time_period > 1970 &
                   IMDB$time_period <= 2000] <- "Current"
IMDB$time_period[IMDB$time_period > 2000 &
                   IMDB$time_period <= 2017] <- "New"
IMDB$time_period <- factor(IMDB$time_period, ordered = TRUE,
                           levels = c("Classic", "Oldie", "Current", "New"))

# Save original graphical parameters
opar <- par(no.readonly = TRUE)

# Multi-frame plots
par(mfrow = c(2,2))
hist(IMDB$duration, xlim = c(3600, 10800),  breaks = 120, col = "lightblue", border = "green",
     xlab = "Movie Duration in Seconds", main = "Distribution of Runtimes") # Slightly right-skewed, approx normal
abline(v = 6114, col = "gray", lty = 2, lwd = 2) 

boxplot(IMDB$duration ~ IMDB$time_period, main = "Movie Runtimes across Time Periods", col = "lightblue",
        pch = 21, xlab = "Time Period", ylab = "Runtime in Seconds") # More variation in runtime for "Current movies"

hist(IMDB$imdbRating, xlim = c(1,10), breaks = 20, col = "lightblue", border = "green", xlab = "IMDB Rating",
     main = "Distribution of IMDB Ratings") # slightly left-skewed, approx normal
abline(v = 6.723, col = "gray", lty = 2, lwd = 2)

boxplot(IMDB$imdbRating ~ IMDB$time_period, main = "IMDB Rating across Time Periods", col = "lightblue",
        pch = 21, xlab = "Time Period", ylab = "IMDB Rating") # More variation in ratings for "Current movies"
```

![unnamed-chunk-3-1](https://user-images.githubusercontent.com/32421511/39900598-df18b380-5490-11e8-9822-e0d1faf65ffa.png)

``` r
# Descriptive Statistics
list("Rating" = summary(IMDB$imdbRating), "Runtime" = summary(IMDB$duration))
```

    ## $Rating
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   1.300   6.200   6.900   6.723   7.500   9.500     251 
    ## 
    ## $Runtime
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##       2    5400    6000    6114    6900   46200     456

##### Preliminary Analysis Recap

From these graphs we can see that for the most part, movie duration(runtime) centers around a median of 6000 seconds, which is slightly more than one and a half hours long. One and a half hours seems to be a standard of sorts because it doesn't change over the years. Another intersting note is that as film directors began to experiment with lengthier films in the "Current" era (1970-2000), the variance in IMDB ratings for that era also increases. This could potentially suggest that movie ratings are affected by the runtime of a movie.

In continuation of the analysis it may be suitable to run a linear regression model over either just the "Current" data or over all of the data with the dependent variable being *rating* and the independent variable being *duration*.

### Application of Linear Regression Methods

I am curious to know more about the relationship between movie duration and imdb rating. The two variables in this experiment (*imdbRating* and *duration*) are both continuous, so it would be appropriate to use an OLS Regression method to observe their relaitonship, with *imdbRating* as the dependent variable and *duration* as the independent variable. Of course, along with any linear regression model must come diagnostics, which I will illustrate graphically in addition to the fitted model.

``` r
library(car)
attach(IMDB)
# Overall Fit
fit <- lm(formula = imdbRating ~ duration)
summary(fit) ## Duration is significant, but it explains a very small amount of the variablility in imdbRating. 
```

    ## 
    ## Call:
    ## lm(formula = imdbRating ~ duration)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4323 -0.5318  0.1672  0.7314  3.3382 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 6.126e+00  3.403e-02  180.04   <2e-16 ***
    ## duration    9.937e-05  5.267e-06   18.87   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.05 on 10461 degrees of freedom
    ##   (609 observations deleted due to missingness)
    ## Multiple R-squared:  0.03291,    Adjusted R-squared:  0.03281 
    ## F-statistic: 355.9 on 1 and 10461 DF,  p-value: < 2.2e-16

``` r
scatter.smooth(duration, imdbRating, lpars = list(lty = 2, col ="red"), main = "IMDB Rating by Duration", ylab = "Rating")
abline(fit$coefficients, col = "blue")
```

![unnamed-chunk-6-1](https://user-images.githubusercontent.com/32421511/39900694-90f0efc8-5491-11e8-8322-f669381d2658.png)

``` r
smoothScatter(duration, imdbRating, main = "IMDB Rating by Duration", ylab = "Rating") # High density visualization
abline(fit$coefficients, col = "blue")
```

![unnamed-chunk-6-2](https://user-images.githubusercontent.com/32421511/39900701-9d4c46e6-5491-11e8-939c-367902fd6e76.png)

``` r
# Checking Model Assumptions 
plot(fit$residuals, main = "Residual Diagnostic Plot", ylab = "Fit Residuals") # Clear violation of Normality/Homoscedasticity assumptions. Megaphone
abline(h = 0, col = "green", lty = 2)
```

![unnamed-chunk-6-3](https://user-images.githubusercontent.com/32421511/39900705-a81d9ce6-5491-11e8-875b-1aa81c1e641c.png)

``` r
qqPlot(fit$residuals) # Clear violation of Normality assumption
```

![unnamed-chunk-6-4](https://user-images.githubusercontent.com/32421511/39900709-b13ba070-5491-11e8-8e3a-d1c607a63dfe.png)

Simple Linear Regression models run based off of the assumptions that the residuals of our model line will be normally distributed around a mean of 0 and also that the observed residual values from the fitted line are independent of changes in values of our predictor variable. In other words, we want to be able to assume that there is no pattern of the distances of the residual values from the fitted line as our predictor variable (duration) varies. Unfortunately, the above graphs are showing that these assumptions are violated as we are observing a classic "megaphone" displacement of the residual values from the fitted line. In many cases, finding an appropriate transformation for the the model can solve this issue of non-normality. Based upon the nature of the curvature in the data as shown in the Q-Q plot, I will try a log transformation to make a new linear model using the predictor of log(durationg) instead of regular duration.

``` r
# Log Transform
attach(IMDB)
```

    ## The following objects are masked from IMDB (pos = 3):
    ## 
    ##     Action, Adult, Adventure, Animation, Biography, Comedy, Crime,
    ##     Documentary, Drama, duration, Family, Fantasy, FilmNoir,
    ##     GameShow, History, Horror, imdbRating, Music, Musical,
    ##     Mystery, News, nrOfGenre, nrOfNewsArticles, nrOfNominations,
    ##     nrOfPhotos, nrOfUserReviews, nrOfWins, ratingCount, RealityTV,
    ##     Romance, SciFi, Short, Sport, TalkShow, Thriller, time_period,
    ##     title, url, War, Western, year

``` r
logfit <- lm(formula = imdbRating ~ log(duration))
summary(logfit)
```

    ## 
    ## Call:
    ## lm(formula = imdbRating ~ log(duration))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4398 -0.5376  0.1671  0.7428  3.3634 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    4.85602    0.18480   26.28   <2e-16 ***
    ## log(duration)  0.21756    0.02133   10.20   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.063 on 10461 degrees of freedom
    ##   (609 observations deleted due to missingness)
    ## Multiple R-squared:  0.009848,   Adjusted R-squared:  0.009753 
    ## F-statistic:   104 on 1 and 10461 DF,  p-value: < 2.2e-16

``` r
scatter.smooth(log(duration), imdbRating, lpars = list(col = "red", lty = 2), main = "IMDB Rating by log(Duration)", ylab = "Rating")
abline(logfit$coefficients, col = "blue")
```

![unnamed-chunk-7-1](https://user-images.githubusercontent.com/32421511/39900715-b94327b6-5491-11e8-9210-c484db9cf27b.png)

``` r
smoothScatter(log(duration), imdbRating, main = "IMDB Rating by log(Duration)", ylab = "Rating") # High density visualization
abline(logfit$coefficients, col = "blue")
```

![unnamed-chunk-7-2](https://user-images.githubusercontent.com/32421511/39900719-c0b467f8-5491-11e8-92bc-f3d4b3641a0b.png)

``` r
# Checking Model Assumptions
plot(logfit$residuals, main = "Residual Diagnostic Plot", ylab = "Fit Residuals")
abline(h = 0, col = "green", lty = 2)
```

![unnamed-chunk-7-3](https://user-images.githubusercontent.com/32421511/39900723-c87452f0-5491-11e8-9e72-6dea20d6d3e9.png)

``` r
qqPlot(logfit$residuals)
```

![unnamed-chunk-7-4](https://user-images.githubusercontent.com/32421511/39900727-d536e96c-5491-11e8-9ca9-26dde376e87c.png)

``` r
detach(IMDB)
```

#### Linear Regression Assumptions Violated

Still, it appears that the data has a strong departure from normality (megaphone phenomenon), which would prove detrimental for inferences obtained from nearly any linear regression or ANOVA model we could form. If the assumptions had not been violated, I may have continued by using an ANOVA model and prehaps ANCOVA to increase the R-squared values and explain more of the variation in the data.

For example: What is the relationship between movie duration and imdb rating?

One-Way ANOVA (Unbalanced): *R**a**t**i**n**g* = *B*<sub>0</sub> + *B*<sub>1</sub>(*T**i**m**e*.*P**e**r**i**o**d*)

Do the mean ratings differ for different time periods when controlling for movie duration?

One-Way ANCOVA Full Model: *R**a**t**i**n**g* = *B*<sub>0</sub> + *B*<sub>1</sub>(*D**u**r**a**t**i**o**n*)+*B*<sub>2</sub>(*T**i**m**e*.*P**e**r**i**o**d*)+*B*<sub>3</sub>(*D**u**r**a**t**i**o**n* \* *T**i**m**e*.*P**e**r**i**o**d*)

------------------------------------------------------------------------

I am sure that somewhere, there is an appropriate transformation for our data that will correct for the violated assumptions of normality and homoscedasticity. However, given time constraints and the labor neccesary towards finding an appropriate model, I decided that it would be more efficient to perform a different statistical test and perhaps explore other variables as well.

My original goal was to examine the relationship between the duration times of movies and their imdbRating. I had to keep an open mind that perhaps imdbRating would make for a more useful predictor variable rather than a response variable. Almost immediately I turned my focus towards my second question: **"How do IMDB ratings predict Oscar nominations"?**

I feel this is an important question to address because, as mentioned earlier, the pinnacle of success for any released movie is to make a lot of money, and there is no question that Oscar nominations generate a lot of revenue. **Films nominated for an Oscar enjoy a box office boost, as more fans head to the big screen and the films expand to more theaters. This can be worth tens of millions of dollars!** So the question at hand then becomes: *What factors are good predictors of at least one Oscar nomination for movies?*

------------------------------------------------------------------------

Logistic Regression Motivations
-------------------------------

The major point of my inquiry is whether or not a movie is nominated for an Oscar, and why? Since you can either win or not win an Oscar we see that the outcome is a binary response. This motivated me to pursue a Logistic Regression Model based off of multiple predictor variables that would determine, with some level of accuracy, the likelihood of a certain movie being nominated for an Oscar.

### Application of Logistic Regression Methods

The formula for our model is...

Binary Logistic Regression Model: $log(\\frac{P(Nominated)}{1 - P(Nominated)}) = B\_0 + B\_iX\_{ji}$

where: j = \# of the predictor from j to m, and i = ith unit change in the predictor

This model can be instrumental in answering a question mentioned before in our Preliminary Exploratory Analysis: "How do IMDB Ratings predict Oscar nominations/wins?". In this case, we are using nominations as our response variable under the reasoning that a movie cannot win an Oscar until it is first nominated. So we want to find a general linearized model that can help us predict Oscar nominations.

The great thing about Logistic Regression and why it is applicable in this situation is that it does not depend on the normality assumption. This is crucial for the data that we are working with, because, as our attempt on a linear regression model showed, the data shows many departures from normality. Logistic Regression accounts for this issue and is still a very powerful statistical classification tool that will prove useful to us in this project.

Lets have another look at our original data:

``` r
head(IMDB[3:12], 12)
```

    ##    imdbRating ratingCount duration year nrOfWins nrOfNominations
    ## 1         8.4       40550     3240 1921        1               0
    ## 2         8.3       45319     5700 1925        2               1
    ## 3         8.4       81007     9180 1927        3               4
    ## 4         8.3       37521     6420 1926        1               1
    ## 5         8.7       70057     5220 1931        2               0
    ## 6         8.5       73726     7020 1931        1               0
    ## 7         8.3       46503     6300 1934        4               1
    ## 8         8.6       90847     5220 1936        3               1
    ## 9         8.2      160414    14280 1939       10               6
    ## 10        8.4       58169     7740 1939        4              10
    ## 11        8.1      209506     6120 1939        6              12
    ## 12        8.2       45737     7740 1940        6               5
    ##    nrOfPhotos nrOfNewsArticles nrOfUserReviews nrOfGenre
    ## 1          19               96              85         3
    ## 2          35              110             122         3
    ## 3          67              428             376         2
    ## 4          53              123             219         3
    ## 5          38              187             186         3
    ## 6          28                4             254         3
    ## 7          40              183             211         2
    ## 8          44               27             180         2
    ## 9         143             1263             653         3
    ## 10         34              110             226         1
    ## 11        126             2363             477         3
    ## 12         20              135             257         1

``` r
summary(IMDB$nrOfNominations)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   0.000   3.877   4.000 137.000

In order to run Logistic Regression we need a categorical variable. In particular, it needs to be a binary variable taking one of two possible values. We can clearly see from the summary statistics and a glance at the data that the **nrOfNominations** column is continuous and varies much differently than from 0 to 1. This problem can be fixed by factoring the **nrOfNominations** variable. Within the context of our problem, we are not interested in how many nominations a film receives, but rather whether it receives a nomination or not. So, to prep the data I created a new variable, **ynNominations**, based off of **nrOfNominations**, and set its values to 0 and 1 depending on whether a nomination was received or not.

``` r
# Data prep for Logistic Regression
table(IMDB$nrOfNominations)
```

    ## 
    ##    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14 
    ## 6458  748  514  444  376  321  269  238  206  136  132  116  108   87   85 
    ##   15   16   17   18   19   20   21   22   23   24   25   26   27   28   29 
    ##   74   60   46   43   63   50   37   41   28   28   21   20   17   20   17 
    ##   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44 
    ##   18   18   11   12   15   15   11   11    4   12    6    4    2    7    8 
    ##   45   46   47   48   49   50   51   52   53   54   55   56   57   58   59 
    ##    4    4    3    3    5    4    8    3    4    1    5    3    5    3    1 
    ##   60   61   62   63   64   65   66   68   69   70   71   72   74   75   77 
    ##    1    2    4    5    1    1    3    3    2    1    3    1    5    1    1 
    ##   78   79   80   81   82   83   85   86   88   90   91  104  110  111  115 
    ##    1    2    3    1    1    2    2    2    1    1    1    2    1    1    1 
    ##  121  125  137 
    ##    1    1    1

``` r
IMDB$ynNominations[IMDB$nrOfNominations >= 1] <- 1
IMDB$ynNominations[IMDB$nrOfNominations == 0] <- 0
IMDB$ynNominations <- factor(IMDB$ynNominations,
                             levels = c(0,1), labels = c("No", "Yes"))
table(IMDB$ynNominations)
```

    ## 
    ##   No  Yes 
    ## 6458 4614

As you can see, the **nrOfNominations** variable has been recoded to a binary response variable called **ynNominations**.

##### Which variables are significant in predicting Oscar nominations?

``` r
## Logistic Regression
attach(IMDB)
```

    ## The following objects are masked from IMDB (pos = 3):
    ## 
    ##     Action, Adult, Adventure, Animation, Biography, Comedy, Crime,
    ##     Documentary, Drama, duration, Family, Fantasy, FilmNoir,
    ##     GameShow, History, Horror, imdbRating, Music, Musical,
    ##     Mystery, News, nrOfGenre, nrOfNewsArticles, nrOfNominations,
    ##     nrOfPhotos, nrOfUserReviews, nrOfWins, ratingCount, RealityTV,
    ##     Romance, SciFi, Short, Sport, TalkShow, Thriller, time_period,
    ##     title, url, War, Western, year

``` r
oscar_nomination <- glm(formula = ynNominations ~ imdbRating + ratingCount + duration + year + nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + nrOfGenre, family = binomial(), data = IMDB)
summary(oscar_nomination) # ratingCount variable not significant
```

    ## 
    ## Call:
    ## glm(formula = ynNominations ~ imdbRating + ratingCount + duration + 
    ##     year + nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + 
    ##     nrOfGenre, family = binomial(), data = IMDB)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.4258  -0.8106  -0.3989   0.9059   3.0652  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -4.636e+01  2.389e+00 -19.408  < 2e-16 ***
    ## imdbRating        7.676e-01  2.999e-02  25.593  < 2e-16 ***
    ## ratingCount      -1.450e-07  1.308e-06  -0.111 0.911721    
    ## duration          2.459e-04  1.489e-05  16.511  < 2e-16 ***
    ## year              1.928e-02  1.161e-03  16.609  < 2e-16 ***
    ## nrOfPhotos        1.800e-02  1.475e-03  12.207  < 2e-16 ***
    ## nrOfNewsArticles  2.845e-04  9.194e-05   3.094 0.001974 ** 
    ## nrOfUserReviews   2.904e-03  3.287e-04   8.836  < 2e-16 ***
    ## nrOfGenre         1.209e-01  3.223e-02   3.752 0.000176 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 14345  on 10462  degrees of freedom
    ## Residual deviance: 10660  on 10454  degrees of freedom
    ##   (609 observations deleted due to missingness)
    ## AIC: 10678
    ## 
    ## Number of Fisher Scoring iterations: 6

##### Our output for the full Logistic Regression model suggests that all variables are significant at the 99% significance level except for **ratingCount**. As a result we choose to fit a reduced model dropping insignificant variables...

``` r
oscar_nomination_reduced <- glm(formula = ynNominations ~ imdbRating + duration + year + nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + nrOfGenre, family = binomial(), data = IMDB)
summary(oscar_nomination_reduced) ## The reduced model shows significance in every variable.
```

    ## 
    ## Call:
    ## glm(formula = ynNominations ~ imdbRating + duration + year + 
    ##     nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + nrOfGenre, 
    ##     family = binomial(), data = IMDB)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.4261  -0.8108  -0.3991   0.9061   3.0642  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -4.632e+01  2.361e+00 -19.617  < 2e-16 ***
    ## imdbRating        7.670e-01  2.948e-02  26.015  < 2e-16 ***
    ## duration          2.459e-04  1.489e-05  16.516  < 2e-16 ***
    ## year              1.926e-02  1.150e-03  16.755  < 2e-16 ***
    ## nrOfPhotos        1.796e-02  1.423e-03  12.621  < 2e-16 ***
    ## nrOfNewsArticles  2.811e-04  8.660e-05   3.246 0.001172 ** 
    ## nrOfUserReviews   2.883e-03  2.661e-04  10.832  < 2e-16 ***
    ## nrOfGenre         1.208e-01  3.222e-02   3.750 0.000177 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 14345  on 10462  degrees of freedom
    ## Residual deviance: 10660  on 10455  degrees of freedom
    ##   (609 observations deleted due to missingness)
    ## AIC: 10676
    ## 
    ## Number of Fisher Scoring iterations: 6

The reduced model shows significance in every variable which indicates that each of the variables have an affect on the outcome of nominations. We also note that there is no drop in the null deviance between the full and reduced models. Let's compare them with an ANOVA test based upon a Chi-squared distribution.

``` r
# Comparison of full to reduced model
anova(oscar_nomination_reduced, oscar_nomination, test = "Chisq") # Insignificant change for the reduced model. Reduced fits just as well, keep reduced
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: ynNominations ~ imdbRating + duration + year + nrOfPhotos + nrOfNewsArticles + 
    ##     nrOfUserReviews + nrOfGenre
    ## Model 2: ynNominations ~ imdbRating + ratingCount + duration + year + 
    ##     nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + nrOfGenre
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1     10455      10660                     
    ## 2     10454      10660  1 0.012266   0.9118

We observe an insignificant change for the reduced model so we can assume that it fits the data just as well as the full model. With that being said, it makes more sense to use the reduced model with less explanatory variables because simpler is better.

Now that we have a model we want to examine the determined coefficients and manipulate the model with some algebra to make it easier to interpret in layman's terms. As it is, our model looks like this...

#### Log Odds Equation

$log(\\frac{P(Nomination)}{1 - P(Nomination)}) = -4.632 + 7.670^{-1}(imdbRating) + 1.476^{-2}(duration.min) + 1.926^{-2}(year) + 1.796^{-2}(nrOfPhotos) + 2.811^{-4}(nrOfNewsArticles) + 2.883^{-3}(nrOfUserReviews) + 1.208^{-1}(nrOfGenre)$

We read this equation as the log of the odds of Oscar nomination based on the effect of the various predictor variables. But this is difficult to interpret. We would much rather our model be interpreted in terms of the probability of Oscar nomination. Thus, we first exponentiate our model and then apply algebra to arrive at a model of the probability of Oscar nomination. I have provided the corresponding equations and coefficient terms below.

``` r
coef(oscar_nomination_reduced) # Log Odds Equation
```

    ##      (Intercept)       imdbRating         duration             year 
    ##    -4.631861e+01     7.670059e-01     2.459369e-04     1.926020e-02 
    ##       nrOfPhotos nrOfNewsArticles  nrOfUserReviews        nrOfGenre 
    ##     1.795746e-02     2.810715e-04     2.882823e-03     1.208161e-01

``` r
exp(coef(oscar_nomination_reduced)) # Odds Equation
```

    ##      (Intercept)       imdbRating         duration             year 
    ##     7.657440e-21     2.153309e+00     1.000246e+00     1.019447e+00 
    ##       nrOfPhotos nrOfNewsArticles  nrOfUserReviews        nrOfGenre 
    ##     1.018120e+00     1.000281e+00     1.002887e+00     1.128417e+00

#### Odds Equation

$\\frac{P(Nomination)}{1 - P(Nomination)} = {7.311}^{-23} \* {2.249}^{imdbRating} \* {1.000}^{duration.min} \* {1.024}^{year} \* {1.019}^{nrOfPhotos} \* {1.000}^{nrOfNewsArticles} \* {1.003}^{nrOfUserReviews} \* {1.160}^{nrOfGenre}$

#### Probability Equation

$P(Nomination) = \\frac{{7.311}^{-23} \* {2.249}^{imdbRating} \* {1.000}^{duration.min} \* {1.024}^{year} \* {1.019}^{nrOfPhotos} \* {1.000}^{nrOfNewsArticles} \* {1.003}^{nrOfUserReviews} \* {1.160}^{nrOfGenre}}{1 + {7.311}^{-23} \* {2.249}^{imdbRating} \* {1.000}^{duration.min} \* {1.024}^{year} \* {1.019}^{nrOfPhotos} \* {1.000}^{nrOfNewsArticles} \* {1.003}^{nrOfUserReviews} \* {1.160}^{nrOfGenre}}$

#### 95% Confidence Intervals and Diagnostic Plot

``` r
exp(confint(oscar_nomination_reduced)) # Probability Equation
```

    ## Waiting for profiling to be done...

    ##                         2.5 %       97.5 %
    ## (Intercept)      7.311175e-23 7.654739e-19
    ## imdbRating       2.033312e+00 2.282444e+00
    ## duration         1.000217e+00 1.000275e+00
    ## year             1.017163e+00 1.021757e+00
    ## nrOfPhotos       1.015303e+00 1.020982e+00
    ## nrOfNewsArticles 1.000117e+00 1.000456e+00
    ## nrOfUserReviews  1.002371e+00 1.003417e+00
    ## nrOfGenre        1.059444e+00 1.202072e+00

``` r
## Diagnostic Plot

plot(predict(oscar_nomination_reduced, type = "response"),
     residuals(oscar_nomination_reduced, type = "deviance"), main = "Logistic Diagnostic Plot", ylab = "Residuals", xlab = "Prediction")
```

![unnamed-chunk-14-1](https://user-images.githubusercontent.com/32421511/39900730-dc9d0b50-5491-11e8-8d81-5684e9314455.png)

As for the **duration** variable, I figured it may also be more understandable to modify duration time from seconds to minutes, a more common standard for keeping time. So I created another variable **duration.min** which is just the duration time divided by 60 in order to convert every value from seconds to minutes.

##### Create a minutes variable.

``` r
## Modify Regression for duration.min
IMDB$duration.min <- IMDB$duration/60
IMDB$duration.min <- as.numeric(IMDB$duration.min)
summary(IMDB$duration.min)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
    ##   0.0333  90.0000 100.0000 101.9000 115.0000 770.0000      456

Now I repeat the procedures from before on the new data with **duration.min** instead of **duration**.

``` r
oscar_nomination2 <- glm(formula = ynNominations ~ imdbRating + ratingCount + duration.min + year + nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + nrOfGenre, family = binomial(), data = IMDB)
summary(oscar_nomination2) # Full Model
```

    ## 
    ## Call:
    ## glm(formula = ynNominations ~ imdbRating + ratingCount + duration.min + 
    ##     year + nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + 
    ##     nrOfGenre, family = binomial(), data = IMDB)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.4258  -0.8106  -0.3989   0.9059   3.0652  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -4.636e+01  2.389e+00 -19.408  < 2e-16 ***
    ## imdbRating        7.676e-01  2.999e-02  25.593  < 2e-16 ***
    ## ratingCount      -1.450e-07  1.308e-06  -0.111 0.911721    
    ## duration.min      1.475e-02  8.936e-04  16.511  < 2e-16 ***
    ## year              1.928e-02  1.161e-03  16.609  < 2e-16 ***
    ## nrOfPhotos        1.800e-02  1.475e-03  12.207  < 2e-16 ***
    ## nrOfNewsArticles  2.845e-04  9.194e-05   3.094 0.001974 ** 
    ## nrOfUserReviews   2.904e-03  3.287e-04   8.836  < 2e-16 ***
    ## nrOfGenre         1.209e-01  3.223e-02   3.752 0.000176 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 14345  on 10462  degrees of freedom
    ## Residual deviance: 10660  on 10454  degrees of freedom
    ##   (609 observations deleted due to missingness)
    ## AIC: 10678
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
oscar_nomination2_reduced <- glm(formula = ynNominations ~ imdbRating + duration.min + year + nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + nrOfGenre, family = binomial(), data = IMDB)
summary(oscar_nomination_reduced) # Reduced Model
```

    ## 
    ## Call:
    ## glm(formula = ynNominations ~ imdbRating + duration + year + 
    ##     nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + nrOfGenre, 
    ##     family = binomial(), data = IMDB)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.4261  -0.8108  -0.3991   0.9061   3.0642  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -4.632e+01  2.361e+00 -19.617  < 2e-16 ***
    ## imdbRating        7.670e-01  2.948e-02  26.015  < 2e-16 ***
    ## duration          2.459e-04  1.489e-05  16.516  < 2e-16 ***
    ## year              1.926e-02  1.150e-03  16.755  < 2e-16 ***
    ## nrOfPhotos        1.796e-02  1.423e-03  12.621  < 2e-16 ***
    ## nrOfNewsArticles  2.811e-04  8.660e-05   3.246 0.001172 ** 
    ## nrOfUserReviews   2.883e-03  2.661e-04  10.832  < 2e-16 ***
    ## nrOfGenre         1.208e-01  3.222e-02   3.750 0.000177 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 14345  on 10462  degrees of freedom
    ## Residual deviance: 10660  on 10455  degrees of freedom
    ##   (609 observations deleted due to missingness)
    ## AIC: 10676
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
exp(coef(oscar_nomination2_reduced)) # Coefficients for the odds of winning an Oscar
```

    ##      (Intercept)       imdbRating     duration.min             year 
    ##     7.657440e-21     2.153309e+00     1.014866e+00     1.019447e+00 
    ##       nrOfPhotos nrOfNewsArticles  nrOfUserReviews        nrOfGenre 
    ##     1.018120e+00     1.000281e+00     1.002887e+00     1.128417e+00

``` r
exp(confint(oscar_nomination2_reduced)) # CIs for the coeffiicents
```

    ## Waiting for profiling to be done...

    ##                         2.5 %       97.5 %
    ## (Intercept)      7.311175e-23 7.654739e-19
    ## imdbRating       2.033312e+00 2.282444e+00
    ## duration.min     1.013105e+00 1.016660e+00
    ## year             1.017163e+00 1.021757e+00
    ## nrOfPhotos       1.015303e+00 1.020982e+00
    ## nrOfNewsArticles 1.000117e+00 1.000456e+00
    ## nrOfUserReviews  1.002371e+00 1.003417e+00
    ## nrOfGenre        1.059444e+00 1.202072e+00

``` r
detach(IMDB)
```

Logistic Regression Conclusions
-------------------------------

From our logistic regression model we conclude that there are seven significant predictor variables for Oscar nominations. They are shown in the output above. One variable that stood out a lot was the **imdbRating** itself! *Unit increases in the rating variable causes the probability of an Oscar nomination to double, holding all other factors constant at their means.* Most of the other factors have a much smaller multiplicative effect than **imdbRating**.

A Closer Look: imdbRating
-------------------------

Holding all other variables constant at their means, I would like to take a closer look at the imdbRating variable. Lets examine the effect of imdbRating on Oscar nominations working by itself.

``` r
# Predictive Models of probability
attach(IMDB)
```

    ## The following objects are masked from IMDB (pos = 3):
    ## 
    ##     Action, Adult, Adventure, Animation, Biography, Comedy, Crime,
    ##     Documentary, Drama, duration, Family, Fantasy, FilmNoir,
    ##     GameShow, History, Horror, imdbRating, Music, Musical,
    ##     Mystery, News, nrOfGenre, nrOfNewsArticles, nrOfNominations,
    ##     nrOfPhotos, nrOfUserReviews, nrOfWins, ratingCount, RealityTV,
    ##     Romance, SciFi, Short, Sport, TalkShow, Thriller, time_period,
    ##     title, url, War, Western, year

``` r
test_imdbRating <- data.frame(imdbRating = c(0,1,2,3,4,5,6,7,8,9,10),
                              duration.min = mean(duration.min, na.rm = TRUE),
                              year = mean(year, na.rm = TRUE),
                              nrOfPhotos = mean(nrOfPhotos),
                              nrOfNewsArticles = mean(nrOfNewsArticles),
                              nrOfUserReviews = mean(nrOfUserReviews),
                              nrOfGenre = mean(nrOfGenre))

test_imdbRating$prob <- predict(oscar_nomination2_reduced, newdata = test_imdbRating, type = "response")
test_imdbRating
```

    ##    imdbRating duration.min     year nrOfPhotos nrOfNewsArticles
    ## 1           0      101.892 1987.422   22.11678          245.768
    ## 2           1      101.892 1987.422   22.11678          245.768
    ## 3           2      101.892 1987.422   22.11678          245.768
    ## 4           3      101.892 1987.422   22.11678          245.768
    ## 5           4      101.892 1987.422   22.11678          245.768
    ## 6           5      101.892 1987.422   22.11678          245.768
    ## 7           6      101.892 1987.422   22.11678          245.768
    ## 8           7      101.892 1987.422   22.11678          245.768
    ## 9           8      101.892 1987.422   22.11678          245.768
    ## 10          9      101.892 1987.422   22.11678          245.768
    ## 11         10      101.892 1987.422   22.11678          245.768
    ##    nrOfUserReviews nrOfGenre        prob
    ## 1         130.8161  2.322525 0.004438609
    ## 2         130.8161  2.322525 0.009509021
    ## 3         130.8161  2.322525 0.020253744
    ## 4         130.8161  2.322525 0.042617091
    ## 5         130.8161  2.322525 0.087468636
    ## 6         130.8161  2.322525 0.171087952
    ## 7         130.8161  2.322525 0.307692266
    ## 8         130.8161  2.322525 0.489020632
    ## 9         130.8161  2.322525 0.673285189
    ## 10        130.8161  2.322525 0.816091364
    ## 11        130.8161  2.322525 0.905260618

``` r
detach(IMDB)
```

This data shows us how the probability doubles for each unit increase in **imdbRating**. This could suggest that imdbRating is not only significant, but a reliable predictor of Oscar nominations for movies. Analysts may be encouraged by this model to look to IMDB ratings as a reliable source to examine the success of their films.

Testing Our Model
-----------------

The previous models we ran were over the entire dataset. Of course, no model can really be determined as accurate until it can predict over random samples of the data provided and still capture many of the anticipated values. I now repeat procedures after splitting the data into test and training data.

``` r
# Making a 60% training and 40% validation dataset
set.seed(1234)
train <- sample(nrow(IMDB), .6*nrow(IMDB))
IMDB.train <- IMDB[train, ]
IMDB.validate <- IMDB[-train, ]

# Check tabular values of nominations for each dataset
table(IMDB.train$ynNominations)
```

    ## 
    ##   No  Yes 
    ## 3880 2763

``` r
table(IMDB.validate$ynNominations)
```

    ## 
    ##   No  Yes 
    ## 2578 1851

``` r
# Use Logistic Regression model on the training data
oscar_nomination2.train <- glm(formula = ynNominations ~ imdbRating + ratingCount + duration.min + year + nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + nrOfGenre, family = binomial(), data = IMDB.train)
summary(oscar_nomination2.train) # ratingCount insignificant, drop from model
```

    ## 
    ## Call:
    ## glm(formula = ynNominations ~ imdbRating + ratingCount + duration.min + 
    ##     year + nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + 
    ##     nrOfGenre, family = binomial(), data = IMDB.train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.2010  -0.8062  -0.4219   0.9141   3.0078  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -4.650e+01  3.077e+00 -15.111  < 2e-16 ***
    ## imdbRating        7.318e-01  3.812e-02  19.198  < 2e-16 ***
    ## ratingCount       8.053e-07  1.749e-06   0.461   0.6451    
    ## duration.min      1.340e-02  1.137e-03  11.791  < 2e-16 ***
    ## year              1.957e-02  1.498e-03  13.066  < 2e-16 ***
    ## nrOfPhotos        1.846e-02  1.908e-03   9.675  < 2e-16 ***
    ## nrOfNewsArticles  3.131e-04  1.229e-04   2.547   0.0109 *  
    ## nrOfUserReviews   2.771e-03  4.377e-04   6.331 2.43e-10 ***
    ## nrOfGenre         8.673e-02  4.131e-02   2.100   0.0358 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 8621.0  on 6290  degrees of freedom
    ## Residual deviance: 6418.3  on 6282  degrees of freedom
    ##   (352 observations deleted due to missingness)
    ## AIC: 6436.3
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
oscar_nomination2_reduced.train <- glm(formula = ynNominations ~ imdbRating + duration.min + year + nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + nrOfGenre, family = binomial(), data = IMDB.train)
summary(oscar_nomination2_reduced.train)
```

    ## 
    ## Call:
    ## glm(formula = ynNominations ~ imdbRating + duration.min + year + 
    ##     nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + nrOfGenre, 
    ##     family = binomial(), data = IMDB.train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.2000  -0.8071  -0.4217   0.9117   3.0132  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -4.672e+01  3.043e+00 -15.354  < 2e-16 ***
    ## imdbRating        7.349e-01  3.754e-02  19.578  < 2e-16 ***
    ## duration.min      1.339e-02  1.137e-03  11.783  < 2e-16 ***
    ## year              1.967e-02  1.484e-03  13.255  < 2e-16 ***
    ## nrOfPhotos        1.869e-02  1.845e-03  10.131  < 2e-16 ***
    ## nrOfNewsArticles  3.326e-04  1.158e-04   2.872  0.00407 ** 
    ## nrOfUserReviews   2.889e-03  3.566e-04   8.102 5.42e-16 ***
    ## nrOfGenre         8.731e-02  4.129e-02   2.114  0.03448 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 8621.0  on 6290  degrees of freedom
    ## Residual deviance: 6418.5  on 6283  degrees of freedom
    ##   (352 observations deleted due to missingness)
    ## AIC: 6434.5
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# Reduced and Full model comparison
anova(oscar_nomination2_reduced.train, oscar_nomination2.train, test = "Chisq") # Reduced is just as good as the full model, use reduced.
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: ynNominations ~ imdbRating + duration.min + year + nrOfPhotos + 
    ##     nrOfNewsArticles + nrOfUserReviews + nrOfGenre
    ## Model 2: ynNominations ~ imdbRating + ratingCount + duration.min + year + 
    ##     nrOfPhotos + nrOfNewsArticles + nrOfUserReviews + nrOfGenre
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      6283     6418.5                     
    ## 2      6282     6418.3  1  0.21447   0.6433

``` r
# Obtain model coefficients
coef(oscar_nomination2_reduced.train) # Log Odds
```

    ##      (Intercept)       imdbRating     duration.min             year 
    ##    -4.671561e+01     7.349153e-01     1.339475e-02     1.966843e-02 
    ##       nrOfPhotos nrOfNewsArticles  nrOfUserReviews        nrOfGenre 
    ##     1.869191e-02     3.326136e-04     2.889092e-03     8.730993e-02

``` r
exp(coef(oscar_nomination2_reduced.train)) # Odds
```

    ##      (Intercept)       imdbRating     duration.min             year 
    ##     5.148364e-21     2.085305e+00     1.013485e+00     1.019863e+00 
    ##       nrOfPhotos nrOfNewsArticles  nrOfUserReviews        nrOfGenre 
    ##     1.018868e+00     1.000333e+00     1.002893e+00     1.091235e+00

``` r
# Obtain CIs for coefficients
exp(confint(oscar_nomination2_reduced.train))
```

    ## Waiting for profiling to be done...

    ##                         2.5 %       97.5 %
    ## (Intercept)      1.271595e-23 1.927360e-18
    ## imdbRating       1.938788e+00 2.246164e+00
    ## duration.min     1.011254e+00 1.015771e+00
    ## year             1.016919e+00 1.022852e+00
    ## nrOfPhotos       1.015223e+00 1.022594e+00
    ## nrOfNewsArticles 1.000115e+00 1.000568e+00
    ## nrOfUserReviews  1.002205e+00 1.003606e+00
    ## nrOfGenre        1.006503e+00 1.183371e+00

Equations from the model:

#### Log Odds Equation

$log(\\frac{P(Nomination)}{1 - P(Nomination)}) = -4.672 + 7.349^{-1}(imdbRating) + 1.339^{-2}(duration.min) + 1.967^{-2}(year) + 1.869^{-2}(nrOfPhotos) + 3.326^{-4}(nrOfNewsArticles) + 2.889^{-3}(nrOfUserReviews) + 8.731^{-2}(nrOfGenre)$

#### Odds Equation (exponentiated values)

$\\frac{P(Nomination)}{1 - P(Nomination)} = {5.148}^{-21} \* {2.085}^{imdbRating} \* {1.013}^{duration.min} \* {1.020}^{year} \* {1.019}^{nrOfPhotos} \* {1.000}^{nrOfNewsArticles} \* {1.003}^{nrOfUserReviews} \* {1.091}^{nrOfGenre}$

#### Probability Equation

$P(Nomination) = \\frac{{5.148}^{-21} \* {2.085}^{imdbRating} \* {1.013}^{duration.min} \* {1.020}^{year} \* {1.019}^{nrOfPhotos} \* {1.000}^{nrOfNewsArticles} \* {1.003}^{nrOfUserReviews} \* {1.091}^{nrOfGenre}}{1 + {5.148}^{-21} \* {2.085}^{imdbRating} \* {1.013}^{duration.min} \* {1.020}^{year} \* {1.019}^{nrOfPhotos} \* {1.000}^{nrOfNewsArticles} \* {1.003}^{nrOfUserReviews} \* {1.091}^{nrOfGenre}}$

Logistic Regression Conclusions (Training)
------------------------------------------

We have now obtained a model based upon a training dataset. Let's examine it's performance on our test data.

``` r
# Predictive Models of probability
attach(IMDB.train)
```

    ## The following objects are masked from IMDB:
    ## 
    ##     Action, Adult, Adventure, Animation, Biography, Comedy, Crime,
    ##     Documentary, Drama, duration, Family, Fantasy, FilmNoir,
    ##     GameShow, History, Horror, imdbRating, Music, Musical,
    ##     Mystery, News, nrOfGenre, nrOfNewsArticles, nrOfNominations,
    ##     nrOfPhotos, nrOfUserReviews, nrOfWins, ratingCount, RealityTV,
    ##     Romance, SciFi, Short, Sport, TalkShow, Thriller, time_period,
    ##     title, url, War, Western, year

``` r
test_imdbRating.train <- data.frame(imdbRating = c(0,1,2,3,4,5,6,7,8,9,10),
                              duration.min = mean(duration.min, na.rm = TRUE),
                              year = mean(year, na.rm = TRUE),
                              nrOfPhotos = mean(nrOfPhotos),
                              nrOfNewsArticles = mean(nrOfNewsArticles),
                              nrOfUserReviews = mean(nrOfUserReviews),
                              nrOfGenre = mean(nrOfGenre))

test_imdbRating.train$prob <- predict(oscar_nomination2_reduced.train, newdata = test_imdbRating.train, type = "response")
test_imdbRating.train
```

    ##    imdbRating duration.min     year nrOfPhotos nrOfNewsArticles
    ## 1           0     101.8777 1987.471    22.2952         253.6041
    ## 2           1     101.8777 1987.471    22.2952         253.6041
    ## 3           2     101.8777 1987.471    22.2952         253.6041
    ## 4           3     101.8777 1987.471    22.2952         253.6041
    ## 5           4     101.8777 1987.471    22.2952         253.6041
    ## 6           5     101.8777 1987.471    22.2952         253.6041
    ## 7           6     101.8777 1987.471    22.2952         253.6041
    ## 8           7     101.8777 1987.471    22.2952         253.6041
    ## 9           8     101.8777 1987.471    22.2952         253.6041
    ## 10          9     101.8777 1987.471    22.2952         253.6041
    ## 11         10     101.8777 1987.471    22.2952         253.6041
    ##    nrOfUserReviews nrOfGenre        prob
    ## 1         131.6703  2.320337 0.005616174
    ## 2         131.6703  2.320337 0.011640487
    ## 3         131.6703  2.320337 0.023971133
    ## 4         131.6703  2.320337 0.048719643
    ## 5         131.6703  2.320337 0.096493193
    ## 6         131.6703  2.320337 0.182142938
    ## 7         131.6703  2.320337 0.317132649
    ## 8         131.6703  2.320337 0.491984401
    ## 9         131.6703  2.320337 0.668819395
    ## 10        131.6703  2.320337 0.808108395
    ## 11        131.6703  2.320337 0.897769280

``` r
detach(IMDB.train)
```

What's going on here is that based on our training dataset, our logistic regression model is making predictions of the estimated probability of winning an Oscar nomination given a certain value for **imdbRating**. The model suggests that for an **imdbRating** of 0-4, chances are very slim for getting an Oscar nomination with probabilities lower than 10%. For values between 5-7 for **imdbRating** we see the probability rise from nerly 20% to 50%. At this point, I would consider the odds of nomination pretty good, nearing the odds of a coin flip. For an **imdbRating** of 8-10, a movie is basically a shoe-in for the Oscar nod with probabilities ranging from about 67% to 90% chances.

Let's examine this relationship graphically...

``` r
plot(test_imdbRating.train$imdbRating, test_imdbRating.train$prob,
     xlab = "IMDB Rating", ylab = "Probability of Nomination", type = "o", pch = 19, col = "green", fg = "blue",
     main = "Oscar Nomination Likelihood", yaxp = c(0.0, 1, 2), ylim = c(0,1))
abline(h = 0, col = "black", lty = 2)
abline(h = 1, col = "black", lty = 2)
abline(h = .5, col = "gray", lty = 2)
text(test_imdbRating.train$imdbRating, test_imdbRating.train$prob, labels = round(test_imdbRating.train$prob, 2), cex = .7)
```

![unnamed-chunk-20-1](https://user-images.githubusercontent.com/32421511/39900734-e3fb2a8a-5491-11e8-8ae3-1af7d79e163b.png)

This graph gives a general idea of how the model works. It takes as inputs the predictor variables from the model and generates estimates of the probability of an Oscar nomination based upon all predictor values (in this case only IMDB Rating). Our next step is to to test the model on the validation dataset and observe how well the model predicts the outcome of any given movie it comes across based upon its characteristics (predictors).

##### Model Performance Test

``` r
# True Classification vs. Model classification
prob <- predict(oscar_nomination2_reduced.train, IMDB.validate, type = "response") # Predicts probability of Oscar nomination with the given model used over the data in the validation dataset
head(prob)
```

    ##         1         2         3         5         6         8 
    ## 0.2420676 0.4560173 0.8873315 0.5974907 0.6453247 0.5924638

``` r
logit.pred <- factor(prob > .45, levels = c(FALSE, TRUE), labels = c("No", "Yes")) # Takes all observations randomly chosen from the validation sample and categorizes them as greater than or less than 45% chance of Oscar nomination based on the information given in the validation dataset. 
head(logit.pred)
```

    ##   1   2   3   5   6   8 
    ##  No Yes Yes Yes Yes Yes 
    ## Levels: No Yes

``` r
logit.performance <- table(IMDB.validate$ynNominations, logit.pred, dnn = c("Actual", "Predicted")) # Calculates the accuracy of predictions on the actual values.
logit.performance 
```

    ##       Predicted
    ## Actual   No  Yes
    ##    No  1832  503
    ##    Yes  526 1311

``` r
library(vcd)
```

    ## Loading required package: grid

``` r
lbls <- round(prop.table(logit.performance), 2)
mosaic(logit.performance, shade = TRUE, legend = F, main = "Model Performance Table", pop = FALSE) # False Positive rate is 12%. False Negative rate is 28.6%. Accuracy of the model in prediction of Oscar Nominations is 75%
labeling_cells(text = lbls, margin = 0, col = "white") (logit.performance)
```

![unnamed-chunk-21-1](https://user-images.githubusercontent.com/32421511/39900740-ebd51bb2-5491-11e8-8be0-3fc301aa0636.png)

| Confusion Matrix Element | Percentage |
|--------------------------|------------|
| Accuracy                 | 75%        |
| Misclassification Rate   | 25%        |
| True Positive Rate       | 70%        |
| False Positive Rate      | 21%        |
| Specificity              | 78%        |
| Precision                | 72%        |
| Prevalence               | 44%        |

Conclusions
-----------

Based upon our results, we find that our predictive model is relatively accurate for Oscar nominations. **Our performance table showed that 75% of the predictions were correct! The model is better at detecting which movies will be nominated for Oscars (21% False Positive) than it is at detecting which movies will not be nominated (29.5% False Negative).** If I were to continue with the project with more time, I would note that just because models are statistically significant, doesn't necessarily make them practically significant. There are some variables in our model that increase the probability of winning an Oscar by a meaningless multiplicative value of 1. Practically speaking, they don't really influence the probability of Oscar nomination by much, if anything at all. Some motivations for further study would be to drop these variables that are significant, but hold no practical application. This may eliminate noise from our model and lead to more confidence in our predictions of Oscar nominations.

As a special note to film directors, it is strongly suggested that they check out IMDB's website for reviews when they want to know about the likelihood of their movie winning an Oscar!!
