We might find
[this](http://tophcito.blogspot.com/2014/04/survey-computing-your-own-post.html)
helpful.

    if(!require(survey)){
      install.packages('survey')
    } 

    ## Loading required package: survey

    ## Loading required package: grid

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

    ## Loading required package: survival

    ## 
    ## Attaching package: 'survey'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     dotchart

    library(survey)

    ?postStratify(design, strata, population, partial = FALSE)
    ?svydesign()
    ?rake()

Simulate some data
==================

We want to create a set of correlated predictors, and then to compute
our outcome variable from those predictors to create a realistic
scenario. The model we will eventually be estimating is:

DELAYDISCOUNT = *β*<sub>0</sub> + *β*<sub>1</sub>ITNR + *β*<sub>2</sub>EDUC + *β*<sub>3</sub>C + *β*<sub>4</sub>IMPULS + *β*<sub>5</sub>PLANFUL + *ϵ*.

We assume that these are all correlated to some degree. I'll start with
some pretty unconstrained assumptions. I'm wrapping this in a function
so that we can use it later to make slightly different data for
different strata.

    generate_data <- function(gen_info){
      require(MASS)
      N <- gen_info$N
      params <- gen_info$params
      nvars <- length(params[-1])
      cormat <- diag(nvars)
      cormat[lower.tri(cormat)] <- runif(nvars, .1, .3)
      
      X <- cbind(rep(1, N), mvrnorm(N, mu = rep(0, nvars), Sigma = cormat))
      colnames(X) <- names(params)
      
      y <- X%*%as.numeric(params)+rnorm(N, 0, 1)
      return(data.frame(X[,-1], y=y))
    }

    params <- list('I' = 0, 'itnr' = .5, 'educ' = .2, 'c' = .2, 'impuls' = .1, 'planful' = .2)

    simDF <- generate_data(list(params = params, N = 200))

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    head(simDF)

    ##          itnr       educ           c     impuls    planful          y
    ## 1  1.99904498 -0.1038574  0.98238476  1.0969754  0.3478878  2.1936332
    ## 2  1.45072667  0.8400415 -0.13525350  0.2240975  0.7885432  2.0310455
    ## 3 -0.65101871  1.0729948 -0.59736318  0.3157750 -1.4793220  0.3876046
    ## 4  0.03692116  0.2931264  0.95833579 -1.0404636  0.3696342  1.7089557
    ## 5  2.26005445 -1.5055399 -1.33382528  0.4628079 -0.6861988  0.7718761
    ## 6 -0.30808425 -1.2065081 -0.06316038  0.8242748  1.8878712 -0.3950116

Simulate strata
---------------

We need to get slightly different parameters for each strata. Let's keep
it simple for now -- three strata on one variable

    strata_params <- list(high = list(N = 200, params = list('I' = 0, 'itnr' = .5, 'educ' = .2, 'c' = .2, 'impuls' = .3, 'planful' = .2)),
                          med = list(N = 150, params = list('I' = .4, 'itnr' = .1, 'educ' = .3, 'c' = .25, 'impuls' = .1, 'planful' = .5)),
                          low = list(N = 50, params = list('I' = -.2, 'itnr' = .8, 'educ' = .1, 'c' = .15, 'impuls' = .2, 'planful' = .1)))

    strata_data_list <- lapply(strata_params, generate_data)
    strataDF <- bind_rows(strata_data_list, .id = 'income')

Now we have a big data set with different representation among the three
strata. We can now start using the survey package and account for the
true population frequency of these groups. In this simulation I'm
assuming that the sample representation is inversly related to the
population frequency.

Use survey
----------

First we prepare our data with the information we have about the
population.

    #turn data into survey design
    strataSVY <- svydesign(ids = ~1, data = strataDF)

    ## Warning in svydesign.default(ids = ~1, data = strataDF): No weights or
    ## probabilities supplied, assuming equal probability

    #true population proportion, turned into an expected frequency given our total sample size
    income_dist <- data.frame(income = c('high', 'med', 'low'), 
                              Freq = nrow(strataDF) * c(.2, .3, .5))
    strataRakeSVY <- rake(design = strataSVY,
                          sample.margins = list(~income),
                          population.margins = list(income_dist))

    summary(weights(strataRakeSVY)) #see blog post about trimming

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     0.4     0.4     0.6     1.0     0.8     4.0

    summary(strataRakeSVY)

    ## Independent Sampling design (with replacement)
    ## rake(design = strataSVY, sample.margins = list(~income), population.margins = list(income_dist))
    ## Probabilities:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.250   1.250   1.875   1.750   2.500   2.500 
    ## Data variables:
    ## [1] "income"  "itnr"    "educ"    "c"       "impuls"  "planful" "y"

Analysizor the datums
---------------------

    summary(svyMod <- svyglm(y ~ (1 + itnr + educ + c + impuls + planful), design = strataRakeSVY))

    ## 
    ## Call:
    ## svyglm(formula = y ~ (1 + itnr + educ + c + impuls + planful), 
    ##     design = strataRakeSVY)
    ## 
    ## Survey design:
    ## rake(design = strataSVY, sample.margins = list(~income), population.margins = list(income_dist))
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.13026    0.07648  -1.703 0.089332 .  
    ## itnr         0.46564    0.08110   5.742 1.88e-08 ***
    ## educ         0.16349    0.07591   2.154 0.031875 *  
    ## c            0.14087    0.07402   1.903 0.057756 .  
    ## impuls       0.27357    0.07416   3.689 0.000257 ***
    ## planful      0.26675    0.06864   3.886 0.000120 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 1.151396)
    ## 
    ## Number of Fisher Scoring iterations: 2

    summary(glmMod <- glm(y ~ 1 + itnr + educ + c + impuls + planful, data = strataDF))

    ## 
    ## Call:
    ## glm(formula = y ~ 1 + itnr + educ + c + impuls + planful, data = strataDF)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8383  -0.7132   0.0044   0.6346   3.4081  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.05880    0.05306   1.108  0.26845    
    ## itnr         0.32322    0.05239   6.170 1.70e-09 ***
    ## educ         0.25449    0.05567   4.572 6.49e-06 ***
    ## c            0.18551    0.05750   3.226  0.00136 ** 
    ## impuls       0.29095    0.05593   5.202 3.18e-07 ***
    ## planful      0.31866    0.05408   5.893 8.15e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 1.120559)
    ## 
    ##     Null deviance: 725.38  on 399  degrees of freedom
    ## Residual deviance: 441.50  on 394  degrees of freedom
    ## AIC: 1188.6
    ## 
    ## Number of Fisher Scoring iterations: 2

    cat('Absolute deviation of coefficients:')

    ## Absolute deviation of coefficients:

    round((coef(glmMod) - coef(svyMod)),2)

    ## (Intercept)        itnr        educ           c      impuls     planful 
    ##        0.19       -0.14        0.09        0.04        0.02        0.05

    cat('Proportional deviation of coefficients:')

    ## Proportional deviation of coefficients:

    round((coef(glmMod) - coef(svyMod))/coef(svyMod),2)

    ## (Intercept)        itnr        educ           c      impuls     planful 
    ##       -1.45       -0.31        0.56        0.32        0.06        0.19
