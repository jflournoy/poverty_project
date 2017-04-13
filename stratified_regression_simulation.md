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
      
      y <- X%*%as.numeric(params)
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

    ##          itnr        educ          c     impuls   planful          y
    ## 1  0.35383218  0.71608131  0.5049562  1.9475718 1.9907803  1.0140368
    ## 2  1.96047776  0.04959444 -1.4274062 -0.8135715 0.7736104  0.7780415
    ## 3  1.15044785  1.99699974  0.6095766  2.5806093 0.4877466  1.4521494
    ## 4 -0.02350452 -0.50563434  1.1903845  0.7379972 0.5586428  0.3107261
    ## 5  1.97765237 -0.12577823  0.4420169  2.6304827 1.3958758  1.5942973
    ## 6 -1.72541694 -1.03788186 -0.3644975 -0.4929629 0.3117913 -1.1301224

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

    summary(svyMod <- svyglm(y ~ 1 + itnr + educ + c + impuls + planful, design = strataRakeSVY))

    ## 
    ## Call:
    ## svyglm(formula = y ~ 1 + itnr + educ + c + impuls + planful, 
    ##     design = strataRakeSVY)
    ## 
    ## Survey design:
    ## rake(design = strataSVY, sample.margins = list(~income), population.margins = list(income_dist))
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.05151    0.02653   1.942   0.0529 .  
    ## itnr         0.52205    0.03143  16.611  < 2e-16 ***
    ## educ         0.18991    0.02668   7.118 5.23e-12 ***
    ## c            0.22059    0.03265   6.757 5.08e-11 ***
    ## impuls       0.16050    0.02542   6.314 7.33e-10 ***
    ## planful      0.26210    0.03177   8.251 2.39e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.1721894)
    ## 
    ## Number of Fisher Scoring iterations: 2

    summary(glmMod <- glm(y ~ 1 + itnr + educ + c + impuls + planful, data = strataDF))

    ## 
    ## Call:
    ## glm(formula = y ~ 1 + itnr + educ + c + impuls + planful, data = strataDF)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.12076  -0.21578  -0.04381   0.16223   1.24614  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.13728    0.01785   7.689  1.2e-13 ***
    ## itnr         0.38999    0.01776  21.963  < 2e-16 ***
    ## educ         0.24658    0.01895  13.014  < 2e-16 ***
    ## c            0.23866    0.01823  13.090  < 2e-16 ***
    ## impuls       0.21321    0.01900  11.223  < 2e-16 ***
    ## planful      0.29624    0.01867  15.865  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.1273545)
    ## 
    ##     Null deviance: 378.204  on 399  degrees of freedom
    ## Residual deviance:  50.178  on 394  degrees of freedom
    ## AIC: 318.79
    ## 
    ## Number of Fisher Scoring iterations: 2

    cat('Absolute deviation of coefficients:')

    ## Absolute deviation of coefficients:

    round((coef(glmMod) - coef(svyMod)),2)

    ## (Intercept)        itnr        educ           c      impuls     planful 
    ##        0.09       -0.13        0.06        0.02        0.05        0.03

    cat('Proportional deviation of coefficients:')

    ## Proportional deviation of coefficients:

    round((coef(glmMod) - coef(svyMod))/coef(svyMod),2)

    ## (Intercept)        itnr        educ           c      impuls     planful 
    ##        1.66       -0.25        0.30        0.08        0.33        0.13
