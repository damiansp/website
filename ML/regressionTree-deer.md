Regression Trees to Predict Fetal Health in White-Tailed Deer
=============================================================

In the statistics section, I presented an analysis of the robustness of fetal deer, where the "robustness" was defined as the ratio: ln(mass) / ln(length).  In general, the higher this ratio, the healthier the fetus, and this in turn is a good predictor that the fawn will survive its first year after birth.

In that section, a generalized linear model was used, and indicated that the sex and number of fetuses, age of the mother, and intensity of culling (localized killing of deer for disease control) were all significant, as were a number of interaction and nonlinear effects.  However, the fit of that model was not especially good (Nagelkerke's r^2 = 0.19), so here I investigate how a regression tree performs on the same analysis, with the same set of available predictors.  After finding a decent regression tree, I use k-folds to determine how well each method performs at predicting novel data.


```r
rm(list = ls())
library(rpart)

deer <- read.csv('~/Desktop/NewDeer/Data/Fetus_Final.csv')
deer <- subset(deer, days > 31) # see the <a href="http://phillips-research.com/deerSS2.html">"Fetus Detection"</a> section.
deer$age <- as.factor(deer$age3categories)
# reorder levels to be increasing ages
deer$age <- factor(deer$age, levels = c('fawn', 'yearling', 'adult'))
names(deer)[46] <- 'intensity'
names(deer)[50] <- 'cwd' # disease status for chronic wasting disease

# I treat year as a factor because I am not looking for trends over time, but do 
# want to allow for annual variation that might be due to precipitation, temperature,
# etc., that may either affect the mothers directly, or indirectly by affecting their
# food resources.
deer$yearF <- as.factor(deer$year)

# calculate the fetal size ("robustness") statistic.  (For each pregnant mother, I
# take the average of all her fetuses).
deer$fsRatio <- log(deer$wtMean) / log(deer$crMean)

# Because of the log/log ratio we get some -Inf values, as well as NA values where 
# one or more of the inputs was missing. Remove these records:
problems = which(!is.finite(deer$fsRatio))

# verify
# deer$fsRatio[problems]
deer = deer[-problems, ]

# Initial regression tree for fetal size ratio
fsr.rt <- rpart(fsRatio ~ maleNo + fetusNo + age + intensity + yearF + cwd, 
				data = deer)
fsr.rt
```

```
## n= 2422 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 2422 84.03510 1.841085  
##   2) age=fawn 217 29.62357 1.663939 *
##   3) age=yearling,adult 2205 46.93180 1.858518 *
```

```r
# show as tree
plot(fsr.rt)
text(fsr.rt)
```

![plot of chunk setup](figure/setup-1.png) 

This tree is our initial go, and can be read as follows.  If the mother is either a yearling or adult, the expected fetal size ratio is 1.859 (right branch).  If the mother is a fawn, the expected ratio is 1.664.

This same result can be arrived at by simply taking the means of each group:

```r
(group.means <- tapply(deer$fsRatio[is.finite(deer$fsRatio)], 
	   				   (deer$age == 'fawn')[is.finite(deer$fsRatio)], mean))
```

```
##    FALSE     TRUE 
## 1.858495 1.663939
```

This is the default tree which the rpart library returns to us in an effort to fit the best tree without overfitting.

We can visualize the affect of the tree split by plotting the data.

```r
plot(deer$fsRatio ~ jitter(as.numeric(deer$age)), pch = 16, 
	 col = rgb(0, 0, 0, 0.2), xlab = 'Age', ylab = 'Fetal size ratio', 
	 xaxt = 'n')
axis(1, labels = c('fawn', 'yearling', 'adult'), at = c(1:3))
abline(h = mean(deer$fsRatio), col = 'grey')
abline(v = 1.5, lwd = 2)
lines(c(0, 1.5), rep(group.means[2], 2), col = 2)
lines(c(1.5, 3.5), rep(group.means[1], 2), col = 4)
legend('bottomright', lwd = c(2, 1, 1, 1), col = c('black', 'grey', 'red', 'blue'),
	   legend = c('partition', 'overall mean', 'fawn mean', 
	   			  'yearling/adult mean'))
```

![plot of chunk visualize](figure/visualize-1.png) 

One problem that immediately stands out is that a mean is being used to predict central tendencies, even though the data (especially for fawns) are skewed.  Attempt to normalize the data.


```r
hist(deer$fsRatio, main = '', xlab = 'Fetal size ratio')
```

![plot of chunk normalize](figure/normalize-1.png) 

```r
# shift values to all be non-negative
fs.trans = deer$fsRatio - min(deer$fsRatio)
hist(fs.trans)
```

![plot of chunk normalize](figure/normalize-2.png) 

```r
# try exponential transforms to minimize the p-value of the Shapiro-Wilk test for 
# normality

(best.p <- shapiro.test(fs.trans)$p)  
```

```
## [1] 3.633909e-54
```

```r
# untransformed is highly non-normal, as expected
exponents = seq(6, 10, 0.1)
ps = numeric(length(exponents))

for (e in 1:length(exponents)) {
	ps[e] = shapiro.test(fs.trans^exponents[e])$p
}

plot(ps ~ exponents, type = 'l')
```

![plot of chunk normalize](figure/normalize-3.png) 

```r
# The optimal value is somewhere around 9-- make finer resolution tests around 
# this value

exponents = seq(8.8, 9.5, 0.01)
ps <- numeric(length(exponents))

for (e in 1:length(exponents)) {
	ps[e] <- shapiro.test(fs.trans^exponents[e])$p
}

plot(ps ~ exponents, type = 'l')
best.p.index <- which(ps == max(ps))
abline(h = ps[best.p.index], col = 'grey')
abline(v = exponents[best.p.index], lty = 2)
```

![plot of chunk normalize](figure/normalize-4.png) 

```r
fs.trans <- fs.trans^9.15
hist(fs.trans, main = '', xlab = expression(('fetal size ratio')^9.15))
```

![plot of chunk normalize](figure/normalize-5.png) 

Now reattempt the tree.

```r
fsr.rt2 <- rpart(fs.trans ~ maleNo + fetusNo + age + intensity + yearF + cwd, 
				data = deer)
fsr.rt2
```

```
## n= 2422 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 2422 40405110000 10587.630  
##   2) age=fawn 217  5026803000  7554.199 *
##   3) age=yearling,adult 2205 33185030000 10886.160  
##     6) yearF=2008,2009,2010,2012 966 10757870000 10087.120 *
##     7) yearF=2003,2004,2005,2006,2007,2011 1239 21329560000 11509.130 *
```

```r
# show as tree
plot(fsr.rt2, compress = T, margin = 0.1)
text(fsr.rt2)
```

![plot of chunk tree_transformed](figure/tree_transformed-1.png) 

```r
# Also, create a function to revert transformed values back to original scale:
fs.inverse = function(x) {
	x^(1/9.5) + min(deer$fsRatio)
}

# Convert end node values
ends <- c(7554.199, 10087.160, 11509.130)
fs.inverse(ends)
```

```
## [1] 1.680275 1.759392 1.796282
```

Fawns are predicted to have fetuses with an fs ratio of 1.68.  For adults and yearlings, the predicted ratio is 1.76 in years 2008, '09, '10, and '12, and 1.80 in 2003 - 07 and 2011.  In the generalized linear model, a number of squared terms were also significant, so let's consider those here.


```r
fsr.rt3 <- rpart(fs.trans ~ maleNo + I(maleNo^2) + fetusNo + I(fetusNo^2) + age + 
				 intensity + I(intensity^2) + yearF + cwd, data = deer) 
fsr.rt3
```

```
## n= 2422 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 2422 40405110000 10587.630  
##   2) age=fawn 217  5026803000  7554.199 *
##   3) age=yearling,adult 2205 33185030000 10886.160  
##     6) yearF=2008,2009,2010,2012 966 10757870000 10087.120 *
##     7) yearF=2003,2004,2005,2006,2007,2011 1239 21329560000 11509.130 *
```

This apppears to have had no effect.

Before moving on, let's again visualize the effect of the existing partitions.
First let's color-code fetal size ratios so that they transition from green at the minimum value to red at the max, so that color can serve as a visual proxy for this value.


```r
colorScale = colorRamp(c('green', 'red'))(fs.trans / max(fs.trans))
deer$fs.colors <- rgb(colorScale[, 1] / 255, colorScale[, 2] / 255, 0)

# also group years by tree decision
deer$yrA <- deer$yearF %in% c('2008','2009', '2010', '2012')

plot(jitter(as.numeric(age), factor = 2) ~ jitter(as.numeric(yrA), factor = 2), 
	 col = fs.colors, data = deer, pch = 16, xlab = 'Year Group', 
	 ylab = 'Age Group', xaxt = 'n', yaxt = 'n')
axis(1, at = c(0, 1), labels = c('2003 - 07, 2011', '2008 - 10, 2012'))
axis(2, at = c(1, 2, 3), labels = c('fawn', 'yearling', 'adult'))
abline(h = 1.5, lwd = 2)
text(0, 1.55, 'First Partition')
lines(c(0.5, 0.5), c(1.5, 3.5), lwd = 2)
text(0.45, 2.5, 'Second Partition', srt = 90)
```

![plot of chunk color_vis](figure/color_vis-1.png) 

Now examine the cross-validation error at different levels of complexity in the tree.  We choose the cp (complexity parameter) that minimizes the cross-validation error.


```r
set.seed(42)
fsr.rt4 <- rpart(fs.trans ~ maleNo + I(maleNo^2) + fetusNo + I(fetusNo^2) + age + 
				 intensity + I(intensity^2) + yearF + cwd, data = deer, 
				 cp = 0.001) 
plotcp(fsr.rt4)
```

![plot of chunk choose_cp](figure/choose_cp-1.png) 

```r
fsr.rt4.prune = prune.rpart(fsr.rt4, 0.0039245)

plot(fsr.rt4.prune, uniform = T, compress = T)
text(fsr.rt4.prune, cex = 0.6)
```

![plot of chunk choose_cp](figure/choose_cp-2.png) 

As this is still ultimately a regression model, we can perform the same diagnostics, and use the sum of squared errors (SSE) of the model residuals relative to the squared deviations of the fetal size ratios from the group mean as a metric of goodness of fit.

```r
plot(jitter(predict(fsr.rt4.prune), factor = 5), resid(fsr.rt4.prune), 
	 xlab = 'Fitted', ylab = 'Residuals')
```

![plot of chunk diagnostics](figure/diagnostics-1.png) 

```r
qqnorm(resid(fsr.rt4.prune))
qqline(resid(fsr.rt4.prune))
```

![plot of chunk diagnostics](figure/diagnostics-2.png) 

```r
# Goodness of fit
1 - sum(resid(fsr.rt4.prune)^2) / sum((fs.trans - mean(fs.trans))^2)
```

```
## [1] 0.1276225
```

Compared to the GLM, the fit would appear to be poorer.  But to actually compare each method directly, let's do a quick and dirty comparison between the regression tree and a similar linear model.  Rather than doing k-folds, which is ideal, for brevity I will just do a single test.

First split the data into test and development sets:


```r
dim(deer) 
```

```
## [1] 2422   55
```

```r
# 2422 rows, so put aside 250 (randomly chosen) for testing.
test.records = sample(1:2422, size = 250)
deer$fs.trans = fs.trans
rm(fs.trans)

test.set = deer[test.records, ]
dev.set = deer[-test.records, ]

# First the regression tree
fsr.rt = rpart(fs.trans ~ maleNo + I(maleNo^2) + fetusNo + I(fetusNo^2) + age + 
			   intensity + I(intensity^2) + yearF + cwd, data = dev.set, 
			   cp = 0.001) 
plotcp(fsr.rt)
```

![plot of chunk dev_test](figure/dev_test-1.png) 

```r
# In actuality, the cross-validation run here is also randomized, so the cp value
# with the lowest cross-validation error will vary between runs.  The researcher 
# could repeat the test many times and keep track of the best cp value, or some 
# similar approach.  Here though, I simply take the best value from the first run.

printcp(fsr.rt)
```

```
## 
## Regression tree:
## rpart(formula = fs.trans ~ maleNo + I(maleNo^2) + fetusNo + I(fetusNo^2) + 
##     age + intensity + I(intensity^2) + yearF + cwd, data = dev.set, 
##     cp = 0.001)
## 
## Variables actually used in tree construction:
## [1] age       fetusNo   intensity maleNo    yearF    
## 
## Root node error: 3.6402e+10/2172 = 16759653
## 
## n= 2172 
## 
##           CP nsplit rel error  xerror     xstd
## 1  0.0570046      0   1.00000 1.00103 0.032453
## 2  0.0275008      1   0.94300 0.94566 0.030817
## 3  0.0099837      2   0.91549 0.92336 0.030945
## 4  0.0084883      3   0.90551 0.92822 0.030993
## 5  0.0081505      4   0.89702 0.92152 0.030655
## 6  0.0081307      5   0.88887 0.91949 0.030568
## 7  0.0064317      6   0.88074 0.91209 0.030285
## 8  0.0062504      7   0.87431 0.90562 0.030403
## 9  0.0058544      9   0.86181 0.90562 0.030403
## 10 0.0044364     10   0.85595 0.89884 0.030095
## 11 0.0034976     11   0.85152 0.90930 0.030577
## 12 0.0033093     12   0.84802 0.90912 0.030446
## 13 0.0033066     13   0.84471 0.90912 0.030446
## 14 0.0031048     14   0.84140 0.90972 0.030538
## 15 0.0027861     15   0.83830 0.91331 0.030793
## 16 0.0025514     16   0.83551 0.91475 0.031012
## 17 0.0024831     17   0.83296 0.91349 0.031013
## 18 0.0022914     19   0.82800 0.91841 0.031339
## 19 0.0019548     21   0.82341 0.92525 0.031476
## 20 0.0018317     24   0.81744 0.92972 0.031453
## 21 0.0017794     27   0.81194 0.92776 0.031418
## 22 0.0015706     30   0.80660 0.92647 0.031578
## 23 0.0015209     33   0.80177 0.92923 0.031607
## 24 0.0015140     34   0.80025 0.93200 0.031734
## 25 0.0014741     35   0.79873 0.93173 0.031806
## 26 0.0014127     39   0.79284 0.93377 0.031834
## 27 0.0013616     42   0.78860 0.93617 0.031936
## 28 0.0013154     43   0.78724 0.93813 0.032088
## 29 0.0012639     45   0.78461 0.93829 0.032141
## 30 0.0011914     49   0.77955 0.93817 0.032162
## 31 0.0011509     50   0.77836 0.93656 0.032157
## 32 0.0011365     51   0.77721 0.93799 0.032256
## 33 0.0011115     52   0.77607 0.93416 0.032324
## 34 0.0010948     53   0.77496 0.93947 0.032476
## 35 0.0010561     54   0.77387 0.94201 0.032519
## 36 0.0010000     56   0.77175 0.94281 0.032568
```

```r
fsr.rt.prune = prune.rpart(fsr.rt, cp = 0.0044364)
fsr.rt.prune
```

```
## n= 2172 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
##   1) root 2172 36401970000 10616.920  
##     2) age=fawn 192  4467545000  7478.074  
##       4) fetusNo< 1.5 137  2599759000  6691.398 *
##       5) fetusNo>=1.5 55  1571813000  9437.614  
##        10) yearF=2005,2008,2010,2011,2012 30   523497800  7554.162 *
##        11) yearF=2004,2006,2007,2009 25   814186800 11697.760 *
##     3) age=yearling,adult 1980 29859340000 10921.290  
##       6) yearF=2008,2009,2010,2012 878  9920476000 10124.680  
##        12) intensity>=3.1 571  6585814000  9763.427 *
##        13) intensity< 3.1 307  3121548000 10796.580 *
##       7) yearF=2003,2004,2005,2006,2007,2011 1102 18937790000 11555.970  
##        14) intensity>=15.66667 135  2403913000 10019.010  
##          28) intensity< 21.83333 96  1150062000  9074.111 *
##          29) intensity>=21.83333 39   957158500 12344.900 *
##        15) intensity< 15.66667 967 16170450000 11770.540  
##          30) age=yearling 160  3004397000 10501.040 *
##          31) age=adult 807 12857060000 12022.240  
##            62) yearF=2004,2011 338  4962653000 11419.620 *
##            63) yearF=2003,2005,2006,2007 469  7683195000 12456.550  
##             126) intensity< 12.25 450  7065574000 12308.380 *
##             127) intensity>=12.25 19   373774200 15965.690 *
```

```r
plot(fsr.rt.prune, uniform = T, compress = T)
text(fsr.rt.prune, cex = 0.6)
```

![plot of chunk dev_test](figure/dev_test-2.png) 

It is reassuring that the splits are almost (though not perfectly) identical to those we got above when using the full data set.  As before, check diagnostics and goodness-of-fit, then go on to make predictions with the test set.

```r
plot(jitter(predict(fsr.rt.prune), factor = 5), resid(fsr.rt.prune), 
	 xlab = 'Fitted', ylab = 'Residuals')
```

![plot of chunk diagnostics2](figure/diagnostics2-1.png) 

```r
qqnorm(resid(fsr.rt.prune))
qqline(resid(fsr.rt.prune))
```

![plot of chunk diagnostics2](figure/diagnostics2-2.png) 

```r
1 - sum(resid(fsr.rt.prune)^2) / 
  sum((dev.set$fs.trans - mean(dev.set$fs.trans))^2)
```

```
## [1] 0.1440456
```

```r
# Make predictions and save SSE
rt.preds = predict(fsr.rt.prune, newdata = test.set)
(rs.sse = sum((rt.preds - test.set$fs.trans)^2))
```

```
## [1] 3946882461
```

Now do the same with a LM.

```r
#fsr.lm = lm(fs.trans ~ maleNo + I(maleNo^2) + fetusNo + I(fetusNo^2) + age + 
#			intensity + I(intensity^2) + yearF + cwd, data = dev.set)

# fsr.lm = step(fsr.lm, direction = 'both')
fsr.lm = lm(fs.trans ~ fetusNo + I(fetusNo^2) + age + intensity + I(intensity^2) +
			yearF + cwd, data = dev.set)
summary(fsr.lm)
```

```
## 
## Call:
## lm(formula = fs.trans ~ fetusNo + I(fetusNo^2) + age + intensity + 
##     I(intensity^2) + yearF + cwd, data = dev.set)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -12534  -2728     12   2695  22399 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     6111.931    930.429   6.569 6.34e-11 ***
## fetusNo         2376.934    659.601   3.604 0.000321 ***
## I(fetusNo^2)    -539.161    163.595  -3.296 0.000998 ***
## ageyearling     2394.189    360.561   6.640 3.96e-11 ***
## ageadult        3248.145    325.316   9.985  < 2e-16 ***
## intensity       -119.179     27.178  -4.385 1.22e-05 ***
## I(intensity^2)     2.925      1.001   2.922 0.003516 ** 
## yearF2004         -9.946    780.919  -0.013 0.989839    
## yearF2005        854.935    777.776   1.099 0.271802    
## yearF2006        396.220    767.551   0.516 0.605759    
## yearF2007        742.467    755.591   0.983 0.325900    
## yearF2008      -1005.760    753.833  -1.334 0.182282    
## yearF2009       -800.075    768.006  -1.042 0.297643    
## yearF2010      -1331.774    786.023  -1.694 0.090350 .  
## yearF2011       -431.165    753.081  -0.573 0.567020    
## yearF2012      -1009.190    774.488  -1.303 0.192700    
## cwdpos          -768.855    539.630  -1.425 0.154366    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3856 on 2140 degrees of freedom
##   (15 observations deleted due to missingness)
## Multiple R-squared:  0.1225,	Adjusted R-squared:  0.1159 
## F-statistic: 18.67 on 16 and 2140 DF,  p-value: < 2.2e-16
```

```r
# The goodness of fit is quite similar to the regression tree.
# Diagnostics:
par(mfrow = c(2, 2))
plot(fsr.lm)
```

![plot of chunk glm](figure/glm-1.png) 

```r
# ...also look pretty good... maybe a very slight increase in variance of the 
# residuals as fitted values increase...
# And now the moment of truth:
lm.preds = predict(fsr.lm, newdata = test.set)
(lm.sse = sum((lm.preds[!is.na(lm.preds)] - 
  test.set$fs.trans[!is.na(lm.preds)])^2))
```

```
## [1] 3740503782
```

```r
# Because we got an NA in the lm predictions, we have to compare SSE per number of 
# predictions:
cbind(rs.sse / 250, lm.sse / 249)
```

```
##          [,1]     [,2]
## [1,] 15787530 15022104
```

The error is less for the linear model in this case, and the astute observer may have noticed too, that the model employed here was somewhat simpler than the one given in statistics section (which was even better fitting) for the sake of making the two models directly comparable.  Nevertheless, regression trees are very fast, and in some cases a fast approximation may be preferred to a more precise, but also more time-consuming analysis.  And, of course, there is no guarantee that a linear model will always outperform an equivalent regression tree.  Note too, that the regression tree is somewhat more succinct in that, with 10 splits, it effectively has 11 parameters, whereas the linear model has 17.
