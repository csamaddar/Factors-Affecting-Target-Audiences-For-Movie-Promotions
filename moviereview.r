movie = read.csv(file.choose(),header=T)
head(movie)
str(movie)
View(movie)


summary(movie)
summary(movie$Age)
summary(movie$Gender)

library(UsingR)
barplot(movie$Liking)


occu = table(movie$Liking)
occu = table(movie$Liking,movie$Occupation)
occu
prop.table(occu)
barplot(occu,beside=T,legend.text = T, col = c("white","Black"))
chisq.test(occu)
# occu
# 
# a    H    p    s
# Like      382   72 1052  252
# Not Like  171   56  794  338
# > chisq.test(occu)
# 
# Pearson's Chi-squared test
# 
# data:  occu
# X-squared = 81.36, df = 3, p-value < 2.2e-16
gen = table(movie$Liking,movie$Gender)
gen
prop.table(gen)
barplot(gen,beside=T,legend.text = T, col = c("black","White"))
chisq.test(gen)
# gen
# 
# F    M
# Like      680 1078
# Not Like  455  904
# > chisq.test(gen)
# 
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  gen
# X-squared = 8.7274, df = 1, p-value = 0.003135

age= table(movie$Liking,movie$Age)
barplot(age,beside=T,legend.text = T, col = c("black","White"))
age
prop.table(age)
chisq.test(age)
# age
# 
# 19 to 35 36 to 50 51 above below 18
# Like         1005      300       84      369
# Not Like      703      263       73      320
# > prop.table(age)
# 
# 19 to 35   36 to 50   51 above   below 18
# Like     0.32242541 0.09624639 0.02694899 0.11838306
# Not Like 0.22553738 0.08437600 0.02341996 0.10266282
# > chisq.test(age)
# 
# Pearson's Chi-squared test
# 
# data:  age
# X-squared = 9.1602, df = 3, p-value = 0.02723
genre = table(movie$Liking,movie$genres)
barplot(genre,beside=T,legend.text = T, col = c("black","White"))
genre
prop.table(genre)
chisq.test(genre)



# genre
# 
# Action Animation Comedy Drama Horror Romance Sci-Fi
# Like         85        10    681   836    107       8     31
# Not Like     70         2    687   452    104      20     24
# > chisq.test(genre)
# 
# Pearson's Chi-squared test
# 
# data:  genre
# X-squared = 77.568, df = 6, p-value = 1.135e-14

#library(ggplot2)

#ggplot(movie,aes(x=Gender, y = genre, col = Liking)) + geom_line()

result = glm(Liking ~ genres -1, family ="binomial",movie)
summary(result)
round(exp(cbind(extimate = coef(result),confint(result))),2)

#                   extimate 2.5 % 97.5 %
# (Intercept)         0.82  0.60   1.13
# genresAnimation     0.24  0.04   0.96
# genresComedy        1.22  0.88   1.71
# genresDrama         0.66  0.47   0.92
# genresHorror        1.18  0.78   1.79
# genresRomance       3.04  1.30   7.71
# genresSci-Fi        0.94  0.50   1.74

result = glm(Liking ~ Gender-1, family ="binomial",movie)

summary(result)
round(exp(cbind(extimate = coef(result),confint(result))),2)

#               extimate 2.5 % 97.5 %
# (Intercept)     0.67  0.59   0.75
# GenderM         1.25  1.08   1.45
#  

result = glm(Liking ~ Age - 1, family ="binomial",movie)
summary(result)
round(exp(cbind(extimate = coef(result),confint(result))),2)
  
# > result = glm(Liking ~ Age, family ="binomial",movie)
# > summary(result)
# 
# Call:
#   glm(formula = Liking ~ Age, family = "binomial", data = movie)
# 
# Deviance Residuals: 
#   Min      1Q  Median      3Q     Max  
# -1.122  -1.030  -1.030   1.238   1.332  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.35739    0.04917  -7.269 3.63e-13 ***
#   Age36 to 50  0.22576    0.09774   2.310   0.0209 *  
#   Age51 above  0.21703    0.16739   1.297   0.1948    
# Agebelow 18  0.21491    0.09084   2.366   0.0180 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 4269.9  on 3116  degrees of freedom
# Residual deviance: 4260.7  on 3113  degrees of freedom
# AIC: 4268.7
# 
# Number of Fisher Scoring iterations: 4
# 
# > round(exp(cbind(extimate = coef(result),confint(result))),2)
# Waiting for profiling to be done...
# extimate 2.5 % 97.5 %
#   (Intercept)     0.70  0.64   0.77
# Age36 to 50     1.25  1.03   1.52
# Age51 above     1.24  0.89   1.72
# Agebelow 18     1.24  1.04   1.48

result = glm(Liking ~ Occupation, family ="binomial",movie)
summary(result)
round(exp(cbind(extimate = coef(result),confint(result))),2)

# > result = glm(Liking ~ Occupation, family ="binomial",movie)
# > summary(result)
# 
# Call:
#   glm(formula = Liking ~ Occupation, family = "binomial", data = movie)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.3044  -1.0605  -0.8602   1.2990   1.5321  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -0.80376    0.09201  -8.736  < 2e-16 ***
#   OccupationHomemaker     0.55244    0.20053   2.755  0.00587 ** 
#   OccupationProfessional  0.52239    0.10332   5.056 4.28e-07 ***
#   OccupationStudent       1.09737    0.12407   8.845  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 4269.9  on 3116  degrees of freedom
# Residual deviance: 4187.7  on 3113  degrees of freedom
# AIC: 4195.7
# 
# Number of Fisher Scoring iterations: 4
# 
# > round(exp(cbind(extimate = coef(result),confint(result))),2)
# Waiting for profiling to be done...
# extimate 2.5 % 97.5 %
#   (Intercept)                0.45  0.37   0.54
# OccupationHomemaker        1.74  1.17   2.57
# OccupationProfessional     1.69  1.38   2.07
# OccupationStudent          3.00  2.35   3.83


library(UsingR)

lm.result=simple.lm(x,y)
 es = resid(lm.result)
 b1 =(coef(lm.result))[['x']]
 s = sqrt( sum( es^2 ) / 8 )
 SE = s/sqrt(sum((x-mean(x))^2))
 t = (b1 - (0.02) )/SE
 ptval = pt(t,8,lower.tail=FALSE)
2* ptval
 