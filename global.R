library(shiny)
library(shinyBS)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinycssloaders)
# get confidence interval output
get_ci_output <- function(sampleStat, criticalValue, se){
  # test stat +/- cv*se
  withCvSe <- sprintf("\\(%.03f \\pm %0.3f \\cdot %0.3f=\\)",
                      sampleStat, criticalValue, se)
  # test stat +/- moe
  withMoe <- sprintf("\\(%0.3f \\pm %0.3f=\\)",
                     sampleStat, criticalValue*se)
  return (paste(withCvSe,withMoe))
}


get_ci <- function(sampleStat, criticalValue, se){
  return(round(c(sampleStat-criticalValue*se, sampleStat+criticalValue*se),3))
}

# get test stat output
get_test_stat_output <- function(sampleStat, nullValue, se){
  step2 <- sprintf("\\(=\\frac{%0.3f-%0.3f}{%0.3f}\\)",sampleStat, nullValue, se)
  return(step2)
}

get_test_stat <- function(sampleStat, nullValue, se){
  testStat = (sampleStat-nullValue)/se
  return(round(testStat,3))
}


# for single proportion
# standard error for single proportion
get_single_prop_se <- function(nullProp = 0.5, n = 100){
  se = sqrt(nullProp*(1-nullProp)/n)
  return(se)
}
# se output in latex
get_single_prop_se_output <- function(sampleProp, n){
  output <- sprintf("\\(\\sqrt{\\frac{\ %.03f (1- %.03f )}{ %1.0f }}\\)",
                    sampleProp,sampleProp,n)
  return (output)
}


# confidence interval for single proportion
get_single_prop_ci <- function(sampleProp, criticalValue, se){
  return(c(sampleProp-MoE, sampleProp+MoE))
}



# standard error for single mean
get_single_mean_se <- function(sd = 1, n = 100){
  se = sd/sqrt(n)
  return(se)
}


get_single_mean_se_output <- function(sd = 1, n = 100){
  output <- sprintf("\\(\\frac{%.03f}{\\sqrt{%1.0f}}\\)",
                    sd,n)
  return (output)
}

# for diff in proportions
# standard error for diff in proportions
get_prop_diff_se <- function(count1 = 10, n1 = 20, count2 = 10, n2 = 20){
  se = sqrt(count1*(n1-count1)/n1^3+count2*(n2-count2)/n2^3)
  return(se)
}
# se of diff in props output with latex
get_prop_diff_se_output <- function(count1, n1, count2, n2){
  output <- sprintf("\\(\\sqrt{\\frac{ %.03f (1- %.03f)}{%1.0f}+\\frac{%.03f (1- %.03f)}{%1.0f}}\\)",
                    count1/n1,count1/n1,n1,
                    count2/n2,count2/n2,n2)
  return (output)
}

# get pooled proportion
get_prop_diff_pool_prop <- function(count1, n1, count2, n2){
  
  return ((count1+count2)/(n1+n2))
}

# get se with pooled prop
get_pool_se <- function(p=0.5, n1, n2){
  se = sqrt(p*(1-p)*(1/n1+1/n2))
  return(se)
}


# standard error for diff in  mean
get_diff_mean_se <- function(sd1 = 1, n1 = 100, sd2 = 1, n2 = 100){
  se = sqrt(sd1^2/n1+sd2^2/n2)
  return(se)
}

# output of se for diff in mean
get_diff_mean_se_output <- function(sd1 = 1, n1 = 100, sd2 = 1, n2 = 100){
  output <- sprintf("\\(\\sqrt{\\frac{%0.1f^{2}}{%1.0f}+\\frac{%0.1f^{2}}{%1.0f}}\\)",
                    sd1,n1,sd2,n2)
  return (output)
}


























# 
# get_student_interval <- function(mean=0, std=1, n= 30, conf.level=0.95){
#   t = qt((1 - conf.level)/2, df = n-1,  lower.tail = FALSE)
#   se = std/sqrt(n)
#   moe = t*se
#   ll = round((mean - moe),4)
#   ul = round((mean + moe),4)
#   return(c(ll,ul))
# }




# 
# 
# 
# get_normal_interval <- function(mean=0, std=1, n= 30, conf.level=0.95){
#   z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
#   se = std/sqrt(n)
#   moe = z*se
#   ll = round((mean - moe),4)
#   ul = round((mean + moe),4)
#   return(c(ll,ul))
# }
# 
# 
# 
# 
# 
# get_normal_interval(mean = 0,std = 1,n = 10,conf.level = 0.95)
# get_student_interval(3.1,0.72,n = 50,conf.level = 0.9)





