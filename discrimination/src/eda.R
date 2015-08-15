library('ProjectTemplate')
library(matrixStats)
wealth.split = function(jobs)
{
  salary = (seq(40,10, by=-10))
  wealth = data.frame(blue = as.numeric(colSums(jobs) %*% salary), green = NA)
  wealth$green = sum(salary)*length(salary) - wealth$blue
  wealth = round(wealth/sum(wealth),2)
  wealth
}

movement = function(jobs, upward.discrimination, downward.discrimination)
{
  for(i in 2:ncol(jobs))
  {
    lucky.person = runif(1)
    lucky.person = ifelse(lucky.person < upward.discrimination[,1], 1, 0)
    pos = which(jobs[,i]==lucky.person)
    lucky.person = ifelse(length(pos) >= 1, sample(pos,1), runif(1,min = 1, max = 10))
    unlucky.person = runif(1)
    unlucky.person = ifelse(unlucky.person > downward.discrimination[,2], 0, 1)
    pos = which(jobs[,i-1]==unlucky.person)
    unlucky.person = ifelse(length(pos) >= 1, sample(pos,1), runif(1,min = 1, max = 10))
    #unlucky.person = runif(1,min = 1, max = 10)
    temp = jobs[unlucky.person,i-1]
    jobs[unlucky.person,i-1] = jobs[lucky.person,i]
    jobs[lucky.person,i] = temp
  }
  jobs
}
population = c(rep("blue", 50), rep("green", 50))
init.discrimination = data.frame(blue =0.9, green = 0.1)

jobs = matrix(0, nrow=10,ncol = 4)
num.blue = 20
num.green = 20-num.blue
for (i in 1:ncol(jobs))
{
  blue.get.job = min(init.discrimination$blue*nrow(jobs), num.blue)
  num.blue = num.blue-blue.get.job
  green.get.job = min(nrow(jobs)-blue.get.job, num.green)
  num.green = num.green-green.get.job
  temp = c(rep(1, blue.get.job), rep(0, green.get.job))
  if(length(temp)<nrow(jobs)) temp = c(temp, rep(1, nrow(jobs)-length(temp)))
  jobs[,i] =temp

}
image(t(jobs))
wealth.split(jobs)
promotion.discrimination = data.frame(blue = 0.9, green = 0.1)
demotion.discrimination = data.frame(blue =0.5, green = 0.5)
blue.wealth = NULL
for(i in 1:1000)
{
  jobs = movement(jobs, promotion.discrimination, demotion.discrimination)
  #blue.wealth = c(blue.wealth, wealth.split(jobs)$blue)
}
wealth.split(jobs)
image(t(jobs))




