##### I currently plan to create a bank of 5000 random questions per test,      #####
##### which requires 25 distinct generators per test as they appear below. So,  #####
##### 75 total genrators for a semester of three tests. Once finished with      #####
##### individual generators, I will begin function wrapping, creating either a  #####
##### single function with many adaptive arguments, or a suite of functions.    #####

##### These will help streamline test writing, requiring only that one runs the #####
##### code to replace the test banks at least once per year. The longest part   #####
##### of the process is waiting on the e-learning uploads of the CSVs so-created. ###                                                               #####

##### 6/25 Generators for test 1 completed  #####
##### 0/25 Generators for test 2 completed  #####
##### 0/25 Generators for test 3 completed  #####

#### 0/75 Generators wrapped into functions #####

##### n is the number of questions desired, answers is the number of answers    #####
##### per question, points.per.q are the points assigned to each question,      #####
##### difficulty is the difficulty assigned to each question, and type is the   #####
##### question's category (all as listed on e-learning. I've labeled the        #####
##### questions by concept, e-learning type, and creation of the concept/type   #####
##### pair. For example, MeanMC1 is a generator for my first very simple        #####
##### multiple choice question about a mean.                                    #####

##### MeanMC1 #####
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt <- "What is the mean of the following dataset?"
dat.size = 5
questions <- data.frame()
for(i in 1:n)
{
points <- sample(c(rep(0,answers-1),100),replace=F)
corr.ind <- 6 + which.max(points)
data <- rnorm(dat.size,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size,df=30))
corr.ans <- mean(data)
ans.text <- rnorm(answers,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.75*rt(answers,df=30))
hint <- "Watch out for negatives!"
feedback <- "Did you sum the numbers (subtracting any negatives) and divide by the sample size?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
content <- c(type, "MeanMC1", "Find the Mean", paste(quest.txt,
                                                     paste(as.character(round(data,digits=3)),
                                                           collapse=", ",sep=""),
                                                     collapse = ""),
             points.per.q, difficulty, points, hint, feedback)
options <- c(rep("",6), round(ans.text, digits = 3), rep("",2))
options[corr.ind] <- round(corr.ans,digits = 3)
questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file="QBank.csv", row.names=F, col.names=F)

##### MedianMC1 #####
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt <- "What is the median of the following dataset?"
questions <- data.frame()
for(i in 1:n)
{
  dat.size = sample(c(5,6), size = 1)
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data <- rnorm(dat.size,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size,df=30))
  corr.ans <- median(data)
  ans.text <- sample(c(median(data)-15.2, sd(data),
                       rnorm(answers-2,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(answers-2,df=30))),
                     replace = F)
  hint <- "Sort the data first."
  feedback <- "Sort and find the middle number, or take the average of the two middle numbers."
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
             rep("Option", answers),"Hint","Feedback")
  content <- c(type, "MedianMC1", "Find the Median", paste(quest.txt,
                                                           paste(as.character(round(data,digits=3)),
                                                                 collapse=", ",sep=""),
                                                           collapse = ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), round(ans.text, digits = 3), rep("",2))
  options[corr.ind] <- round(corr.ans,digits = 3)
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file="QBank.csv", row.names=F, col.names=F)

##### SDMC1 #####
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt <- "What is the SD of the following dataset?"
dat.size = 5
questions <- data.frame()
for(i in 1:n)
{
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data <- rnorm(dat.size,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size,df=30))
  corr.ans <- sd(data)
  ans.text <- sample(c(sd(data)^2, sqrt(sd(data)^2*(dat.size-1)),
                       rnorm(answers-2,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(answers-2,df=30))),
                     replace = F)
  hint <- "Don't forget to take a square root at the end."
  feedback <- "1: Mean. 2: Squared differences. 3: Sum. 4: Divide. 5. Square Root. "
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
             rep("Option", answers),"Hint","Feedback")
  content <- c(type, "SDMC1", "Find the SD", paste(quest.txt,
                                                       paste(as.character(round(data,digits=3)),
                                                             collapse=", ",sep=""),
                                                       collapse = ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), round(ans.text, digits = 3), rep("",2))
  options[corr.ind] <- round(corr.ans,digits = 3)
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file="QBank.csv", row.names=F, col.names=F)

##### NormDistMC1 #####
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Given that some normally distributed data has a mean of "
quest.txt2 <- " and a standard deviation of "
quest.txt3 <- ". What is the probability that a randomly sampled datapoint will be less than "
quest.txt4 <- "?"
dat.size = 1
questions <- data.frame()
for(i in 1:n)
{
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- rnorm(dat.size,mean=rnorm(1,mean=900,sd=300),sd=100) + (0.25*rt(dat.size,df=30))
  data2 <- runif(dat.size, 65, 150)
  data3 <- sample(c(runif(dat.size, data1 - 180, data1 - 5),
                  runif(dat.size, data1 + 5, data1 + 180)), size = 1)
  corr.ans <- pnorm(data3, data1, data2)
  up.min <- corr.ans + .05
  down.max <- corr.ans - .05
  ans.text <- sample(c(runif(2, ifelse(up.min < 1, up.min, 0), ifelse(up.min < 1, 1.05, down.max)),
                       runif(2, ifelse(down.max > 0, -0.05, up.min), ifelse(down.max > 0, down.max, 1))),
                     replace = F)
  hint <- "You'll need your Z-table for this question."
  feedback <- "1: Calculate Z. 2: Find area below on the Z-table."
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
             rep("Option", answers),"Hint","Feedback")
  content <- c(type, "NormDistMC1", "Normal Probability", paste(quest.txt1, round(data1, digits=3), quest.txt2,
                                                                round(data2, digits=3), quest.txt3, 
                                                                round(data3, digits = 3), quest.txt4,
                                                                collapse = "", sep= ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), round(ans.text, digits = 3), rep("",2))
  options[corr.ind] <- round(corr.ans,digits = 3)
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file="QBank.csv", row.names=F, col.names=F)

##### NormDistMC2 #####
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Given that some normally distributed data has a mean of "
quest.txt2 <- " and a standard deviation of "
quest.txt3 <- ". What is the probability that a randomly sampled datapoint will be greater than "
quest.txt4 <- "?"
dat.size = 1
questions <- data.frame()
for(i in 1:n)
{
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- rnorm(dat.size,mean=rnorm(1,mean=900,sd=300),sd=100) + (0.25*rt(dat.size,df=30))
  data2 <- runif(dat.size, 65, 150)
  data3 <- sample(c(runif(dat.size, data1 - 180, data1 - 5),
                    runif(dat.size, data1 + 5, data1 + 180)), size = 1)
  corr.ans <- 1 - pnorm(data3, data1, data2)
  up.min <- corr.ans + .05
  down.max <- corr.ans - .05
  ans.text <- sample(c(runif(2, ifelse(up.min < 1, up.min, 0), ifelse(up.min < 1, 1.05, down.max)),
                       runif(2, ifelse(down.max > 0, -0.05, up.min), ifelse(down.max > 0, down.max, 1))),
                     replace = F)
  hint <- "You'll need your Z-table for this question."
  feedback <- "1: Calculate Z. 2: Find area below on the Z-table. 3: Take 1 - area below."
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
             rep("Option", answers),"Hint","Feedback")
  content <- c(type, "NormDistMC2", "Normal Probability", paste(quest.txt1, round(data1, digits=3), quest.txt2,
                                                                round(data2, digits=3), quest.txt3, 
                                                                round(data3, digits = 3), quest.txt4,
                                                                collapse = "", sep= ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), round(ans.text, digits = 3), rep("",2))
  options[corr.ind] <- round(corr.ans,digits = 3)
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file="QBank.csv", row.names=F, col.names=F)

##### RobustMC1 #####
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A researcher wants to estimate the center of her data's distribution with a statistic that is "
quest.txt2 <- " Which statistic should she select for this purpose?"
dat.size = 
  questions <- data.frame()
for(i in 1:n)
{
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data <- sample(c("the least robust.", "the most robust."), size = 1)
  corr.ans <- ifelse(data == "the least robust.", "The mean", "The median")
  ans.text <- sample(c(ifelse(data == "the least robust", "The median", "The mean"), "The 10% trimmed mean",
                       "The first quartile", "The third quartile", "The full range (max - min)", "The middle 50% range (Q3 - Q1)",
                       "The standard deviation", "The maximum", "The minimum", "A relative frequency"),
                     size = answers)
  hint <- "Remember that 'robust' means insensitive to outliers."
  feedback <- "The mean is least robust, and the median is most robust."
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
             rep("Option", answers),"Hint","Feedback")
  content <- c(type, "RobustMC1", "Robust Concept", paste(quest.txt1, data, quest.txt2,
                                                          collapse = "", sep = ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), ans.text, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[((9+answers)):((8+answers)*(n+1)),]
write.table(questions, sep=",", file="QBank.csv", row.names=F, col.names=F)