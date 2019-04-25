
## 1. add up every elements in the matrix
a <- matrix(c(0,2,0,2,1,2,2,0,2), nrow=3, ncol=3, byrow=TRUE)
a
sum(a)

x = 0

for(i in 1:3){
  for(j in 1:3){
    x = x + a[i,j]
  }
}
x

## 2. Use a simple 'ifelse' statement to add a new column 'male.teen' to the data frame.
## This is a boolean column, indicating T if the observation is a male younger than 20 years.
student.df = data.frame( name = c("Sue", "Eva", "Henry", "Jan"),
                         sex = c("f", "f", "m", "m"), 
                         years = c(21,31,29,19)); student.df

student.df$male.teen = ifelse(student.df$sex == "m" & student.df$years < 20, "T", "F")
student.df

## 3. Write a double for loop which prints 30 numbers (1:10, 2:11, 3:12). 
## Those are three clusters of ten numbers each.
## The first loop determines the number of clusters (3) via its length; 
## the second loop the numbers to be printed (1 to 10 at the beginning). 
## Each cluster starts one number higher than the previous one.
## expected result:
## [1]  1
## [1]  2
## [1]  3
## [1]  4
## [1]  5
## [1]  6
## [1]  7
## [1]  8
## [1]  9
## [1] 10
## [1]  2
## [1]  3
## [1]  4
## [1]  5
## [1]  6
## [1]  7
## [1]  8
## [1]  9
## [1] 10
## [1] 11
## [1]  3
## [1]  4
## [1]  5
## [1]  6
## [1]  7
## [1]  8
## [1]  9
## [1] 10
## [1] 11
## [1] 12

for(j in 1:3){
  for(i in 1:10){
    print(i+j-1)
  }
}

for(j in 1:3){
  for(i in j:(j+9)){
    print(i)
  }
}


## 4. Write a repeat loop containing three random numbers. 
## The loop repeats itself exactly ten times before it stops.

## basic syntax:
repeat {
  command
  if(condition) break
}

x = rnorm(3)
x

cnt = 0
repeat{
  print(x)
  cnt = cnt + 1
  if(cnt >= 10) break
}


## 5. Write a for loop that prints the Displacement ('disp') of the 'mtcars' dataset.
## a. This loop will only print observations of 160 or higher in 'disp'.
## b. This loop will stop as soon as an observation is smaller than 160 in 'disp'.
#expected result

#a
# [1] 160
# [1] 160
# [1] 258
# [1] 360
# [1] 225
# [1] 360
# [1] 167.6
# [1] 167.6
# [...]

#b
# [1] 160
# [1] 160

####a
## method 1
x = mtcars$disp
for(i in 1: length(x)){
  if(x[i]>=160){print(x[i])}
}

## method 2
x = mtcars$disp
for(i in x){
  if(i>=160){print(i)}
}

## method 3
x = mtcars$disp
for(i in x){
  if(i < 160) next
  print(i)
}


#####b

## method 1
for(i in 1:length(x)){
  if(x[i]>=160){print(x[i])} 
  else {break}
}

## method 2
for(i in x){
  if(i>=160){print(i)}
  else {break}
}

## method 3
for(i in x){
  if(i<160) break
  print(i)
}



## 6. You have the data.frame 'mydf' with four columns like below.
a = c(3,7,NA, 9)
b = c(2,NA,9,3)
f = c(5,2,5,6)
d = c(NA,3,4,NA)
mydf = data.frame(a=a,b=b,f=f,d=d)
mydf
## You want to add another column '5':
## the 5th column contains the value of col 2 if col 1 is NA;
## the 5th column contains the value of col 4 if col 2 is NA;
## the 5th column contains the value of col 3 in all other cases.

mydf$e = ifelse(is.na(mydf$a) & !is.na(mydf$b), mydf$b, 
                ifelse(is.na(mydf$b) & !is.na(mydf$d), mydf$d, mydf$f))

mydf

## 7.Write a while loop starting with x = 0. 
## The loop prints all numbers up to 35 but it skips number 7.
## expected result:
# [1] 1
# [1] 2
# [1] 3
# [1] 4
# [1] 5
# [1] 6
# [1] 8
# [1] 9
# [1] 10
# [1] 11
# [...]

x=0
while(x <= 35){
  x = x + 1
  if(x == 7) next
  print(x)
}

## 8.We are using the same while loop as in the last exercise.
## The loop prints again all numbers up to 35, 
## but this time it skips a whole vector of numbers: 3,9,13,19,23,29.
## expected results:
# [1] 1
# [1] 2
# [1] 4
# [1] 5
# [1] 6
# [1] 7
# [1] 8
# [1] 10
# [1] 11
# [1] 12
# [1] 14
# [1] 15
# [1] 16
# [1] 17
# [1] 18
# [1] 20
# [1] 21
# [1] 22
# [1] 24
# [1] 25
# [1] 26
# [1] 27
# [1] 28
# [1] 30
# [1] 31
# [1] 32
# [1] 33
# [1] 34
# [1] 35

x=0
while(x <= 35){
  x = x + 1
  if(x %in% c(3,9,13,19,23,29)) next
  print(x)
}



## 9. You have an urn with balls from 1 to 100. 
## You want to find out how often you need to draw a ball to get number 55. 
## This is an experiment with replacement - you put the ball back each time you draw. 
## Simulate 1000 runs of the experiment to get an accurate estimation of the required draws.
## Use seed 23 to make the experiment reproducible. Use loops (for, while) for the solution.

set.seed(23)
x = c(1:100)
x


cnt = NULL
for(i in 1:1000){
  cnt[i] = 0
  num = 0
  while(num !=55){
    num = sample(x,1,replace = T)
    cnt[i] = cnt[i] + 1 
  }
}
cnt
sum(cnt)/1000

## 10. Get 1000 simulations of a paired dice game.
## A game immediately stops if you have an initial total (2 dice) of 5,6,7,8,9.
## If the first cast does not meet those 5 totals you would continue until you get either 11 or 12.
## What is the average number of dice casts per game?

set.seed(23)

for(i in 1:1000){
  start = sample(1:6,1) + sample(1:6, 1)
  x = start
  cnt[i]=1
  if(!x %in% c(5,6,7,8,9)){
    repeat{
    x = sample(1:6, 1) + sample(1:6,1)
    cnt[i] = cnt[i] + 1
    if(x %in% c(11,12)) break
    }
    }
}

cnt
sum(cnt)/1000

## 11. Use the 'rivers' dataset to write a for loop. The loop prints the dataset:
## rivers shorter than 500 are a 'short river';
## rivers longer than 2000 are a 'long river';
## and rivers in the middle range are printed in their original numbers.

data(rivers)
rivers

for(i in rivers){
  if(i<500) {print("short river")}
  else if(i > 2000){print("long river")}
  else {print(i)}
}



## 12. Using the following variables:
x=1
i=c(1:10)
## write a for() loop that increments x by two for each i.

for(j in i){
  x = x + 2
  print(x)
}




## 13. Using the following variables:
x=1
y=40
i=c(1:10)
## write a for() loop that increments x by three and decrease y by two, for each i.

for(j in i){
  x = x + 3
  y = y - 2
  l = c(x,y)
  print(l)
}



## 14. Using the following variables:a,b
##  write a nested for() loop (where the outer for loop increment a from 2 to 8 by 1
## and theinner for loop increment b from 1 to 6 by 1) that print 'a' less than 'b' if a < b

for(a in 2:8){
  for(b in 1:6){
    if(a<b){cat(a,"less than",b,'\n')}
  }
}


## 15. Using the following variables:
x=c(2,4)
# type a while () loop that adds even numbers to x,
# while the length of x is less than 12.
# For example, in the first iteration you get x = 2,4,6, and the third x =2,4,6,8.

i = 6
while(length(x) < 12){
  x = c(x, i)
  i = i + 2
}
x

## 16. Using the following variable:
a=15:10
b=20:15
# type a while () loop that computes a vector x=225 224 221 216 209 200
# ,such that
# x[1]=a[1]*b[6]
# x[2]=a[2]*b[5]
# x[3]=a[3]*b[4]
#  .
#  .
# x[6]=a[6]*b[1]

i = 1
x = NULL
while(i < 7){
  x[i] = a[i] * b[7-i]
  i = i + 1
}
x
