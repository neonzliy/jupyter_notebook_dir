# HW3 Big Mart

### load data
train = read.csv("C:/Users/leonz/Desktop/BA Class/Homeworks/HW3/BigMart_Sales_Prediction/TrainData.csv", header = T)
test = read.csv ("C:/Users/leonz/Desktop/BA Class/Homeworks/HW3/BigMart_Sales_Prediction/TestData.csv", header = T)

head(train)
str(train)
dim(train)
names(train)

### check for missing values
table(is.na(train))
colSums(is.na(train)) # only numeric variable missing
summary(train) # check min and max distance to mean, check variable names;

### Data Visualization
library(ggplot2)


### numeric values

# Item_Weight vs. Item_Outlet_Sales
ggplot(train, aes(x = Item_Weight, y = Item_Outlet_Sales)) + 
  geom_point(size = 1, color = "navy")

# this graph shows Item_Weight does not have any specific affect on Sales, there is no obvious
# pattern suggesting different Item_Weight contributes to Sales differently.

# Item_Visibility vs. Item_Outlet_Sales ***
ggplot(train, aes(x = Item_Visibility, y = Item_Outlet_Sales)) + 
  geom_point(size = 1, color = "navy")

# There is an obvious seperating point around Visibility of 0.2. Visibility above 0.2
# suffers from low reported sales. In order to keep sales in a relatively normal range,
# it is best to keep the visibility below 0.2.

# Item_MRP vs. Item_Outlet_Sales ***
ggplot(train, aes(x = Item_MRP, y = Item_Outlet_Sales)) + 
  geom_point(size = 1, color = "navy")

# Items with higher MRP the possibility of generating higher sales is bigger.
# However, it is not safe to say higher MRP will result in a higher sales since
# MRP and Sales does not have a positive linear relationship.


### factors

install.packages(plyr)
library(plyr)
levels(train$Item_Fat_Content)

# rename values
train$Item_Fat_Content = revalue(train$Item_Fat_Content, 
                                 c("LF" = "Low Fat", "low fat" = "Low Fat",
                                   "reg" = "Regular"))
levels(train$Item_Fat_Content)

# Item_Fat_Content vs. Item_Outlet_Sales
ggplot(train, aes(x = Item_Outlet_Sales)) + 
  geom_histogram(aes(fill = Item_Fat_Content),bins = 50, color = "grey")+
  facet_grid(Item_Fat_Content~., scales = "free")

# The pattern for low fat food is identical to the pattern of regular food,
# although low fat food is more popular in terms of count of sales, we would
# suggest to adjust to inventory to match the porportion of low fat and regular.

# Item_Type vs. Item_Outlet_Sales
ggplot(train, aes(x = Item_Outlet_Sales)) + 
  geom_histogram(aes(fill = Item_Type),bins = 50, color = "grey")+
  facet_grid(Item_Type~., scales = "free")

# regardless of the item type, sales above 5000 is relatively small.
# breakfast and snack foods share a similar pattern where count of sales
# is averaged out when sales increases. Sales of Hard drinks is relatively
# volitail. The rest item types all follow "higher sales lower count of sales" tend.

# Outlet_Establishment_Year vs. Item_Outlet_Sales
ggplot(train, aes(x = Item_Outlet_Sales)) + 
  geom_histogram(aes(fill = Outlet_Establishment_Year),bins = 50, color = "grey")+
  facet_grid(Outlet_Establishment_Year~., scales = "free")

library(dplyr)
distinct_year = train %>% distinct(Outlet_Establishment_Year)
distinct_year = factor(distinct_year)
###?????????1998??????data??????????????????,?????????????????????T_T
# Expect 1995 and 1998, count of sales deceases while sales increase.
# However, for 1995 and 1998. soon after sales reach roughly 1000,
# count of sales decreases significantly. In 1998 we believe there might 
# not be data of sales. It is important to know what happened in these two
# years.

levels(train$Outlet_Size)
levels(train$Outlet_Size)[1] = "Not specified"
levels(train$Outlet_Size)



# Outlet_Size vs. Item_Outlet_Sales
ggplot(train, aes(x = Item_Outlet_Sales)) + 
  geom_histogram(aes(fill = Outlet_Size),bins = 50, color = "grey")+
  facet_grid(Outlet_Size~., scales = "free")

# Big size and mid size outlets then to have a more consistant sales count while sales
# increases. On the other hand, small and not specified size of outlets count of sales
# decreases relatively fast comparing to big and mid size outlets. It is better to put
# low MRP items in small and not specified outlets and higher MRP items in big and mid
# size outlets to optimize sales.

# Outlet_Location_Type vs. Item_Outlet_Sales
ggplot(train, aes(x = Item_Outlet_Sales)) + 
  geom_histogram(aes(fill = Outlet_Location_Type),bins = 50, color = "grey")+
  facet_grid(Outlet_Location_Type~., scales = "free")

# there is no evidence showing different location type contributes to different sales.

# Outlet_Type vs. Item_Outlet_Sales
ggplot(train, aes(x = Item_Outlet_Sales)) + 
  geom_histogram(aes(fill = Outlet_Type),bins = 50, color = "grey")+
  facet_grid(Outlet_Type~., scales = "free")

# similar to outlet size, we see different patterns shown for different outlet types.
# It is most likely caused by different outlet type has different inventories. i.e.
# grocery stores tend not to high very high MRP items, therefore it is hard for grocery
# stores to generate high sales.(refering to MRP analysis) Similarly, supermarket type3
# tend to have more diversified items and more high MRP items, therefore, the sales generated
# is high.

### Data Preprocessing ###
# add Item_Outlet_Sales to test

test$Item_Outlet_Sales = NA

# combine train and test
data = rbind(train, test)

# impute missing values in Item Weight
data$Item_Weight[is.na(data$Item_Weight)] = mean(data$Item_Weight, na.rm = T)
table(is.na(data$Item_Weight))

# impute 0 values in Item visibility
data$Item_Visibility = ifelse(data$Item_Visibility == 0,
                              mean(data$Item_Visibility, na.rm = T),
                              data$Item_Visibility)
summary(data$Item_Visibility)

# Item_Fat_Content mis-matched level issue
levels(data$Item_Fat_Content)
data$Item_Fat_Content = revalue(data$Item_Fat_Content, 
                                 c("LF" = "Low Fat", "low fat" = "Low Fat",
                                   "reg" = "Regular"))
levels(data$Item_Fat_Content)

# Give a level to Outlet_Size missing value
levels(data$Outlet_Size)
levels(data$Outlet_Size)[5] = "Not specified"
levels(data$Outlet_Size)

# Year
data$Year = 2017 - data$Outlet_Establishment_Year
str(data)

data$Outlet_Establishment_Year = factor(data$Outlet_Establishment_Year)
str(data)


regdata = select(data, -contains("Identifier"), -Outlet_Establishment_Year)
names(regdata)

train1 = regdata[1:nrow(train),]
test1 = regdata[-(1:nrow(train)),]
str(train1)
str(test1)

# fit linear regression model
model1 = lm(Item_Outlet_Sales ~ ., data = train1)

# report fitted linear regression model
summary(model1)

# default residuals plots. . . diagnostic graphics 
par(mfrow = c(2,2))
plot(model1)


# deal with heteroskedasticity
model2 = lm(log(Item_Outlet_Sales) ~ ., data = train1)
plot(model2)

# predict sales at test data set
?predict
names(test1)
head(test1)
regtest = select(test1, -contains("Sales"))
names(regtest)

pre_sales = predict(model2, regtest)
test$predicted_sales = pre_sales
names(test)
head(test)


# Output
result = test %>% select(Item_Identifier, Outlet_Identifier, predicted_sales)
write.csv(result, file ="C:/Users/leonz/Desktop/BA Class/Homeworks/HW3/HW3_BigMart.csv" )
