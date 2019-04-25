#####################################################################
######################### Data Exploration ##########################
#####################################################################

## Read data
data = read.csv('C:/Users/leonz/Desktop/BA Class/Capstone_Project/users.csv', header = T)
str(data)

### Missing Value
table(is.na(data))
colSums(is.na(data))
summary(data)


#####################################################################
####################### Data Pre-processing #########################
#####################################################################

### Country_Destination
# the distribution of country_destination
summary(data$country_destination)
prop.table(table(data$country_destination))
require(ggplot2)
ggplot(data, aes(data$country_destination)) +
  geom_bar(fill = "light blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = 'gray'),
        axis.text = element_text(size = 10)) +
  xlab("Country Destination")

# Create "Outside US"
data$ctr = factor(ifelse(data$country_destination == "US", "US",
                  ifelse(data$country_destination == "NDF", "NDF", "Outside_US")))
unique(data$ctr)
table(data$ctr)
prop.table(table(data$ctr))
ggplot(data, aes(data$ctr)) +
  geom_bar(fill = "pink") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10)) +
  xlab("Country Destination")
  

### Pre-process gender
prop.table(table(data$gender))
summary(data$gender)

### Pre-process age
summary(data$age)
# we see that age ranges from 1 to 2014 which is clearly not correct
data$age = ifelse((data$age >= 1915) & (data$age <= 2014), 2017 - data$age, data$age)
data$age = ifelse((data$age >= 16) & (data$age <= 100), data$age, NA)
summary(data$age)

# break out date_account_created by year, month and day
library(stringr)
dac = as.data.frame(str_split_fixed(data$date_account_created, '/',3))
head(dac)
data['dac_year'] = dac[,3]
data['dac_month'] = dac[,1]
data['dac_day'] = dac[,2]
head(data)

# break out date_first_booking by year, month and day
dfb = as.data.frame(str_split_fixed(data$date_first_booking, '/',3))
dfb
data['dfb_year'] = dfb[,3]
data['dfb_month'] = dfb[,1]
data['dfb_day'] = dfb[,2]
head(data)


# timestamp_first_active
data['tfa_year'] = substring(as.character(data['timestamp_first_active']), 3, 6)
data['tfa_month'] = substring(as.character(data['timestamp_first_active']), 7, 8)
data['tfa_day'] = substring(as.character(data['timestamp_first_active']), 9, 10)
head(data)



#####################################################################
######################## Data Visualization #########################
#####################################################################


### Gender distribution
ggplot(data, aes(data$gender)) +
  geom_bar(fill = "pink") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Gender") +
  ylab("Num of Records") 


### gender vs. destination country
## count
ggplot(data, aes(gender, fill = ctr)) +
  geom_bar(position = "dodge") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Gender")

## percentage
library(dplyr)
df = as.data.frame(data %>%
                     count(gender, ctr) %>%
                     ungroup %>%
                     group_by(gender) %>%
                     mutate(total = sum(n), pct = n/total))
df

ggplot(df, aes(x = gender, y = pct, fill= ctr)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Gender") +
  ylab("Percent of Total [%]") +
  labs(fill="Destination") +
  scale_fill_brewer(palette = "PuRd") + 
  theme_minimal()
  

### Distribution of Age
summary(data$age)
boxplot(data$age)

## Age bucket
data$age_bkt = factor(ifelse((data$age >= 16) & (data$age < 26), "16-25",
                      ifelse((data$age >= 26) & (data$age < 36), "26-35",
                      ifelse((data$age >= 36) & (data$age < 46), "36-45",
                      ifelse((data$age >= 46) & (data$age < 56), "46-55",
                      ifelse((data$age >= 56) & (data$age < 66), "56-65",
                      ifelse((data$age >= 66) & (data$age < 76), "66-75",
                      ifelse(data$age >= 76, "76+", "other"
                             ))))))))

data$age_bkt2 = as.character(cut(data$age, breaks = c(16,25,35,45,55,65,75,100),
                                labels = c('16-25','26-35', "36-45", "46-55", "56-65", 
                                           "66-75", "76+")))

## distribution of age bucket
summary(data$age_bkt)
ggplot(data, aes(data$age_bkt)) +
  geom_bar(fill = "pink") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Age Bucket")

## distribution of age bucket histogram

summary(data$age_bkt)
ggplot(data, aes(data$age)) +
  geom_histogram(stat = "count", bins = 500, fill = "pink") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Age Bucket") +
  scale_x_continuous(limits = c(16,90), breaks = seq(0, 100, 3)) +
  scale_fill_brewer(palette = "Paired") + 
  theme_minimal() 


## age bucket vs. ctr
# count
ggplot(data, aes(data$age_bkt, fill = data$ctr)) +
  geom_bar(position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Age Bucket") +
  scale_fill_brewer(palette = "PuRd") + 
  theme_minimal()

# percentage
df2 = as.data.frame(data %>%
                      count(age_bkt, ctr) %>%
                      ungroup %>%
                      group_by(age_bkt) %>%
                      mutate(total = sum(n), pct = n/sum(n)))
head(df2)

ggplot(df2, aes(x = age_bkt, y = pct, fill = ctr)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Age Bucket") +
  ylab("Percent of Total [%]") +
  labs(fill="Destination") +
  scale_fill_brewer(palette = "PuRd") + 
  theme_minimal()


## dac_year vs. ctr
df3 = as.data.frame(data %>%
                      count(dac_year, ctr) %>%
                      ungroup %>%
                      group_by(dac_year) %>%
                      mutate(total = sum(n), pct = n/sum(n)))


ggplot(df3, aes(x = dac_year, y = pct, fill = ctr)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Account Created Year") +
  ylab("Percent of Total [%]") +
  labs(fill="Destination") +
  scale_fill_brewer(palette = "PuRd") + 
  theme_minimal()

## dac_month vs. ctr
df3.1 = as.data.frame(data %>%
                      count(dac_year,dac_month, ctr) %>%
                      ungroup %>%
                      group_by(dac_year,dac_month) %>%
                      mutate(total = sum(n), pct = n/sum(n)))


ggplot(df3.1, aes(x = dac_month, y = pct, fill = ctr)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Account Created Year") +
  ylab("Percent of Total [%]") +
  labs(fill="Destination") +
  scale_fill_brewer(palette = "PuRd") + 
  theme_minimal()


## date_first_booking vs. ctr
df10 = as.data.frame(data %>%
                      count(dfb_year, ctr) %>%
                      ungroup %>%
                      group_by(dfb_year) %>%
                      mutate(total = sum(n), pct = n/sum(n)))


ggplot(df10, aes(x = dfb_year, y = pct, fill = ctr)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("First Booking Year") +
  ylab("Percent of Total [%]") +
  labs(fill="Destination") +
  scale_fill_brewer(palette = "PuRd") + 
  theme_minimal() 


## remove "NDF"
df10.1 = as.data.frame(data[data$ctr != "NDF",] %>%
                      count(dfb_year, ctr) %>%
                      ungroup %>%
                      group_by(ctr) %>%
                      mutate(total = sum(n), pct = n/sum(n)))

ggplot(df10.1, aes(x = dfb_year, y = pct, fill = ctr)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("First Booking Year") +
  ylab("Percent of Total [%]") +
  labs(fill="Destination") +
  scale_fill_brewer(palette = "PuRd") + 
  theme_minimal() 


## signup_app vs. ctr
df4 = as.data.frame(data %>%
                      count(signup_app, ctr) %>%
                      ungroup %>%
                      group_by(signup_app) %>%
                      mutate(total = sum(n), pct = n/sum(n)))


ggplot(df4, aes(x = signup_app, y = pct, fill = ctr)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Account Created Year") +
  ylab("Percent of Total [%]") +
  labs(fill="Destination") +
  scale_fill_brewer(palette = "PuRd") + 
  theme_minimal()


## signup_flow vs. ctr

is.numeric(data$signup_flow)
data$signup_flow = as.factor(data$signup_flow)
levels(data$signup_flow)

df11 = as.data.frame(data %>%
                      count(signup_flow, ctr) %>%
                      ungroup %>%
                      group_by(signup_flow) %>%
                      mutate(total = sum(n), pct = n/sum(n)))


ggplot(df11, aes(x = signup_flow, y = pct, fill = ctr)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Page Led to Signup") +
  ylab("Percent of Total [%]") +
  labs(fill="Destination") +
  scale_fill_brewer(palette = "PuRd") + 
  theme_minimal()

df11.1 = as.data.frame(data %>%
                       count(signup_flow, dfb_year) %>%
                       ungroup %>%
                       group_by(signup_flow) %>%
                       mutate(total = sum(n), pct = n/sum(n)))

ggplot(df11.1, aes(x = dfb_year, y = pct, fill = signup_flow)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("First Booking Date") +
  ylab("Percent of Total [%]") +
  labs(fill="Page Led to Signup") 

# remove blank record
df11.12 = as.data.frame(data[data$dfb_year != "",] %>%
                         count(signup_flow, dfb_year) %>%
                         ungroup %>%
                         group_by(signup_flow) %>%
                         mutate(total = sum(n), pct = n/sum(n)))

# x = dfb_year, y = pct, fill = signup_flow
ggplot(df11.12, aes(x = dfb_year, y = pct, fill = signup_flow)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("First Booking Date") +
  ylab("Percent of Total [%]") +
  labs(fill="Page Led to Signup") 


# x = signup_flow, y = pct, fill = dfb_year
ggplot(df11.12, aes(x = signup_flow, y = pct, fill = dfb_year)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Page Led to Signup") +
  ylab("Percent of Total [%]") +
  labs(fill="First Booking Date") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.1)) +
  scale_fill_brewer(palette = "Paired") + 
  theme_minimal() 

# the distribution of language
ggplot(data, aes(data$language)) +
  geom_bar(fill = "light blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = 'gray'),
        axis.text = element_text(size = 10)) +
  xlab("Language Peference")

## language vs. ctr
is.factor(data$language)
levels(data$language)

df12 = as.data.frame(data %>%
                       count(language, ctr) %>%
                       ungroup %>%
                       group_by(language) %>%
                       mutate(total = sum(n), pct = n/sum(n)))


ggplot(df12, aes(x = language, y = pct, fill = ctr)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Language Peference") +
  ylab("Percent of Total [%]") +
  labs(fill="Destination") +
  scale_fill_brewer(palette = "Blues") + 
  theme_minimal()


# Install
install.packages("wesanderson")
# Load
library(wesanderson)

## first_device_type vs. ctr
is.factor(data$first_device_type)
levels(data$first_device_type)

df13 = as.data.frame(data %>%
                       count(first_device_type, ctr) %>%
                       ungroup %>%
                       group_by(first_device_type) %>%
                       mutate(total = sum(n), pct = n/sum(n)))


ggplot(df13, aes(x = first_device_type, y = pct, fill = ctr)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(angle = 50, hjust = 1,size = 10, color = '#474747')) +
  xlab("Page Led to Signup") +
  ylab("Percent of Total [%]") +
  labs(fill="Destination") +
  scale_fill_manual(values=wes_palette(n=3, name="Moonrise3"))
  




## first_browser vs. age_bkt
data$age_bkt
prop.table(table(data$first_browser))

df18 = as.data.frame(data[data$first_browser != 'NA',] %>%
                       count(first_browser, age_bkt) %>%
                       ungroup %>%
                       group_by(age_bkt) %>%
                       mutate(total = sum(n), pct = n/sum(n)))


ggplot(df18, aes(x = age_bkt, y = pct, fill = first_browser)) +
  geom_bar(stat='identity', position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(angle = 50, hjust = 1,size = 10, color = '#474747')) +
  xlab("Age Buckets") +
  ylab("Percent of Total [%]") +
  labs(fill="First Browser Type") 



#####################################################################
######################### Conversion Rate ###########################
#####################################################################
data$booked = ifelse(data$ctr == "NDF", 0, 1)

## dac_year vs. conversion rate
df5 = as.data.frame(data %>%
                      group_by(dac_year) %>%
                      summarise(n = n(),
                                booked = sum(booked),
                                conversion_rate = booked/n))
ggplot(df5, aes(dac_year)) +
  geom_line(aes(y = n), group = 1, size = 2, color = "pink") +
  geom_line(aes(y = conversion_rate*130000), group = 1, size = 2, color = "light blue") +
  scale_y_continuous(breaks = seq(0, 80000, 10000), "Number of Records", 
                     sec.axis = sec_axis(~./130000, "Conversion Rate")) +
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Account Created Year") 
  



## Gender vs. Conversion rate
df6 = as.data.frame(data %>%
                      group_by(gender) %>%
                      summarise(n = n(), booked = sum(booked), 
                                conversion_rate = booked/n))
ggplot(df6, aes(gender, conversion_rate)) +
  geom_bar(stat = "identity", fill = "pink") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Gender")   



## Age Bucket vs. Conversion Rate
df7 = as.data.frame(data %>%
                      group_by(age_bkt) %>%
                      summarise(n = n(), booked = sum(booked), 
                                conversion_rate = booked/n))




ggplot(df7, aes(age_bkt, conversion_rate)) +
  geom_bar(aes(y = conversion_rate),stat = "identity", fill = "pink") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 10, color = '#474747')) +
  xlab("Age Bucket") +
  scale_y_continuous(breaks = seq(0, 0.6, 0.1)) +
  geom_line(aes(y = conversion_rate),group = 1, size = 1.5, color = "light blue") +
  geom_point(size = 3, color = "orange")



## Afflicate_Provider vs. Conversion Rate
# percentage
df8 = as.data.frame(data %>%
                      group_by(affiliate_provider) %>%
                      summarise(n = n(), booked = sum(booked), 
                                conversion_rate = booked/n) %>%
                      mutate(total = sum(n), pct = n/total * 1000))


ggplot(df8, aes(x = affiliate_provider, y = conversion_rate)) +
  geom_bar(aes(y = conversion_rate * 100000),stat = "identity", fill = "pink", alpha = 0.95) +
  geom_bar(aes(y = n),stat = "identity", fill = "pink", alpha = 0.0) +
  geom_line(aes(y = n),group = 1, size = 1.5, color = "light blue", alpha = 0.95) +
  geom_line(aes(y = conversion_rate * 100000),group = 1, size = 1.5, color = "pink", alpha = 0.0) +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text = element_text(size = 11, angle = 50, hjust = 1,color = '#474747')) +
  xlab("Affiliate_Provider") +
  ylab("User Number") +
  scale_y_continuous(sec.axis = sec_axis(~./100000, breaks = seq(0,1,0.5))) 


# remove "direct"
df9 = as.data.frame(data[data$affiliate_provider != "direct",] %>%
                      group_by(affiliate_provider) %>%
                      summarise(n = n(), booked = sum(booked), conversion_rate = booked/n))

ggplot(df9, aes(x = affiliate_provider, y = conversion_rate)) +
  geom_bar(aes(y = conversion_rate * 50000),stat = "identity", fill = "pink", alpha = 0.0) +
  geom_bar(aes(y = n),stat = "identity", fill = "pink", alpha = 0.0) +
  geom_line(aes(y = n),group = 1, size = 1.5, color = "light blue", alpha = 0.95) +
  geom_line(aes(y = conversion_rate * 50000),group = 1, size = 1.5, color = "pink", alpha = 0.95) +
  theme_bw() +
  theme(axis.title = element_text(size = 11, color = '#474747'),
        axis.text.x = element_text(size = 11, angle = 50, hjust = 1,color = '#474747')) +
  xlab("Affiliate_Provider") +
  ylab("User Number") +
  scale_y_continuous(sec.axis = sec_axis(~./50000, breaks = seq(0,1,0.2), name = "Conversion Rate [%]"))



## first_browser vs. conversion rate

df14 = as.data.frame(data %>%
                       group_by(first_browser) %>%
                       summarise(n = n(), booked = sum(booked), 
                                 conversion_rate = booked/n) %>%
                       mutate(total = sum(n), pct = n/total * 1000))


ggplot(df14, aes(x= first_browser, y = conversion_rate)) +
  geom_bar(aes(y = conversion_rate),stat = "identity", fill = "pink", alpha = 0) +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text.x = element_text(angle=50, hjust = 1, size = 10, color = '#474747')) +
  xlab("First Browser") +
  ylab("Conversion Rate [%]") +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  geom_line(aes(y = conversion_rate),group = 1, size = 1.5, color = "light blue") 



# Remove Zeros

df14.1 = as.data.frame(df14[df14$conversion_rate != 0.00000000,])      

ggplot(df14.1, aes(x= first_browser, y = conversion_rate)) +
  geom_bar(aes(y = conversion_rate),stat = "identity", fill = "pink", alpha = 1) +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text.x = element_text(angle=50, hjust = 1, size = 10, color = '#474747')) +
  xlab("Age Bucket") +
  ylab("Conversion Rate [%]") +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  geom_line(aes(y = conversion_rate),group = 1, size = 1.5, color = "light blue") +
  geom_point(size = 3, color = "orange")


# Only Zeros

df14.2 = as.data.frame(df14[df14$conversion_rate == 0.00000000,])      
df14.2


ggplot(df14.2, aes(x= first_browser, y = conversion_rate)) +
  geom_bar(aes(y = conversion_rate),stat = "identity", fill = "pink", alpha = 0) +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text.x = element_text(angle=50, hjust = 1, size = 10, color = '#474747')) +
  xlab("Age Bucket") +
  scale_y_continuous(breaks = seq(0, 0, 0.1)) +
  geom_line(aes(y = conversion_rate),group = 1, size = 1.5, color = "light blue") +
  geom_point()




## first_device_type vs. conversion rate

df15 = as.data.frame(data %>%
                       group_by(first_device_type) %>%
                       summarise(n = n(), booked = sum(booked),
                                 conversion_rate = booked/n) %>%
                       mutate(total = sum(n), pct = n/total * 1000))


ggplot(df15, aes(first_device_type, conversion_rate)) +
  geom_bar(aes(y = conversion_rate),stat = "identity", fill = "pink") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text.x = element_text(angle = 50, hjust = 1, size = 10, color = '#474747')) +
  xlab("First Device Type") +
  ylab("Conversion Rate [%]") +
  scale_y_continuous(breaks = seq(0, 1, 0.05)) +
  geom_line(aes(y = conversion_rate),group = 1, size = 1.5, color = "light blue") +
  geom_point(size = 3, color = "orange")


## signup_flow vs. conversion rate

df16 = as.data.frame(data %>%
                       group_by(signup_flow) %>%
                       summarise(n = n(), booked = sum(booked),
                                 conversion_rate = booked/n) %>%
                       mutate(total = sum(n), pct = n/total * 1000))


ggplot(df16, aes(signup_flow, conversion_rate)) +
  geom_bar(aes(y = conversion_rate),stat = "identity", fill = "pink") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text.x = element_text(size = 10, color = '#474747')) +
  xlab("Signup Flow") +
  ylab("Conversion Rate [%]") +
  scale_y_continuous(breaks = seq(0, 1, 0.05)) +
  geom_line(aes(y = conversion_rate),group = 1, size = 1.5, color = "light blue") +
  geom_point(size = 3, color = "orange")


## signup_app vs. conversion rate

df17 = as.data.frame(data %>%
                       group_by(signup_app) %>%
                       summarise(n = n(), booked = sum(booked),
                                 conversion_rate = booked/n) %>%
                       mutate(total = sum(n), pct = n/total * 1000))


ggplot(df17, aes(signup_app, conversion_rate)) +
  geom_bar(aes(y = conversion_rate),stat = "identity", fill = "pink") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, color = '#474747'),
        axis.text.x = element_text(size = 10, color = '#474747')) +
  xlab("Signup App") +
  ylab("Conversion Rate [%]") +
  scale_y_continuous(breaks = seq(0, 1, 0.05)) +
  geom_line(aes(y = conversion_rate),group = 1, size = 1.5, color = "light blue") +
  geom_point(size = 3, color = "orange")







df19 = as.data.frame(data %>%
                       group_by(signup_app, age_bkt, ctr, dac_year) %>%
                       summarise(n = n(), booked = sum(booked),
                                 conversion_rate = booked/n) %>%
                       mutate(total = sum(n), pct = n/total * 1000))

lm(conversion_rate ~ ., data = df19)

library(xlsx)
write.csv(data, "C:/Users/leonz/Desktop/BA Class/Capstone_Project/mydata.csv")
