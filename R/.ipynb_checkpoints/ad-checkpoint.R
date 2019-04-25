install.packages("rmarkdown")
install.packages('tinytex')


library(rJava)
library(RJDBC)
library(ggplot2)
library(scales)
library(dplyr)
library(rjags)
library(tinytex)

jdbcDriver <- JDBC(driverClass="com.snowflake.client.jdbc.SnowflakeDriver",
                   classPath="/Users/leon.zhao/Documents/snowflake-jdbc-3.5.3.jar")
jdbcConnection <- dbConnect(jdbcDriver,
                            "jdbc:snowflake://offerup.snowflakecomputing.com:443/?WAREHOUSE=ANALYTICS_WH&DATABASE=APPSFLYER_PROD&ROLE=ANALYTICS_TOOL&AUTHENTICATOR=SNOWFLAKE",
                            "leon.zhao@offerupnow.com",
                            "Steel24ly@offerup")

bumper = "select 
          cal_date, 
          metric_name,
          variant_name, 
          metric_value
          from leanplum.experiments.daily_experiments_aggregates
          where cal_date >= '2018-04-13'
          and metric_name = 'Bumpers per DAU'
          and experiment_id = 6581618470944768
          order by 1, 2, 3
          ;"

seller_ads = "select 
              cal_date, 
              metric_name,
              variant_name, 
              metric_value
              from leanplum.experiments.daily_experiments_aggregates
              where cal_date >= '2018-04-13'
              and metric_name = 'Seller Ad Purchasers per DAU'
              and experiment_id = 6581618470944768
              order by 1, 2, 3
              ;"

rawdata = "select 
          cal_date, 
          metric_name,
          variant_name, 
          metric_value
          from leanplum.experiments.daily_experiments_aggregates
          where cal_date >= '2018-04-13'
          and (metric_name = 'Bumpers'
          or metric_name = 'Seller Ad Purchasers'
          or metric_name = 'DAU')
          and experiment_id = 6581618470944768
          order by 1, 2, 3
          ;"

revenue = "select 
          cal_date, 
          metric_name,
          variant_name, 
          metric_value
          from leanplum.experiments.daily_experiments_aggregates
          where cal_date >= '2018-04-13'
          and metric_name = 'Seller Ads Revenue per DAU'
          and experiment_id = 6581618470944768
          order by 1, 2, 3
          ;" 

percent_dau_selling = "select 
                      cal_date, 
                      metric_name,
                      variant_name, 
                      metric_value
                      from leanplum.experiments.daily_experiments_aggregates
                      where cal_date >= '2018-04-13'
                      and metric_name = 'Sellers per DAU'
                      and experiment_id = 6581618470944768
                      order by 1, 2, 3
                      ;"

bumpers_user_bkt = "select lhs.cal_date,
                    lhs.variant_name,
                    lhs.experiment_name,
                    lhs.metric_name,
                    lhs.metric_value,
                    rhs.total_users_bucketed
                    from
                    (select 
                    cal_date, 
                    metric_name,
                    variant_name,
                    experiment_name,
                    experiment_id,
                    variant_id,
                    metric_value
                    from leanplum.experiments.daily_experiments_aggregates
                    where cal_date >= '2018-04-13'
                    and (metric_name = 'Bumpers'
                    or metric_name = 'Seller Ad Purchasers')
                    and experiment_id = 5001668643127296
                    order by 1, 2, 3
                    ) lhs
                    join
                    (
                      select * from
                      ou_users.prasad.cumulative_users_bucketed
                      where experiment_id = 5001668643127296
                      and cal_date >= '2018-04-13'
                    ) rhs
                      on lhs.cal_date = rhs.cal_date
                      and lhs.experiment_id = rhs.experiment_id
                      and lhs.variant_id = rhs.variant_id
                      order by 1
                      ;
                      "


bumper_per_dau = dbGetQuery(jdbcConnection, bumper)
seller_ads_per_dau = dbGetQuery(jdbcConnection, seller_ads)
df1 = dbGetQuery(jdbcConnection, rawdata)
dau_selling = dbGetQuery(jdbcConnection, percent_dau_selling) 
rev_1 = dbGetQuery(jdbcConnection, revenue)

bumper_per_ubkt = dbGetQuery(jdbcConnection, bumpers_user_bkt)


bumper_per_dau
seller_ads_per_dau
df1
dau_selling
bumper_per_ubkt




bumper_clean = data.frame(1:27)
bumper_clean$caret_1 = filter(bumper_per_dau, VARIANT_NAME == 'Caret 1')[,4]
bumper_clean$caret_2 = filter(bumper_per_dau, VARIANT_NAME == 'Caret 2')[,4]
bumper_clean$promote_button = filter(bumper_per_dau, VARIANT_NAME == 'Promote Button')[,4]
bumper_clean$promote_button_2 = filter(bumper_per_dau, VARIANT_NAME == 'Promote Button 2')[,4]

bumper_clean

t.test(bumper_clean$caret_1,bumper_clean$promote_button, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(bumper_clean$caret_1,bumper_clean$caret_2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(bumper_clean$caret_1,bumper_clean$promote_button_2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(bumper_clean$caret_2,bumper_clean$promote_button, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(bumper_clean$caret_2,bumper_clean$promote_button_2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

wilcox.test(bumper_clean$caret_1,bumper_clean$promote_button)

#stat-sig

bumper_clean = bumper_clean %>% mutate((caret_1+caret_2)/2, (promote_button+promote_button_2)/2)
bumper_clean_total = bumper_clean[-2:-5] %>% rename(carets = `(caret_1 + caret_2)/2`, promote_buttons = `(promote_button + promote_button_2)/2`)

t.test(bumper_clean_total$carets,bumper_clean_total$promote_buttons, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


seller_ads_clean = seller_ads_per_dau[1:2]
seller_ads_clean$caret_1 = filter(seller_ads_per_dau, VARIANT_NAME == 'Caret 1')[,4]
seller_ads_clean$caret_2 = filter(seller_ads_per_dau, VARIANT_NAME == 'Caret 2')[,4]
seller_ads_clean$promote_button = filter(seller_ads_per_dau, VARIANT_NAME == 'Promote Button')[,4]
seller_ads_clean$promote_button_2 = filter(seller_ads_per_dau, VARIANT_NAME == 'Promote Button 2')[,4]



t.test(seller_ads_clean$caret_1,seller_ads_clean$promote_button, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(seller_ads_clean$caret_1,seller_ads_clean$caret_2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
#stat-sig
t.test(seller_ads_clean$caret_1,seller_ads_clean$promote_button_2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(seller_ads_clean$caret_2,seller_ads_clean$promote_button, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
#stat-sig
t.test(seller_ads_clean$caret_2,seller_ads_clean$promote_button_2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)



raw_bumpers = filter(df1, METRIC_NAME == 'Bumpers')
raw_seller_ads = filter(df1, METRIC_NAME == 'Seller Ad Purchasers')
raw_dau = filter(df1, METRIC_NAME == 'DAU')



C_bumpers = raw_bumpers[1:2]
C_bumpers$caret_1 = filter(raw_bumpers, VARIANT_NAME == 'Caret 1')[,4]
C_bumpers$caret_2 = filter(raw_bumpers, VARIANT_NAME == 'Caret 2')[,4]
C_bumpers$promote_button = filter(raw_bumpers, VARIANT_NAME == 'Promote Button')[,4]
C_bumpers$promote_button_2 = filter(raw_bumpers, VARIANT_NAME == 'Promote Button 2')[,4]

C_bumpers
C_bumpers[3:6] %>% str()
c_bumpers = C_bumpers %>% 
  mutate((caret_1 + caret_2)/2, (promote_button + promote_button_2)/2)

c_bumpers = c_bumpers %>% rename(carets = `(caret_1 + caret_2)/2`, promote_buttons = `(promote_button + promote_button_2)/2`)





c_seller_ads = raw_seller_ads[1:2]
c_seller_ads$caret_1 = filter(raw_seller_ads, VARIANT_NAME == 'Caret 1')[,4]
c_seller_ads$caret_2 = filter(raw_seller_ads, VARIANT_NAME == 'Caret 2')[,4]
c_seller_ads$promote_button = filter(raw_seller_ads, VARIANT_NAME == 'Promote Button')[,4]
c_seller_ads$promote_button_2 = filter(raw_seller_ads, VARIANT_NAME == 'Promote Button 2')[,4]

c_seller_ads = c_seller_ads %>% 
  mutate((caret_1 + caret_2)/2, (promote_button + promote_button_2)/2)

c_seller_ads = c_seller_ads %>% rename(carets = `(caret_1 + caret_2)/2`, promote_buttons = `(promote_button + promote_button_2)/2`)


c_dau = raw_dau[1:2]
c_dau$caret_1 = filter(raw_dau, VARIANT_NAME == 'Caret 1')[,4]
c_dau$caret_2 = filter(raw_dau, VARIANT_NAME == 'Caret 2')[,4]
c_dau$promote_button = filter(raw_dau, VARIANT_NAME == 'Promote Button')[,4]
c_dau$promote_button_2 = filter(raw_dau, VARIANT_NAME == 'Promote Button 2')[,4]

c_dau = c_dau %>% 
  mutate((caret_1 + caret_2)/2, (promote_button + promote_button_2)/2)

c_dau = c_dau %>% rename(carets = `(caret_1 + caret_2)/2`, promote_buttons = `(promote_button + promote_button_2)/2`)

all_together = c(c_bumpers[-3:-6], c_seller_ads[7:8], c_dau[7:8])

all_together = as.data.frame(all_together) %>%
  mutate((carets + carets.1)/carets.2, (promote_buttons + promote_buttons.1)/promote_buttons.2)
all_together = all_together %>%
  rename(control = `(carets + carets.1)/carets.2`, treatment = `(promote_buttons + promote_buttons.1)/promote_buttons.2`)
all_together = all_together %>% select(control, treatment)  

t.test(all_together$control,all_together$treatment, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


dau_selling_clean = dau_selling[1:2]
dau_selling_clean$caret_1 = filter(dau_selling, VARIANT_NAME == 'Caret 1')[,4]
dau_selling_clean$caret_2 = filter(dau_selling, VARIANT_NAME == 'Caret 2')[,4]
dau_selling_clean$promote_button = filter(dau_selling, VARIANT_NAME == 'Promote Button')[,4]
dau_selling_clean$promote_button_2 = filter(dau_selling, VARIANT_NAME == 'Promote Button 2')[,4]

dau_selling_clean

t.test(dau_selling_clean$caret_1,dau_selling_clean$promote_button, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(dau_selling_clean$caret_1,dau_selling_clean$caret_2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(dau_selling_clean$caret_1,dau_selling_clean$promote_button_2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(dau_selling_clean$caret_2,dau_selling_clean$promote_button, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(dau_selling_clean$caret_2,dau_selling_clean$promote_button_2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)




rev_clean = rev_1[1:2]
rev_clean$caret_1 = filter(rev_1, VARIANT_NAME == 'Caret 1')[,4]
rev_clean$caret_2 = filter(rev_1, VARIANT_NAME == 'Caret 2')[,4]
rev_clean$promote_button = filter(rev_1, VARIANT_NAME == 'Promote Button')[,4]
rev_clean$promote_button_2 = filter(rev_1, VARIANT_NAME == 'Promote Button 2')[,4]

rev_clean

wilcox.test(rev_clean$caret_1, rev_clean$promote_button)
wilcox.test(rev_clean$caret_1, rev_clean$promote_button_2)
wilcox.test(rev_clean$caret_2, rev_clean$promote_button)
wilcox.test(rev_clean$caret_2, rev_clean$promote_button_2)
wilcox.test(rev_clean$caret_1, rev_clean$caret_2)


ggplot(rev_clean, aes(CAL_DATE)) +
  geom_line(aes(y = caret_1), group=1, size = 1, color = "light blue") +
  geom_line(aes(y = caret_2), group=1, size = 1, color = "navy") +
  geom_line(aes(y = promote_button), group=1, size = 1, color = "green") +
  geom_line(aes(y = promote_button_2), group=1, size = 1, color = "pink") 




## users bucketed calculation

ubked_bumpers = filter(bumper_per_ubkt, METRIC_NAME == 'Bumpers')
ubked_seller_ads = filter(bumper_per_ubkt, METRIC_NAME == 'Seller Ad Purchasers')


c_bumpers = data.frame(1:27)
c_bumpers$caret_1 = filter(ubked_bumpers, VARIANT_NAME == 'Caret 1')[,5]
c_bumpers$caret_2 = filter(ubked_bumpers, VARIANT_NAME == 'Caret 2')[,5]
c_bumpers$promote_button = filter(ubked_bumpers, VARIANT_NAME == 'Promote Button')[,5]
c_bumpers$promote_button_2 = filter(ubked_bumpers, VARIANT_NAME == 'Promote Button 2')[,5]
c_bumpers$total_ubkt = filter(ubked_bumpers, VARIANT_NAME == 'Promote Button 2')[,6]


c_bumpers
c_bumpers %>% str()
c_bumpers = c_bumpers %>% 
  mutate((caret_1 + caret_2)/2, (promote_button + promote_button_2)/2) 
c_bumpers = c_bumpers %>% rename(carets = `(caret_1 + caret_2)/2`, promote_buttons = `(promote_button + promote_button_2)/2`)
c_bumpers= c_bumpers %>% 
  mutate(carets/total_ubkt, promote_buttons/total_ubkt)


t.test(c_bumpers$`carets/total_ubkt`,c_bumpers$`promote_buttons/total_ubkt` , alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


c_seller_ads = data.frame(1:27)
c_seller_ads$caret_1 = filter(ubked_seller_ads, VARIANT_NAME == 'Caret 1')[,5]
c_seller_ads$caret_2 = filter(ubked_seller_ads, VARIANT_NAME == 'Caret 2')[,5]
c_seller_ads$promote_button = filter(ubked_seller_ads, VARIANT_NAME == 'Promote Button')[,5]
c_seller_ads$promote_button_2 = filter(ubked_seller_ads, VARIANT_NAME == 'Promote Button 2')[,5]
c_seller_ads$total_ubkt = filter(ubked_seller_ads, VARIANT_NAME == 'Promote Button 2')[,6]


c_seller_ads
c_seller_ads %>% str()
c_seller_ads = c_seller_ads %>% 
  mutate((caret_1 + caret_2)/2, (promote_button + promote_button_2)/2) 
c_seller_ads = c_seller_ads %>% rename(carets = `(caret_1 + caret_2)/2`, promote_buttons = `(promote_button + promote_button_2)/2`)
c_seller_ads= c_seller_ads %>% 
  mutate(carets/total_ubkt, promote_buttons/total_ubkt)

t.test(c_seller_ads$`carets/total_ubkt`,c_seller_ads$`promote_buttons/total_ubkt` , alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)




c_total = data.frame(1:27)
c_total$carets = c_bumpers$carets + c_seller_ads$carets
c_total$promote_buttons = c_bumpers$promote_buttons + c_seller_ads$promote_buttons





## users bucketed calculation - performance dashboard

ubked_bumpers = filter(bumper_per_ubkt, METRIC_NAME == 'Bumpers')
ubked_seller_ads = filter(bumper_per_ubkt, METRIC_NAME == 'Seller Ad Purchasers')


c_bumpers = data.frame(1:22)
c_bumpers$cal_date = filter(ubked_bumpers, VARIANT_NAME == 'Control')[,1]
c_bumpers$control = filter(ubked_bumpers, VARIANT_NAME == 'Control')[,5]
c_bumpers$variant1 = filter(ubked_bumpers, VARIANT_NAME == 'Variant 1')[,5]
c_bumpers$total_ubkt_control = filter(ubked_bumpers, VARIANT_NAME == 'Control')[,6]
c_bumpers$total_ubkt_variant1 = filter(ubked_bumpers, VARIANT_NAME == 'Variant 1')[,6]


c_bumpers
c_bumpers %>% str() 
c_bumpers= c_bumpers %>% 
  mutate(control/total_ubkt_control, variant1/total_ubkt_variant1)


t.test(c_bumpers$`control/total_ubkt_control`,c_bumpers$`variant1/total_ubkt_variant1` , alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)




c_seller_ads = data.frame(1:27)
c_seller_ads$caret_1 = filter(ubked_seller_ads, VARIANT_NAME == 'Caret 1')[,5]
c_seller_ads$caret_2 = filter(ubked_seller_ads, VARIANT_NAME == 'Caret 2')[,5]
c_seller_ads$promote_button = filter(ubked_seller_ads, VARIANT_NAME == 'Promote Button')[,5]
c_seller_ads$promote_button_2 = filter(ubked_seller_ads, VARIANT_NAME == 'Promote Button 2')[,5]
c_seller_ads$total_ubkt = filter(ubked_seller_ads, VARIANT_NAME == 'Promote Button 2')[,6]


c_seller_ads
c_seller_ads %>% str()
c_seller_ads = c_seller_ads %>% 
  mutate((caret_1 + caret_2)/2, (promote_button + promote_button_2)/2) 
c_seller_ads = c_seller_ads %>% rename(carets = `(caret_1 + caret_2)/2`, promote_buttons = `(promote_button + promote_button_2)/2`)
c_seller_ads= c_seller_ads %>% 
  mutate(carets/total_ubkt, promote_buttons/total_ubkt)

t.test(c_seller_ads$`carets/total_ubkt`,c_seller_ads$`promote_buttons/total_ubkt` , alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

?t.test
t.test(0.0009934543,0.0010315725, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

c_total = data.frame(1:27)
c_total$carets = c_bumpers$carets + c_seller_ads$carets
c_total$promote_buttons = c_bumpers$promote_buttons + c_seller_ads$promote_buttons









## fun test

data = read.csv('/Users/leon.zhao/Desktop/data.csv', header = T)
str(data)

install.packages('weights')
library(weights)

a = wtd.t.test(data$Conntrol.Posters.per.DAU , 
               data$Treatment.Posters.per.DAU, 
               weight = data$Conntrol.Posters.per.DAU, 
               weighty = data$Treatment.Posters.per.DAU, 
               alternative = c("two.sided"), bootse=TRUE)
a
a$coefficients


t.test(data$Control, data$Treatment, alternative = c("two.sided"),
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)




i = 2

for (i in 1:20) {
  
while(i < 10) 
  {
    i = i + 1; 
    print(i);
    }
}


###


x = rnorm(30, 45827, 55)
y = rnorm(30, 39892, 64)
n1 = rnorm(30, 3489860, 260)
n2 = rnorm(30, 3576294, 515)


x
y
n1
n2

summary(df1)
df1 = data.frame(1:30)
df1$control = x
df1$treatment = y
df1$control_n = n1
df1$treatment_n = n2
df1 = df1[,-1]


ttest = t.test(x/n1, y/n2, alternative = c("two.sided"),
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)
ttest

ttest$statistic
ttest$method
ttest$estimate
ttest$parameter


wtd.t.test(x, y, 
           weight = n1, 
           weighty = n2, 
           alternative = c("two.sided"), bootse=TRUE)

write.csv(df1,
          "/Users/leon.zhao/Desktop/abtest_data/sampledata.csv",
          row.names = FALSE)

?t.test

pnorm(-23.893273041, mean = 0, sd = 1)
pnorm(-4.9992, mean = 0, sd = 1)
