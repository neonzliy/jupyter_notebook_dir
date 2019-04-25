bump = read.csv("/Users/leon.zhao/Desktop/abtest_data/bump_caret.csv", header = T)
promo = read.csv("/Users/leon.zhao/Desktop/abtest_data/seller_ad_caret.csv", header = T)
## purchased = read.csv("/Users/leon.zhao/Desktop/abtest_data/promo_reorder_purchased.csv", header = T)


library(rJava)
library(RJDBC)
library(ggplot2)
library(scales)
library(dplyr)
library(rjags)

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
          and experiment_id = 5421687952769024
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
          and experiment_id = 5421687952769024
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
          or metric_name = 'Seller Ads'
          or metric_name = 'DAU')
          and experiment_id = 5421687952769024
          order by 1, 2, 3
          ;"




bumper_per_dau = dbGetQuery(jdbcConnection, bumper)
seller_ads_per_dau = dbGetQuery(jdbcConnection, seller_ads)
df1 = dbGetQuery(jdbcConnection, rawdata)

bumper_per_dau
seller_ads_per_dau
df1

remove(bumper_clean)


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


seller_ads_clean = data.frame(1:27)
seller_ads_clean$caret_1 = filter(seller_ads_per_dau, VARIANT_NAME == 'Caret 1')[,4]
seller_ads_clean$caret_2 = filter(seller_ads_per_dau, VARIANT_NAME == 'Caret 2')[,4]
seller_ads_clean$promote_button = filter(seller_ads_per_dau, VARIANT_NAME == 'Promote Button')[,4]
seller_ads_clean$promote_button_2 = filter(seller_ads_per_dau, VARIANT_NAME == 'Promote Button 2')[,4]

seller_ads_clean


t.test(seller_ads_clean$caret_1,seller_ads_clean$promote_button, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(seller_ads_clean$caret_1,seller_ads_clean$caret_2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
#stat-sig
t.test(seller_ads_clean$caret_1,seller_ads_clean$promote_button_2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(seller_ads_clean$caret_2,seller_ads_clean$promote_button, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(seller_ads_clean$caret_2,seller_ads_clean$promote_button_2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)





