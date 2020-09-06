library(dplyr)
library(lubridate)
library(ggplot2)

### 3. Referred signups
## check duplicates
ref= read.csv('referrals.csv')
trades= read_csv('trades.csv')
str(ref)
ref$referree_signup_dt= ymd_hms(ref$referree_signup_date) 

ref %>%
  group_by(referree_user_id) %>%
  summarise(count=n()) %>%
  filter(count>1)

## plot trends
min(ref$referree_signup_dt)
max(ref$referree_signup_dt)
ref %>%
  group_by(dt= date(referree_signup_dt)) %>%
  summarise(signups=n()) %>%
  ggplot(aes(x=dt, y= signups)) +
  geom_line(color= "#0072B2") +
  scale_x_date(breaks = seq(as.Date("2017-06-02"), as.Date("2017-09-01"), by="15 days"))+
  xlab('date') +
  ylab('number of signups') +
  theme_minimal() 

## join with trades data
str(ref)
sum(!is.na(df$completed_at))
ref$referree_user_id= as.character(ref$referree_user_id)
df= left_join(ref, trades, c("referree_user_id"= "user_id")) %>%
  group_by(referree_user_id, referree_signup_dt) %>%
  summarise(count= n_distinct(completed_at, na.rm = T))

df= df %>%
  mutate(traded = case_when(count > 0  ~ 1,
                            count == 0 ~ 0))

df %>%
  # group_by(week=  week(referree_signup_dt)) %>%
  group_by(week= floor_date(referree_signup_dt, "week", week_start = 1)) %>%
  summarise(signups= n(),
            trade_rate= mean(traded)) %>%
  ggplot() +
  geom_col(aes(x= week, y= signups)) +
  geom_line(aes(x= week, y= trade_rate*10000*2), group= 1) +
  scale_y_continuous(sec.axis = sec_axis(~./10000/2, name = "trade_rate")) +
  # scale_x_date(breaks = seq(as.Date("2017-05-29"), as.Date("2017-08-28"), by="1 week")) +
  theme_minimal()


### 4. Experiment
exp= read.csv('split_test_exposures.csv')
events= read.csv('events.csv')
str(exp)
exp$exposed_dt= ymd_hms(exp$exposed_time)
min(exp$exposed_time)
max(exp$exposed_time) ## 17/6/29-17/9/1

# check sample size and time span
exp %>%
  group_by(split_test_group) %>%
  summarise(min_exposed_time= min(exposed_time),
            max_exposed_time= max(exposed_time),
            users= n_distinct(user_id))
exp %>%
  group_by(date=date(exposed_time), split_test_group) %>%
  summarise(users= n_distinct(user_id)) %>%
  ggplot(aes(x= date, y= users, color= split_test_group)) +
  geom_line() +
  theme_minimal()

# filter events to iOS
events_ios= 
  events %>% filter(platform== 'iOS')

# join experiment data with referrer page view data
exp %>%
  left_join(events_ios, by= 'user_id') %>%
  filter(!is.na(event_time) & event_type== 'referrer_page_viewed') %>%
  group_by(date= date(event_time), split_test_group) %>%
  summarise(pageviews= n()) %>%
  ggplot(aes(x= date, y= pageviews, color= split_test_group)) +
  geom_line() +
  theme_minimal()

# join experiment data with referrer page invite action data
exp %>%
  left_join(events_ios, by= 'user_id') %>%
  filter(!is.na(event_time) & event_type== 'referrer_page_invite_action') %>%
  group_by(date= date(event_time), split_test_group) %>%
  summarise(invite_actions= n()) %>%
  ggplot(aes(x= date, y= invite_actions, color= split_test_group)) +
  geom_line() +
  theme_minimal()

## clean experiment population 
dup=
  exp %>%
  group_by(user_id) %>%
  summarise(cnt= n_distinct(split_test_group)) %>%
  filter(cnt> 1)

exp1=
exp %>%
  left_join(dup, by= 'user_id') %>%
  filter(is.na(cnt)
         & exposed_time< ymd_hms('2017-07-28 00:00:00'))
exp1 %>%
  group_by(exposed_date= date(exposed_time), split_test_group) %>%
  summarise(users= n_distinct(user_id)) %>%
  ggplot(aes(x= exposed_date, y= users, color= split_test_group)) +
  geom_line() +
  theme_minimal()

# check sample sizes and referred page invate actions after cleaning
exp1 %>%
  left_join(events_ios, by= 'user_id') %>%
  filter(!is.na(event_time) & event_type== 'referrer_page_invite_action') %>%
  group_by(event_date= date(event_time), split_test_group) %>%
  summarise(invite_actions= n()) %>%
  ggplot(aes(x= event_date, y= invite_actions, color= split_test_group)) +
  geom_line() +
  theme_minimal()

## join with events data 
events$event_time= ymd_hms(events$event_time)

exp_event= exp1 %>%
  left_join(events_ios, by= 'user_id') 
min(exp_event$event_time, na.rm = T)
max(exp_event$event_time, na.rm = T)

exp_action= exp_event %>%
  group_by(user_id) %>%
  summarise(
    # pv= sum(if_else(event_type== 'referrer_page_viewed' 
    #                         & !is.na(event_time) & exposed_time< event_time
    #                         , 1, 0)),
            invite_actions= sum(if_else(event_type== 'referrer_page_invite_action'
                                & !is.na(event_time)
                                & exposed_time< event_time, 1, 0)))

# summarise number of invate actions for each group
exp_action %>%
  inner_join(exp1, by= 'user_id') %>%
  group_by(split_test_group) %>%
  summarise(volume= n_distinct(user_id),
            action= sum(invite_actions))  

## t-test
group_action= exp_action %>%
  inner_join(exp1, by= 'user_id')
pairwise.t.test(group_action$invite_actions, group_action$split_test_group
                ,'none')
pairwise.t.test(group_action$invite_actions, group_action$split_test_group
                ,'bonferroni')

## confidence interval with bonferroni adjustment
require(DescTools)
res <- aov(formula = invite_actions ~ split_test_group, data = group_action)
PostHocTest(res, method = "bonferroni")


  



