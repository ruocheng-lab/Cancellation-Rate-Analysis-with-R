library("data.table")
library("ggplot2")
library("psych")
library("dplyr")
library("tidyr")

rm(list = ls())
setwd("/Users/jady/Desktop/UTD/semester2/6337_predictive analysis/project")
# load the data, hotel_bookings_raw
hotel_df <- fread("hotel_bookings_raw.csv")

head(hotel_df)

# check the structure of the data
str(hotel_df)

# convert the data type to factor
hotel_df <- hotel_df %>% 
  mutate(
    hotel=as.factor(hotel),      
    is_canceled=as.factor(is_canceled),
    meal=as.factor(meal),
    country=as.factor(country),
    market_segment=as.factor(market_segment),
    distribution_channel=as.factor(distribution_channel),
    is_repeated_guest=as.factor(is_repeated_guest),
    reserved_room_type=as.factor(reserved_room_type),
    assigned_room_type=as.factor(assigned_room_type),
    deposit_type=as.factor(deposit_type),
    customer_type=as.factor(customer_type),
    reservation_status=as.factor(reservation_status),
    agent=as.factor(agent),
    company=as.factor(company),
    arrival_date_day_of_month=as.factor(arrival_date_day_of_month),
    arrival_date_month=as.factor(arrival_date_month),
    arrival_date_year=as.factor(arrival_date_year)
    
  )


# check the missing values
colSums(is.na(hotel_df))

# check the unique values of each column
sapply(hotel_df, function(x) length(unique(x)))

# company & agent column has too many unique values, remove this column
# arrival_date_year seems not useful, remove this column
# reservation_status_date seems not useful, remove this column
# reservation_status is the same as is_canceled, remove this column

# remove columns, 'company', 'arrival_date_year', 'reservation_status_date', 
hotel_df_v1 <- hotel_df %>% 
  select(-c(company, arrival_date_year, reservation_status_date, reservation_status, agent, company, country))

head(hotel_df_v1)

# convert childern column to integer
hotel_df_v1$children <- as.integer(hotel_df_v1$children)
# check unique values of children column
unique(hotel_df_v1$children)
# create table for each unique value of children column
table(hotel_df_v1$children)
# remove rows with children >= 10
hotel_df_v1 <- hotel_df_v1[children < 10, ]
# check unique values of children column
unique(hotel_df_v1$children)

# check reserved_room_type and assigned_room_type columns
# if reserved_room_type is different from assigned_room_type, it is considered as reassigned
# create a new column called room_reassigned
# if reserved_room_type is equal to assigned_room_type, room_reassigned = 0
# if reserved_room_type is different from assigned_room_type, room_reassigned = 1

# Convert factor variables to character
hotel_df_v1$reserved_room_type <- as.character(hotel_df_v1$reserved_room_type)
hotel_df_v1$assigned_room_type <- as.character(hotel_df_v1$assigned_room_type)

# Create new column 'room_reassigned'
hotel_df_v1$room_reassigned <- ifelse(hotel_df_v1$reserved_room_type == hotel_df_v1$assigned_room_type, 0, 1)
# drop reserved_room_type and assigned_room_type columns
hotel_df_v1 <- hotel_df_v1[, -c('reserved_room_type', 'assigned_room_type')]
# convert room_reassigned to factor
hotel_df_v1$room_reassigned <- as.factor(hotel_df_v1$room_reassigned)

# check for missing values -> No missing values in hotel_df_v1
colSums(is.na(hotel_df_v1))

# check the unique values of room_reassigned
table(hotel_df_v1$room_reassigned)


########################
# Below is data exploration
########################



# (1) check the cancellation rate by each month (arrival_date_month)
# create table for each unique value of arrival_date_month
table(hotel_df_v1$arrival_date_month)
# November ~ February have low number of bookings
# check the cancellation rate by month

hotel_df_v1$arrival_date_month <- factor(hotel_df_v1$arrival_date_month, 
    levels = c("January", "February", "March", "April", "May", "June", "July", 
    "August", "September", "October", "November", "December"))

monthly_cancellation_data <- hotel_df_v1 %>%
  group_by(arrival_date_month) %>%
  summarise(
    cancel_rate = mean(is_canceled == 1)
  )

print(monthly_cancellation_data)
# visualizing cancellation rate by month
hotel_df_v1 %>% 
  group_by(arrival_date_month) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  ) %>% 
  ggplot(aes(x = arrival_date_month, y = cancel_rate, fill = arrival_date_month)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired")+
  geom_text(aes(label = paste0(round(cancel_rate * 100, 2), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.5,
            color = "black") +
  labs(title = "Cancellation rate by month", x = "Month", y = "Cancellation rate")

#Chi-Square Test
chisq.test(monthly_cancellation_data$cancel_rate)
#The very small Chi-squared statistic (X-squared = 0.04387) 
#and the high p-value (p-value = 1) suggest that 
# cancellation rate by month range from 0.3 to 0.5 but no significant difference








# (2) check the cancellation rate by hotel
hotel_df_v2 <- hotel_df_v1 %>%
  group_by(hotel) %>%
  summarise(
    cancel_rate = mean(is_canceled == 1, na.rm = TRUE),
  )
hotel_df_v2
ggplot(hotel_df_v2, aes(x = hotel, y = cancel_rate, fill = hotel)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("#F4511E", "#7D3C98")) +
  labs(title = "Cancellation Rate for Resort vs City Hotel",
       x = "Hotel Type",
       y = "Cancellation Rate (%)") +
  geom_text(aes(label = paste0(round(cancel_rate * 100, 2), "%")),
            position = position_stack(vjust = 0.5),
            size = 5,
            color = "white") +
  theme_minimal()

# city hotel has higher cancellation rate than resort hotel, why?

# check the cancellation rate by hotel and adr
hotel_df_v1 %>% 
  group_by(hotel) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1),
    avg_adr = mean(adr)
  )
# the average rate of city hotel is higher than resort hotel, 
# would this be the reason that city hotel has higher cancellation rate?
# Need a plot?
# create boxplot for adr by hotel
hotel_df_v1 %>% 
  ggplot(aes(x = hotel, y = adr, fill = hotel)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#F4511E", "#7D3C98")) +
  labs(title = "Average daily rate by hotel", x = "Hotel", y = "Average daily rate")

# one outliner in city hotel, exclude the point above 1000 in the data
hotel_df_v1 %>% 
  filter(adr < 1000) %>% 
  ggplot(aes(x = hotel, y = adr, fill = hotel)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#F4511E", "#7D3C98")) +
  labs(title = "Average daily rate by hotel", x = "Hotel", y = "Average daily rate")


# (3) check the cancellation rate by hotel and lead_time
hotel_df_v1 %>% 
  group_by(hotel) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1),
    avg_lead_time = mean(lead_time)
  )
# city hotel has higher cancellation rate and higher lead time

# create boxplot for lead_time by hotel
hotel_df_v1 %>% 
  ggplot(aes(x = hotel, y = lead_time, fill = hotel)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#F4511E", "#7D3C98")) +
  labs(title = "Lead time by hotel", x = "Hotel", y = "Lead time")
# create distribution plot for lead_time by hotel
hotel_df_v1 %>% 
  ggplot(aes(x = lead_time, fill = hotel, fill = hotel)) +
  geom_histogram(alpha = 0.85, bins = 30) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("#F4511E", "#7D3C98")) +
  labs(title = "Lead time distribution by hotel", x = "Lead time", y = "Density")


# (4) check the cancellation rate by hotel and market_segment
hotel_df_v1 %>% 
  group_by(hotel, market_segment) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  )
# no matter city or resort, market segment 'Groups' has the highest cancellation rate
# create a plot for the cancellation rate by hotel and market_segment
hotel_df_v1 %>% 
  group_by(hotel, market_segment) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  ) %>% 
  ggplot(aes(x = market_segment, y = cancel_rate, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#F4511E", "#7D3C98")) +
  labs(title = "Cancellation rate by market segment", x = "Market segment", y = "Cancellation rate")+
  geom_text(aes(label = paste0(round(cancel_rate * 100, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.25,
            size = 5,
            color = "black")


# (5) segment the lead time into 5 groups (lead time is a continuous variable, segment into 5 groups)
# ignore lead time < 0 issue for now
hotel_df_v1$lead_time_group <- cut(hotel_df_v1$lead_time, breaks = 5)
# count in each group
table(hotel_df_v1$lead_time_group)
# check the cancellation rate by hotel and lead_time_group
hotel_df_v1 %>% 
  group_by(hotel, lead_time_group) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  )
# check the cancellation rate by lead_time_group
hotel_df_v3 <- hotel_df_v1 %>% 
  group_by(lead_time_group) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  )
#plot
ggplot(hotel_df_v3, aes(x = lead_time_group, y = cancel_rate, fill = lead_time_group)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Cancellation Rate by Lead Time Group",
       x = "Lead Time Group",
       y = "Cancellation Rate") +
  scale_fill_brewer(palette = "Oranges")+
  geom_text(aes(label = scales::percent(cancel_rate)),
            vjust = -0.3,
            size = 5) +
  theme_minimal() + 
  theme(legend.position = "none")
# There appears to be a direct relationship between the length of the lead time 
#and the cancellation rate. As the lead time increases, 
#so does the likelihood of a booking being canceled.
#This could be because customers with longer lead times are more likely 
#to experience changes that force them to cancel, like conflicting schedules 
#or financial shifts. Shorter lead times may reflect more definite travel plans, 
#suggesting a lower risk of cancellation. For hotels, recognizing this pattern 
#is key for minimizing financial risk, potentially informing policies 
#on overbooking and cancellations for long-lead-time reservations.

# (6) check the cancellation rate by total planned length of days to stay(ttl_stays_group)
#create a new column ttl_stays = stays_in_weekend_nights + stays_in_week_nights
hotel_df_v1$ttl_stays <- hotel_df_v1$stays_in_weekend_nights + hotel_df_v1$stays_in_week_nights
# unique values of ttl_stays
unique(hotel_df_v1$ttl_stays)
# segment the ttl_stays into 5 groups
hotel_df_v1$ttl_stays_group <- cut(hotel_df_v1$ttl_stays, breaks = 5)
# count in each group
table(hotel_df_v1$ttl_stays_group)
# ttl stays day above 14 is considered as long stay
# focus on ttl stays 14 days or less, and segment into 5 groups
hotel_df_v1$ttl_stays_group <- cut(hotel_df_v1$ttl_stays, breaks = c(-Inf, 0, 2, 5, 8, 10, 14,20,30,40,50,Inf))


hotel_df_v1 %>% 
  group_by(ttl_stays_group) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  )
hotel_df_v1 %>% 
  group_by(ttl_stays_group) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  ) %>% 
  ggplot(aes(x = ttl_stays_group, y = cancel_rate, fill = ttl_stays_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Paired")+
#  geom_line(aes(group = 1), color = "black", size = 1) +
#  geom_point(color = "grey", size = 3) +
  geom_text(aes(label = scales::percent(cancel_rate)),
            vjust = -0.7,
            position = position_dodge(width = 0.9), 
            size = 4) +
  labs(title = "Comparison of Cancellation Rates by Planned Length of Stay",
       x = "Groups by Length of Stay",
       y = "Cancellation Rate") 




# (7) Analyzing the influence of planned length of stay on cancellation rate of different hotels 

# check the cancellation rate by hotel and ttl_stays_group
hotel_df_v1 %>% 
  group_by(hotel, ttl_stays_group) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  )
# ttl stays within 14 days, the longer the stay, the higher the cancellation rate for city hotel
# for resort hotel, the cancellation rate is similar for all groups
# create a plot for the cancellation rate by hotel and ttl_stays_group
hotel_df_v1 %>% 
  group_by(hotel, ttl_stays_group) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  ) %>% 
  ggplot(aes(x = ttl_stays_group, y = cancel_rate, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#F4511E", "#7D3C98")) +
  geom_text(aes(label = scales::percent(cancel_rate)),
            vjust = -0.3,
            position = position_dodge(width = 0.9), 
            size = 4) +
  labs(title = "Comparison of Cancellation Rates by Planned Length of Stay of Different Hotels",
       x = "Groups by Length of Stay",
       y = "Cancellation Rate") 





# (8) check the cancellation rate by hotel and customer_type
hotel_df_v1 %>% 
  group_by(hotel, customer_type) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  )
# city hotel and resort hotel have different pattern
# city hotel has higher cancellation rate for 'Transient', 'Transient-Party' and 'Contract'
# resort hotel has higher cancellation rate for 'Group'
# create a plot for the cancellation rate by hotel and customer_type
hotel_df_v1 %>% 
  group_by(hotel, customer_type) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  ) %>% 
  ggplot(aes(x = customer_type, y = cancel_rate, fill = hotel)) +
  scale_fill_manual(values = c("#F4511E", "#7D3C98")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(cancel_rate)),
            vjust = -0.3,
            position = position_dodge(width = 0.9), 
            size = 4) +
  labs(title = "Cancellation rate by customer type", x = "Customer type", y = "Cancellation rate")

### Since the country column was dropped, will use the original data to check the cancellation rate by country
# check the cancellation rate by country, exclude the booking number below 500
hotel_df %>% 
  group_by(country) %>% 
  filter(n() > 500) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  ) %>% 
  arrange(desc(cancel_rate))
# Portugal has the highest cancellation rate
# create table for each unique value of country
table(hotel_df$country)
# Portugal has the highest number of bookings
# create a plot for the cancellation rate by country

hotel_df %>% 
  group_by(country) %>% 
  filter(n() > 500) %>% 
  summarise(
    cancel_rate = mean(is_canceled == 1)
  ) %>% 
  ggplot(aes(x = reorder(country, -cancel_rate), y = cancel_rate, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Cancellation rate by country", x = "Country", y = "Cancellation rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



########################
#Using hotel_df_v1 for the following analysis
########################

# check the structure of the data
str(hotel_df_v1)

## Logistic regression with all variables
# split the data into training and testing
set.seed(123)
train_index <- sample(1:nrow(hotel_df_v1), 0.7*nrow(hotel_df_v1))
train <- hotel_df_v1[train_index, ]
test <- hotel_df_v1[-train_index, ]

# logistic regression
logit_model_bsl <- glm(is_canceled ~ ., data = train, family = binomial)
# model summary
summary(logit_model_bsl)

# predict on test data
test$pred_bsl <- predict(logit_model_bsl, newdata = test, type = "response")
# confusion matrix
table(test$is_canceled, test$pred_bsl > 0.5)

# accuracy
# sum(test$is_canceled == (test$pred_bsl > 0.5))/nrow(test)
# I am not sure why the accuracy is 0 by the above code
# I use the following code to calculate the accuracy
(20845 + 8232) / (20845 + 8232 + 5043 + 1696)
# the accuracy is 0.8118


# But this model contain too many variables, let's try to reduce the variables
# especially the arrival date variables
hotel_df_v2 <- hotel_df_v1 %>% 
  select(-c(arrival_date_day_of_month, lead_time_group, ttl_stays_group))


## Logistic regression with reduced variables

# split the data into training and testing
set.seed(123)
train_index <- sample(1:nrow(hotel_df_v2), 0.7*nrow(hotel_df_v2))
train <- hotel_df_v2[train_index, ]
test <- hotel_df_v2[-train_index, ]

# logistic regression
logit_model_v2 <- glm(is_canceled ~ ., data = train, family = binomial)
# model summary
summary(logit_model_v2)

# predict on test data
test$pred_v2 <- predict(logit_model_v2, newdata = test, type = "response")
# confusion matrix
table(test$is_canceled, test$pred_v2 > 0.5)

# accuracy
(20842 + 8277) / (20842 + 8277 + 4998 + 1699)

# bsl & v2 logistic model is not that different
# but v2 model has less variables

########################
## decision tree
########################
library(rpart)
library(rpart.plot)
library(pROC)

# split the data into training and testing
set.seed(123)

train_index <- sample(1:nrow(hotel_df_v2), 0.7*nrow(hotel_df_v2))
train <- hotel_df_v2[train_index, ]
test <- hotel_df_v2[-train_index, ]

# decision tree
tree_model_full <- rpart(is_canceled ~ ., data = train, method = "class", cp = 0, minsplit = 2, minbucket = 1)

prp(tree_model_full, type = 1, extra = 1, under = TRUE)

# plot the tree
plot(tree_model_full)

printcp(tree_model_full)

pruned_tree <- prune(tree_model_full, cp =  7.1096e-05)

prp(pruned_tree, type = 1, extra = 1, under = TRUE)


# predict on test data
test$pred_tree_prun <- predict(pruned_tree, newdata = test, type = "class")
# confusion matrix
table(test$is_canceled, test$pred_tree_prun)

# accuracy
sum(test$is_canceled == test$pred_tree_prun)/nrow(test)


## Use Random Forest
library(randomForest)

# split the data into training and testing
set.seed(123)

train_index <- sample(1:nrow(hotel_df_v2), 0.7*nrow(hotel_df_v2))
train <- hotel_df_v2[train_index, ]
test <- hotel_df_v2[-train_index, ]

# random forest
rf_model <- randomForest(is_canceled ~ ., data = train, ntree = 500, mtry = 3)

# predict on test data
test$pred_rf <- predict(rf_model, newdata = test, type = "class")
# confusion matrix
table(test$is_canceled, test$pred_rf)

# accuracy
sum(test$is_canceled == test$pred_rf)/nrow(test)

# random forest has the slightly better accuracy than decision tree

