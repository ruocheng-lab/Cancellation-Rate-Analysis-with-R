ggplot(hotel_df_v1, aes(x=is_canceled, fill=is_canceled)) +
  geom_bar() +
  labs(x='Cancellation Status', y='Count') +
  scale_fill_manual(values=c("#F4511E", "#7D3C98")) +
  theme_minimal()



# Create a summary of counts for is_canceled
cancellation_counts <- as.data.frame(table(hotel_df_v1$is_canceled))
print(colnames(cancellation_counts))

# Calculate percentage for each group
cancellation_counts$Percentage <- cancellation_counts$Freq / sum(cancellation_counts$Freq) * 100

# Create the pie chart with percentages
ggplot(cancellation_counts, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#F4511E", "#7D3C98")) +
  labs(fill = "Cancellation Status") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))








#number of Resort Hotel
Resort <- sum(hotel_df_v1$hotel == 'Resort Hotel')
Resort #40059
#uncanceled Resort Hotel
R_u <- hotel_df_v1[hotel == 'Resort Hotel'& is_canceled == 0]
#cancel rate of Resort Hotel
nrow(R_u) #28938
length(R_u) #25
a <- (Resort - nrow(R_u))/Resort
a #0.2776335
#number of City Hotel
City <- sum(hotel_df_v1$hotel == 'City Hotel')
City #79326
#uncanceled City Hotel
C_u <- hotel_df_v1[hotel == 'City Hotel'& is_canceled == 0]
#cancel rate of City Hotel
b <- (City - nrow(C_u))/City
b #0.4172696

cancellation_data <- data.frame(
  Hotel = c("Resort Hotel", "City Hotel"),
  Cancel_Rate = c(a, b)
)

ggplot(cancellation_data, aes(x = Hotel, y = Cancel_Rate, fill = Hotel)) +
  geom_bar(stat = "identity", width = 0.5) + 
  scale_fill_manual(values = c("#F4511E", "#7D3C98")) +
  labs(title = "Cancellation Rate for Resort vs City Hotel",
       x = "Hotel Type",
       y = "Cancellation Rate") +
  geom_text(aes(label = round(Cancel_Rate, 4)), 
            position = position_stack(vjust = 0.5),
            size = 5) + 
  theme_minimal() 





#Monthly Figure

# figure of arrival in dff month


month <- hotel_df_v1 %>%
  group_by(arrival_date_month) %>%
  summarise(count = n())

print(month)

month$arrival_date_month <- factor(month$arrival_date_month, 
                                   levels = c("January", "February", "March", "April", "May", "June", "July", 
                                              "August", "September", "October", "November", "December"))

ggplot(month, aes(x = arrival_date_month, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Arrival in Diff Month", x = "Month", y = "Amount")

# Cancellation rate in diff month

cancellation_rates <- hotel_df_v1 %>%
  group_by(arrival_date_year, arrival_date_month) %>%
  summarise(
    total_bookings = n(),
    canceled_bookings = sum(is_canceled),
    cancellation_rate = canceled_bookings / total_bookings,
    .groups = 'drop' 
  )

hotel_df_v1$arrival_date_month <- factor(hotel_df_v1$arrival_date_month, 
                                                levels = c("January", "February", "March", "April", "May", "June", "July", 
                                                           "August", "September", "October", "November", "December"))

hotel_df_v1$arrival_date_year <- factor(hotel_df_v1$arrival_date_year, 
                                               levels = c("2015", "2016", "2017"))


ggplot(hotel_df_v1, aes(x = arrival_date_month, y = cancellation_rate, fill = arrival_date_month)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") + 
  theme_minimal() +
  labs(title = "Cancellation Rate by Month", x = "Month", y = "Cancellation Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









ggplot(cancellation_rates, aes(x = arrival_date_month, y = cancellation_rate, fill = as.factor(arrival_date_year))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Cancellation Rate in Different Months and Years", x = "Month", y = "Cancellation Rate") +
  scale_fill_brewer(palette = "Pastel1") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) 


