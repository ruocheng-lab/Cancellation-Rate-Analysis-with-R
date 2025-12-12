library(ggplot2)

# Histogram
ggplot(hotel_df_v1, aes(x=lead_time)) + geom_histogram(binwidth=10, fill="lightblue", color="black")

# Box Plot
ggplot(hotel_df_v1, aes(x=is_canceled, y=adr, fill=is_canceled)) + geom_boxplot()

# Stacked Bar Chart
ggplot(hotel_df_v1, aes(x=meal, fill=is_canceled)) + geom_bar(position="fill")

# Time Series Plot
hotel_df_v1$date <- as.Date(paste(hotel_df_v1$arrival_date_year, hotel_df_v1$arrival_date_month, hotel_df_v1$arrival_date_day_of_month, sep="-"))
ggplot(hotel_df_v1, aes(x=date, group=is_canceled, fill=is_canceled)) + geom_line(stat="count")

# Mosaic Plot
library(vcd)
mosaic(~ is_canceled + customer_type, data = hotel_df_v1)

# Scatterplot Matrix
library(GGally)
ggpairs(hotel_df_v1[, c("lead_time", "adr", "stays_in_weekend_nights", "total_of_special_requests")])


hotel_long_df_v1 <- reshape2::melt(hotel_df_v1)



ggplot(hotel_long_df_v1, aes(x = value)) + 
  geom_histogram(bins = 50, fill = "#F9A04D", color = "black") + 
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),
        strip.text.x = element_text(size = 16))
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

