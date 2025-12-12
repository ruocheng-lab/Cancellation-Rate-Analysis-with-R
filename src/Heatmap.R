#Generating PNG heatmap

png(file = "larger_heatmap.png", width = 1200, height = 1200, res = 120)
numerical_data <- hotel_df_v1 %>% select(where(is.numeric))
cor_matrix <- cor(numerical_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45,  # Text label color and rotation
         tl.cex = 1, #font size
         diag = FALSE)
#Save the file
dev.off()


if (!require(corrplot)) install.packages("corrplot")
if (!require(dplyr)) install.packages("dplyr")

library(corrplot)
library(dplyr)



png(file = "larger_heatmap.png", width = 1200, height = 1200, res = 120)
data_for_corr <- hotel_df_v1 %>%
  select(is_canceled, lead_time, stays_in_weekend_nights, stays_in_week_nights, 
         adults, children, babies, previous_cancellations, previous_bookings_not_canceled,
         booking_changes, days_in_waiting_list, adr, required_car_parking_spaces, 
         total_of_special_requests) %>%
  na.omit()  
str(data_for_corr) #is_canceled is not numeric, we need to transfer it
# Converting 'is_canceled' from factor to numeric
data_for_corr$is_canceled <- as.numeric(as.character(data_for_corr$is_canceled)) - 1
cor_matrix <- cor(data_for_corr)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45,  # Text label color and rotation
         tl.cex = 1, #font size
         diag = FALSE)
#Save the file
dev.off()

#larger font

png(file = "larger_heatmap with data2.png", width = 1200, height = 1200, res = 120)
data_for_corr <- hotel_df_v1 %>%
  select(is_canceled, lead_time, stays_in_weekend_nights, stays_in_week_nights, 
         adults, children, babies, previous_cancellations, previous_bookings_not_canceled,
         booking_changes, days_in_waiting_list, adr, required_car_parking_spaces, 
         total_of_special_requests) %>%
  na.omit()  
str(data_for_corr) #is_canceled is not numeric, we need to transfer it
# Converting 'is_canceled' from factor to numeric
data_for_corr$is_canceled <- as.numeric(as.character(data_for_corr$is_canceled)) - 1

cor_matrix <- cor(data_for_corr)

corrplot(cor_matrix, method="color", type="upper", order="hclust",
         tl.col="black", tl.srt=45, addCoef.col="black", 
         number.cex=0.6, # font size
         number.digits=2, # float digits
         number.format=".2f", 
         diag=FALSE) # whether to use diag
dev.off() 

#Smaller Heatmap if needed

data_selected <- hotel_df_v1 %>% 
  select(is_canceled, lead_time, total_of_special_requests,
         required_car_parking_spaces, booking_changes, previous_cancellations)


data_selected$is_canceled <- as.numeric(as.character(data_selected$is_canceled)) - 1


cor_matrix_selected <- cor(data_selected, use = "complete.obs")

# Create and display the correlation matrix heatmap
png("selected_correlation_matrix.png", width=1200, height=1200, res = 120)
corrplot(cor_matrix_selected, method = "color",
         type = "upper", # Display the upper triangle of the matrix
         order = "hclust", # Order variables by hierarchical clustering
         tl.col = "black", # Set the color for labels
         tl.srt = 45, # Set the rotation for labels
         tl.cex = 1,
         addCoef.col = "black", # Set the color for coefficient numbers
         number.cex = 0.7,
         diag=FALSE) # Set the size for coefficient numbers
dev.off() # Close the graphics device

#larger font
png("selected_correlation_matrix.png", width=1200, height=1200, res = 120)
corrplot(cor_matrix_selected, method="color", type="upper", order="hclust", 
         tl.col="black", tl.srt=45, addCoef.col="black",  
         number.cex=1.5, # font size 
         number.digits=4, # float digits 
         number.format=".2f",  
         diag=FALSE) # whether to use diag 
dev.off() 



#try again larger font
png("selected_correlation_matrix.png", width=1200, height=1200, res = 120)
corrplot(cor_matrix_selected, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45,  # Text label color and rotation
         tl.cex = 1.5, #font size
         diag = FALSE)
dev.off() 