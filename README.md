# Shinyapp Project R
![](https://github.com/skala2/Hotel-Bookings/blob/main/www/R_studio.png)

[Shinyapp](https://sundariuncc.shinyapps.io/Hotel_booking_guide/) website.

# 1.	Project Overview:
![](https://github.com/skala2/Hotel-Bookings/blob/main/www/hotel.jpeg)
 
The goal of the project is to predict the count of booking cancellations and peak time of bookings in which month of the year. The data set contains booking information for a city hotel and a resort hotel, and includes information such as when the booking was made, the number of adults, children, and/or babies, and the number of available parking spaces, among other things. From this, we can understand the customer’s behavior and it might help us to take better decisions.  

The process of our analysis will be: Understanding the Datasets, Analyzing, and visualizing the data. 

Our goal is to predict type of customers who prefers to stay either in the city style or the resort style, and to predict the possibility of monthly cancellations. We also performed time series analysis to forecast the number of monthly bookings.


# 2.Data Source :
![Data in github](https://github.com/skala2/Hotel-Bookings/blob/main/data/hotel_bookings.csv)
Data was collected from the [Kaggle](https://www.kaggle.com/jessemostipak/hotel-booking-demand) website.
This data set contains booking information for a city hotel and a resort hotel, and includes information such as when the booking was made, length of stay, the number of adults, children, and/or babies, and the number of available parking spaces, among other things.
The information is collected from hotels in different countries.

# 3.	Business Understanding:

My goal for this project is to identify the type of hotel chossen by different customer type. The count of customers, number of cancellations and the maximum number of bookings monthly. The best time and the best type of hotel to chosse for parents with children. This will help the customers to make better decisions while planning travel.

# 4. Data Understanding:

R libraries used: shinydashboard, shinythemes, ggthemes, shiny, ggplot2, dplyr, RColorBrewer, gganimate, babynames, hrbrthemes, magrittr, shinycssloaders, shinycustliader.

The data set contains 119390 rows and 32 columns.

# 5. Analyzing, and visualizing the data:

The hotel data for united states is filtered to plot the number of bookings per holidays and number of bookings per month. This visualization is plotted under Best time of Year to Book - Bookings by Holidays and Months.
The racing line chart is used to visualize the best time of a year for parents to make hotel bookings. This is under Best time of Year to Book - Kid friendly time of year.
The Best to book the hotel either the night of a week or weekend. This is under Quiet Night in, or Night out on the town? - Hotel popularity tab.
The hotel type chossen by different type of customers. It is plotted as barchart and one can find it under Quiet Night in, or Night out on the town? - Resort vs. City Clientele.
For the people who opt to book hotel at the last minute. They can find their best chances by month in a bar chart visualization under Last minute Bookers tab.

