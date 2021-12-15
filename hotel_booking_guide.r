###### LIBRARIES ######
library(shinydashboard)
library(shinythemes)
library(ggthemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(gganimate)
library(babynames)
library(hrbrthemes)
library(magrittr)
library(shinycssloaders)
library(shinycustomloader)

###### INPUT DATA ######
hotel_df <- read.csv('hotel_bookings.csv')
hotel <- read.csv('hotel_bookings.csv')

hotel_df

###### DATA PREPROCESSING ######
hotel_df$arrival_date_month <- factor(hotel_df$arrival_date_month, levels = c("January","February","March", "April",
                                                                              "May", "June", "July", "August", 
                                                                              "September", "October", "November", "December"))
hotel_list <- list("Resort Hotel","City Hotel")

hotel$holiday<-"Non-Holiday"
hotel$holiday[hotel$arrival_date_month=='December'& ((hotel$arrival_date_day_of_month>=21)&(hotel$arrival_date_day_of_month<=28))]<-"Christmas"
hotel$holiday[hotel$arrival_date_month=='July'& ((hotel$arrival_date_day_of_month>=1)&(hotel$arrival_date_day_of_month<=6))]<-"July 4th"
hotel$holiday[hotel$arrival_date_month=='February'& ((hotel$arrival_date_day_of_month>=12)&(hotel$arrival_date_day_of_month<=16))]<-"Valentine's Day"
hotel$holiday[hotel$arrival_date_month=='September'& ((hotel$arrival_date_day_of_month>=1)&(hotel$arrival_date_day_of_month<=8))]<-"Labor Day"
hotel$holiday[hotel$arrival_date_month=='November'& ((hotel$arrival_date_day_of_month>=21)&(hotel$arrival_date_day_of_month<=30))]<-"Thanksgiving Day"
hotel$holiday[hotel$arrival_date_month=='May'& ((hotel$arrival_date_day_of_month>=21)&(hotel$arrival_date_day_of_month<=28))]<-"Memorial Day"
hotel$holiday[(hotel$arrival_date_month=='December'& ((hotel$arrival_date_day_of_month>=29)&(hotel$arrival_date_day_of_month<=31)))|(hotel$arrival_date_month=='January'& ((hotel$arrival_date_day_of_month>=1)&(hotel$arrival_date_day_of_month<=3)))]<-"New Years"

hotel$holiday<-factor(hotel$holiday,levels=c("Christmas","July 4th","Valentine's Day","Labor Day","Thanksgiving Day","Memorial Day","New Years","Non-Holiday"))
summary(hotel$holiday)


#Add in Bookings column
hotel$Bookings <- 1

#Creating a numerical column for month
hotel$month <- as.integer(factor(hotel$arrival_date_month, levels = month.name))

# Group by Month and year and get sum of bookings
hotel_edit <- hotel %>% 
  group_by(month,arrival_date_year)%>% 
  # group_by(month) %>% 
  summarise(Bookings = sum(Bookings))

# Group by Month and year and holiday and get sum of bookings
hotel_edit_2 <- hotel %>% 
  group_by(country,holiday, arrival_date_year)%>% 
  # group_by(month) %>% 
  summarise(Bookings = sum(Bookings),lead_time=mean(lead_time))

#Setting month to numeric just to make sure
hotel_edit$month <- as.numeric(hotel_edit$month)
# Setting y variable: bookings to numeric
hotel_edit$Bookings <- as.numeric(hotel_edit$Bookings)
hotel_edit_2$Bookings <- as.numeric(hotel_edit_2$Bookings)
# Year = setting the variable used to color and split the data to factor so lines are properly drawn
hotel_edit$arrival_date_year <- factor(hotel_edit$arrival_date_year)
#Just renaming year column 
hotel_edit$Year = hotel_edit$arrival_date_year

hotel_edit_2$Year = hotel_edit_2$arrival_date_year


hotel_edit_2 <- hotel_edit_2[hotel_edit_2$holiday != 'Non-Holiday', ] 

hotel_edit_2 <- hotel_edit_2[hotel_edit_2$country == 'USA', ] 

# Group by Month and year and get sum of children
hotel_children_edit <- hotel %>% 
  group_by(month,arrival_date_year)%>% 
  # group_by(month) %>% 
  summarise(children = sum(children))

#Setting month to numeric just to make sure
hotel_children_edit$month <- as.numeric(hotel_children_edit$month)
# Setting y variable: bookings to numeric
hotel_children_edit$children <- as.numeric(hotel_children_edit$children)
# Year = setting the variable used to color and split the data to factor so lines are properly drawn
hotel_children_edit$arrival_date_year <- factor(hotel_children_edit$arrival_date_year)
#Just renaming year column 
hotel_children_edit$Year = hotel_children_edit$arrival_date_year



#formulas for infoBox Max, Min, and Average count of stays in Weekend Nights
s = max(hotel_df$stays_in_weekend_nights)
s_1 = min(hotel_df$stays_in_weekend_nights [hotel_df$stays_in_weekend_nights > 5])
s_2 = mean(hotel_df$stays_in_weekend_nights [hotel_df$stays_in_weekend_nights > 5])

#formulas for infoBox Max, Min, and Average count of stays in Week Nights
m = max(hotel_df$stays_in_week_nights)
m_1 = min(hotel_df$stays_in_week_nights [hotel_df$stays_in_weekend_nights > 6])
m_2 = mean(hotel_df$stays_in_week_nights [hotel_df$stays_in_weekend_nights > 6])


###### UI PAGE ######

ui <- dashboardPage(
  
  ###### DASHBOARD HEADER AND SIDEBAR ######  
  dashboardHeader(title = "Hotel Travel Guide"),
  
  dashboardSidebar(
    width = 310,
    sidebarMenu(
      menuItem("Best Time of Year to Book", tabName = "mainTab1", icon = icon("fas fa-umbrella-beach"), 
               menuSubItem("Bookings by Holidays and Months", tabName = "subTab1",icon = icon("fas fa-gifts")),
               menuSubItem("Kid - Friendly Time of Year", tabName = "subTab2",icon = icon("fas fa-child"))),
      menuItem("Quiet Night in, or Night out on the Town?", tabName = "mainTab2", icon = icon("far fa-moon"), 
               menuSubItem("Hotel Popularity", tabName = "subTab3",icon = icon("fas fa-chart-line")),
               menuSubItem("Resort vs. City Clientele", tabName = "subTab4",icon = icon("fas fa-suitcase-rolling"))),
      menuItem("Last Minute Bookers", tabName = "mainTab3", icon = icon("fas fa-user-clock"), 
               menuSubItem("Cancelations by Hotel Types", tabName = "subTab5",icon = icon("fas fa-business-time")),
               menuSubItem("Cancelations by Month", tabName = "subTab6",icon = icon("far fa-calendar-alt"))),
      menuItem("About", tabName = "mainTab4", icon = icon("fas fa-info-circle"), 
      menuSubItem("Authors and Background Information", tabName = "subTab7",icon = icon("fas fa-users")),
      menuSubItem("Data Source", tabName = "subTab8",icon = icon("fas fa-database")))
    
    )
  ),
  
  
  ###### DASHBOARD BODY ######  
  dashboardBody(
    tabItems(
      tabItem(tabName = "subTab1",
              sidebarPanel(
                # inputs
                conditionalPanel(
                  condition="input.tabselected=='Bookings per Holiday'",
                  selectizeInput("stateInput", "Year",
                                 choices = unique(hotel_edit_2$Year),  
                                 selected="2015", multiple =FALSE),
                )
              ),
              conditionalPanel(
                condition="input.tabselected=='Bookings per Holiday'",
                titlePanel("Hotel Bookings per Holiday (USA)"),
                h5("With Average Lead Times")
                
              ),
              tabsetPanel(
                id = "tabselected",
                tabPanel("Bookings per Holiday", plotOutput("holiday"),
                         box("Above you can see the amount of bookings per Holiday. We included the average lead times on each bar to give you an idea of how far ahead you should be planning to book.
                This tool is great for our travelers that like to plan ahead. But don't worry, we have something for our more spontaneaus traverls too! That information can be found under the Last Minute Bookers Tab.",
                             width = 12)),
                tabPanel("Bookings per Month", verbatimTextOutput("summary")),
              ),
              conditionalPanel(
                actionButton("go", "Show me a Plot!"),
                condition = "input.go <= 0 && input.tabselected=='Bookings per Month'"
              ),
              #shinycssloaders::withSpinner(
              conditionalPanel(
                condition="input.tabselected=='Bookings per Month'",
                titlePanel("Hotel Bookings per Month")
              ),
              conditionalPanel(
                img(src="https://www.hutchgo.com.hk/Content/img/hotel-loading-gif.gif",height="50%", width="50%",class="center"),
                condition = "input.go <= 0 && input.tabselected=='Bookings per Month'"
              ),
              conditionalPanel(
                condition = "input.go > 0 && input.tabselected=='Bookings per Month'",
                style = "display: none;",
                img(src="https://i.ibb.co/6tZmHKB/Racing-Line-Graph-Q1.gif",height="50%", width="50%",class="center"),
                #withLoader(imageOutput("bookings_by_month"), type="image", loader="https://i.ibb.co/XbHMvBY/Racing-Line-Graph-Q1.gif",proxy.height = "100px")
                box("We are excited to let you know the most popular booking times in real-time! This plot will show you when the best time for you and your family to book that much-needed getaway! 
  We are here to make your experience amazing!
  
  Our findings: Most bookings were made in the months of April/May and September/October from 2015-2017. 
  Our advice to you: Planning on having a holiday getaway? We think you should book an average of 4 months prior to your scheduled day of arrival.
  Find out how long in advance people book here! (Link)", width = 12)
              )
      ),
      
      tabItem(tabName = "subTab2",
              h3("For our parent readers, here's some advice on the most kid-friendly times of year to book!"),
              conditionalPanel(
                actionButton("go1", "Show me a Plot!"),
                condition = "input.go1 <= 0"
                # img(src="https://6elrmjmsbs335jwbeybarrry-wpengine.netdna-ssl.com/wp-content/uploads/2021/09/SnowglobeGIF2.gif",height="50%", width="50%",class="center"),
              ),
              conditionalPanel(
                condition = "input.go1 > 0",
                style = "display: none;",
                img(src="https://i.ibb.co/C1d76wN/Children-Line-Graph-Q5.gif",height="50%", width="50%",class="center"),
                #withLoader(imageOutput("children"), type="image", loader="https://i.ibb.co/zNNCMX9/Children-Line-Graph-Q5.gif",proxy.height = "100px")
                box("Looking for the best time to travel with kids? There is a noticeable increase in guest bookings with children in July and August. 
  Our advice to you: Save the date for your next summer vacation for the hottest months of the year!", width = 12)
              )
      ),
      
      tabItem(tabName = "subTab3",
              h2("Hotel popularity based on count of stays"),
              sidebarPanel(
                selectInput(inputId="hotel_2", label="Select the Hotel", choices=hotel_list), width = 3),
              
              mainPanel(
                tabsetPanel(
                  tabPanel("Weekend Nights", plotOutput("plot"), fluidRow(infoBoxOutput("max"), infoBoxOutput("min"), infoBoxOutput("avg"))),
                  tabPanel("Week Nights", plotOutput("plot_2"), fluidRow(infoBoxOutput("max_"),infoBoxOutput("min_"), infoBoxOutput("avg_"))),
                  box("Whether you're  a non-stop party animal, or prefer a quiet getaway, this tool will provide you with all of the answers that you need, feel free to go ahead and self help!",
                      width = 12)
                ))
      ),
      
      tabItem(tabName = "subTab4",
              h2("What kinds of people book at what types of hotel? We answer this for you here!"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("htl_typ1","Select the type of hotel",
                              choices = list("City Hotel",
                                             "Resort Hotel")),
                  box("Listen, we get it. It's your vacation and you deserve to be around the types of people that you want to be around. Go ahead and use our tool, we wont tell a soul.",
                      width = 12)
                ),
                mainPanel(plotOutput("cust_typ_plt"))
              )
              
      ),
      
      tabItem(tabName = "subTab5",
              h2("Which type of hotel do you have a better chance of getting a last minute booking?"),
              radioButtons("htl_typ","Select the Hotel Type",
                           choices = list("City Hotel",
                                          "Resort Hotel")),
              plotOutput("mnth_cncl_plt"),
              box("As you might be able to tell, city hotels have more steady cancelations over several months in the middle of the year.
                     Wheras, Resort hotels have many cancelations in two primary months during the summer. 
                     This means late bookers have a better chance of getting a booking in a City hotel for the majority of the summer, while July and August are ideal for last minute Resort bookings!",
                  width = 12)
              
      ),
      tabItem(tabName = "subTab6",
              h3("More cancelations in the month means you have a greater chance of snagging a last minute booking!"),
              withLoader(plotOutput("cancel_by_month"), type="image", loader="https://i.pinimg.com/originals/c8/a1/76/c8a1765ffc8243f3a7b176c0ca84c3c1.gif",proxy.height = "100px"),
              box("As you can see, mid-months appear to have higher cancelation counts. August is the frontrunner for the most cancelations. 
                  With that being said, you might have a chance at booking a last-minute summer vacacation. Winter months have a dip in cancelations, but summer is where the fun is at anyway!",
                  width = 12)
      ),
      tabItem(tabName = "subTab7",
              h3("About Us: Authors and Background Information",img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/b/b9/UNC_Charlotte_Main_Logo.png/1200px-UNC_Charlotte_Main_Logo.png",height="10%", width="10%",class="center")),
              box("This ShinyApp was a collaborative effort by Evelyn Guidry, Kail Amoakon, Sundari Kala, and Christopher McManus. " , br(), br(), "We are students at UNC Charlotte's School of Data Science in
            Chase Romano's Visual Analytics course. The goal for this ShinyApp was to create a Hotel Travel Guide for those looking to book at two hotels, either the city hotel or resort hotel.", br(), br(),
                 "We provide insights on the best time of year to book, the optimal time to reserve a room for a quiet night, and those looking to book last minute through a collection of graphs.", width = 12)
              ),
    
      tabItem(tabName = "subTab8",
              h3("Data Source & References"),
              box("The dataset used to create this ShinyApp was sourced from Kaggle. This ShinyApp can be recreated by visiting this link to the dataset: ", width = 12,tags$a(href="https://www.kaggle.com/jessemostipak/hotel-booking-demand", "Kaggle Hotel Booking Dataset")))
      
    )
  )
)


###### SERVER FUNCTION ######
server <- function(input, output, session) {
  
  ###### SUBTAB 1/ QUESTION ONE CODE ######
  d <- reactive({
    filtered <-
      hotel_edit_2 %>%
      filter(Year %in% input$stateInput)})
  output$bookings_by_month <- renderImage({
    input$go
    Sys.sleep(1.5)
    plot(runif(10))
    test <- hotel_edit %>%
      ggplot() + 
      geom_point(aes(x = month, y = Bookings, color = Year))+
      geom_line(data = hotel_edit, aes(x = month, y = Bookings, color = Year))+
      scale_x_continuous(breaks = 1:12) +
      theme_economist() +
      ggtitle("Hotel Bookings per Month") +
      xlab("Month") +
      ylab("Bookings") +
      transition_reveal(month)
    #Saving as gif
    anim_save("RacingLineGraph_Q1.gif",animate(test))
    
    # Return a list containing the filename
    list(src = "RacingLineGraph_Q1.gif", contentType = "image/gif")
    
  },deleteFile = TRUE)
  output$holiday <- renderPlot({
    ggplot(d(), aes(x=holiday, y=Bookings, fill=holiday)) +
      geom_bar(stat='identity', position='dodge')+
      theme_economist() +
      xlab("Holiday") +
      scale_fill_brewer(direction = -1, palette = "Blues", name= "Holiday")+
      geom_text(aes(x=holiday, y=Bookings, fill=holiday,label=round(lead_time,0)),vjust = -0.35,size=4)
    #geom_text(aes(label=Freq),position = position_dodge(width = 1),hjust = -0.3,size=3)
    
  })
  
  ###### SUBTAB 2/ QUESTION FIVE(SEVEN) CODE ######
  output$children <- renderImage({
    input$go1
    Sys.sleep(1.5)
    plot(runif(10))
    children_plot <- hotel_children_edit %>%
      ggplot() + 
      geom_point(aes(x = month, y = children, color = Year))+
      geom_line(data = hotel_children_edit, aes(x = month, y = children, color = Year))+
      scale_x_continuous(breaks = 1:12)+
      theme_economist() +
      xlab("Month") +
      ylab("Children") +
      transition_reveal(month)
    
    #Saving as gif
    anim_save("ChildrenLineGraph_Q5.gif",animate(children_plot))
    
    # Return a list containing the filename
    list(src = "ChildrenLineGraph_Q5.gif", contentType = "image/gif")
    
  },deleteFile = TRUE)
  ###### SUBTAB 3/ QUESTION THREE CODE ######
  htl_type <- reactive({filter(hotel_df, hotel==input$hotel_2)})
  
  output$plot <- renderPlot({
    ggplot(htl_type(), aes(x=stays_in_weekend_nights, y=arrival_date_month)) +
      geom_bar(stat="identity", position="dodge", fill = "#00A8A8") + labs(x="Count of stays during Weekend", y="Check-in Month") +
      theme_economist() + 
      theme(#panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + coord_flip()
  })
  
  output$plot_2 <- renderPlot({
    ggplot(htl_type(), aes(x=stays_in_week_nights, y=arrival_date_month)) +
      geom_bar(stat="identity", position="dodge", fill = "#00A8A8") + labs(x="Count of stays during Week", y="Check-in Month") +
      theme_economist() +
      theme(#panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + coord_flip()
  })
  
  output$max <- renderInfoBox({
    infoBox(title= "Max # of weekend night stays",
            value= s)
    
  })
  
  output$min <- renderInfoBox({
    infoBox(title= "Min # of weekend night stays",
            value= s_1)
    
  })
  
  output$avg <- renderInfoBox({
    infoBox(title= "Average # of week night stays",
            value= round(s_2, digits=1))
    
  })
  
  output$max_ <- renderInfoBox({
    infoBox(title= "Max # of week night stays",
            value= m)
    
  })
  
  output$min_ <- renderInfoBox({
    infoBox(title= "Min # of week night stays",
            value= m_1)
    
  })
  
  output$avg_ <- renderInfoBox({
    infoBox(title= "Average # of week night stays",
            value= round(m_2, digits=1))
    
  })
  
  
  
  ###### SUBTAB 4/ QUESTION FOUR CODE ######
  cust_typ <- reactive({
    filter(hotel_df,hotel==input$htl_typ1) %>% 
      group_by(customer_type) %>% 
      summarise(n=n())
  })
  output$cust_typ_plt <- renderPlot({
    ggplot(cust_typ(),aes(customer_type,n,fill=customer_type)) + 
      geom_bar(stat="identity") + 
      labs(x="", y="Count") + 
      theme_economist() + 
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 90)
            #panel.background = element_rect(fill="white"),
            #panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank())
      ) +
      guides(fill=guide_legend(title="Customer Type"))
  }) 
  
  
  
  ###### SUBTAB 5/ QUESTION TWO CODE ######  
  mnth_cncl <- reactive({
    filter(hotel_df,hotel==input$htl_typ & reservation_status=='Canceled') %>% 
      group_by(arrival_date_month) %>% 
      summarise(n=n())  %>% 
      mutate(prptn = n/sum(n) * 100)
  })
  output$mnth_cncl_plt <- renderPlot({
    ggplot(mnth_cncl(),aes(arrival_date_month, prptn ,fill=arrival_date_month)) + #removed "fill=pal"
      geom_bar(stat='identity', position = position_dodge(), fill = "#00A8A8") +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 90)) +
      #legend.position = "top") +
      #panel.background = element_rect(color="black",fill="white"),
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank()) +
      labs(x="Months",y="Percent")
  })
  
  
  
  ###### SUBTAB 6/ QUESTION SIX COD ######  
  output$cancel_by_month <- renderPlot({
    ggplot(hotel_df, aes(x=arrival_date_month, y=is_canceled)) + 
      geom_bar(stat = 'identity', color = "#00A8A8") +
      theme_economist() +
      labs(x="Month",y="Cancelation count")
  })
  
}

shinyApp(ui, server)