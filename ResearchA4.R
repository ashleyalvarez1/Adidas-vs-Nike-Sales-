library(tidyverse)


library(readr)
StockX_Data_Contest_2019_3 <- read_csv("Downloads/StockX-Data-Contest-2019-3.csv")
View(StockX_Data_Contest_2019_3)


library(readr)
Adidas_Vs_Nike <- read_csv("Downloads/Adidas Vs Nike.csv")
View(Adidas_Vs_Nike)

summary(Adidas_Vs_Nike)


#code to remove varibles I am not using in my data
library(dplyr)
Adidas_Vs_Nike <- Adidas_Vs_Nike%>% select(-`Product ID`,- 'Reviews')

#I will start off with Adidas Vs Nike, as it is a supplemtal Data for my actual data set, Stockx Dta Contest

###name of variable (categorical)
#Product NAME 
#step 1: Convert to factor variable
Adidas_Vs_Nike = Adidas_Vs_Nike %>%
  mutate(`Product Name`= factor(`Product Name`))


#step 2: Show levels of the factor variables

levels(Adidas_Vs_Nike$`Product Name`)
summary(Adidas_Vs_Nike$`Product Name`)
#reduce number of levels when there is no valid answer given or combining levels
# Im not sure how to reduce to categories because there are so many levels I just reduced to number 
#of levels. 
Adidas_Vs_Nike$`Product Name` = fct_lump(Adidas_Vs_Nike$`Product Name`, n = 1531, other_level = "other")

#step 4: Create a frequency table of the levels

table(Adidas_Vs_Nike$`Product Name`)


###name of variable (categorical)
# Brand in Adidas bs Nike1
#step 1: Convert to factor variable
Adidas_Vs_Nike = Adidas_Vs_Nike %>%
  mutate(Brand= factor(Brand))

#step 2: Show levels of the factor variable
levels(Adidas_Vs_Nike$Brand)

#step 3: Reduce the number of levels (no more than 4-5 levels) by either removing
#levels when there is no valid answer given or combining levels

Adidas_Vs_Nike$Brand = factor(Adidas_Vs_Nike$Brand, levels = c("Adidas Adidas ORIGINALS",  "Adidas CORE / NEO", "Adidas ORIGINALS" ,"Adidas SPORT PERFORMANCE",
                                                               "Nike"))

#step 4: Create a frequency table of the levels
table(Adidas_Vs_Nike$Brand)

#I will leave description alone as again it is needed for each unique shoe. 

#name of variable (quantitative)
#Listing Price 
##step 1: convert to numeric, date, or logical (whichever is the most appropriate). You
#can create additional variables if you have a date/time object (time of day, day of week,

#I needed it add a decimal place in order to correctly calcuate mean median etc 
Adidas_Vs_Nike <- Adidas_Vs_Nike %>%
  mutate(`Listing Price` = `Listing Price` / 100)

## step 2: create a summary table of the variable (min, max, mean, median, n
summary(Adidas_Vs_Nike$`Listing Price`)


#name of variable (quantitative)
#Sale Price
##step 1: convert to numeric, date, or logical (whichever is the most appropriate). You
#can create additional variables if you have a date/time object (time of day, day of week,
Adidas_Vs_Nike <- Adidas_Vs_Nike %>%
  mutate(`Sale Price` = `Sale Price`/100)

## step 2: create a summary table of the variable (min, max, mean, median, n)
summary(Adidas_Vs_Nike$`Sale Price`)

#name of variable (quantitative)
#Rating
##step 1: convert to numeric, date, or logical (whichever is the most appropriate). You
#can create additional variables if you have a date/time object (time of day, day of week,
#the variable rate is already in numeric form

## step 2: create a summary table of the variable (min, max, mean, median,
summary(Adidas_Vs_Nike$Rating)

#I will now be using my main data set : Stockx Contest


#name of variable(quantitative)
#Order Date
#Step1 for numeric data,
#I wanted to separate the day month and year too do separate analyis 
StockX_Data_Contest_2019_3 = StockX_Data_Contest_2019_3%>%
  separate(col = `Order Date`,
           into = c("Order_month","Order_day","Order_year"),
           sep = "/",
           convert = TRUE)

#will do the same for Release Date


StockX_Data_Contest_2019_3 = StockX_Data_Contest_2019_3%>%
  separate(col = `Release Date`,
           into = c("release_month","release_day","release_year"),
           sep = "/",
           convert = TRUE)

#name of variable (quantitative)
#Shoe Size
##step 1: convert to numeric, date, or logical (whichever is the most appropriate). You
#can create additional variables if you have a date/time object (time of day, day of week,
#is already numeric                                                                
## step 2: create a summary table of the variable (min, max, mean, median, n)
summary(StockX_Data_Contest_2019_3$`Shoe Size`)





#name of variable (quantitative)
#Sale Price
##step 1: convert to numeric, date, or logical (whichever is the most appropriate). You
#can create additional variables if you have a date/time object (time of day, day of week,
#is already numeric  
#name of variable (quantitative)
StockX_Data_Contest_2019_3 <- StockX_Data_Contest_2019_3 %>%
  mutate(`Sale Price` = as.numeric(gsub("[^0-9.]", "", `Sale Price`), na.rm = TRUE))


## step 2: create a summary table of the variable (min, max, mean, median, n)

summary(StockX_Data_Contest_2019_3$`Sale Price`)

#name of variable (quantitative)
#Retail Price
##step 1: convert to numeric, date, or logical (whichever is the most appropriate). You
#can create additional variables if you have a date/time object (time of day, day of week,
#is already numeric  
StockX_Data_Contest_2019_3 <- StockX_Data_Contest_2019_3 %>%
  mutate(`Retail Price` = as.numeric(gsub("[^0-9.]", "", `Retail Price`), na.rm = TRUE))


## step 2: create a summary table of the variable (min, max, mean, median, n)
summary(StockX_Data_Contest_2019_3$`Retail Price`)


###name of variable (categorical)
#Brand
#step 1: Convert to factor variable
StockX_Data_Contest_2019_3 = StockX_Data_Contest_2019_3 %>%
  mutate(Brand= factor(Brand))

#step 2: Show levels of the factor variable
levels(StockX_Data_Contest_2019_3$Brand)
#step 3: Reduce the number of levels (no more than 4-5 levels) by either removing
#levels when there is no valid answer given or combining levels
#no need to reduce the levels since there is only 2
#step 4: Create a frequency table of the level
table(StockX_Data_Contest_2019_3$Brand)


###name of variable (categorical)
#Sneaker Name
#step1 : convert to factor varialbe

StockX_Data_Contest_2019_3 = StockX_Data_Contest_2019_3%>%
  mutate(`Sneaker Name` = factor(`Sneaker Name`) )


#step 2: Show levels of the factor variable
levels(StockX_Data_Contest_2019_3$`Sneaker Name`)


library(dplyr)
library(forcats)




#step 4: Create a frequency table of the level
Stable = table(StockX_Data_Contest_2019_3$`Sneaker Name`)
View(Stable)

# Geography (county, state) or names: report the number of levels instead of all the
StockX_Data_Contest_2019_3 = StockX_Data_Contest_2019_3%>% mutate(`Buyer Region`=factor(`Buyer Region`))
levels(StockX_Data_Contest_2019_3$`Buyer Region`)
nlevels(StockX_Data_Contest_2019_3$`Buyer Region`)

table(StockX_Data_Contest_2019_3$`Buyer Region`)


#CLEAN DATA ON TOP 
#NOW TO VISUALIZATIONS

#with my first analaysis i want to see the a line graph of when an increase in s



daily_sales = StockX_Data_Contest_2019_3%>%
  group_by(Brand,Order_month,Order_day, Order_year)%>%
  summarise(TotalSales = sum(`Sale Price`))

#I had to change the format of year in order to get a proper x value to show the trend
#used google and found the paste0 is to add 00 to my date so R can read it 
daily_sales$Order_year = ifelse(as.numeric(daily_sales$Order_year) < 50, 
                              paste0("20", daily_sales$Order_year), 
                              paste0("19", daily_sales$Order_year))

# Created a new column named "OrderDate" representing the full date
daily_sales <- mutate(daily_sales, OrderDate = as.Date(paste(Order_year, Order_month, Order_day, sep = "-")))


#visualization1
#ploting the line graph 
ggplot(daily_sales, aes(x = OrderDate, y = TotalSales, color = Brand)) +
  geom_line() +
  labs(title = "Nike and Adidas Sales Over Time",
       x = "Date",
       y = "Total Sales",
       color = "Brand")


library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

#got this from slides on Lecture 13
US_States_Tigris<-states()
only_states <- US_States_Tigris%>%
  filter(REGION != "9") %>%
  shift_geometry()

#making a data frame grouping buyer region , and making sepater columsn of off white count
#and yeezy count
#chatgpt help 
brandbystate = StockX_Data_Contest_2019_3%>%
  mutate(White = as.numeric(Brand == "Off-White"),
         Yeezy = as.numeric(Brand == "Yeezy")) %>%
  group_by(`Buyer Region`) %>%
  summarise(OffWhite_Count = sum(White),
            Yeezy_Count = sum(Yeezy))

#joining tigris data and main data set to map 
join_data = left_join(
  only_states,
  brandbystate,
  by = c("NAME" = "Buyer Region"))

library(tigris)
library(dplyr)
library(ggplot2)

#visualization for count of OFF White 
ggplot(join_data, aes(fill = OffWhite_Count )) +
  geom_sf() +
  scale_fill_viridis_c(name = "Total Counts", label = scales::comma) +
  labs(title = "Counts of OFF Whites in US States") +
  theme_minimal()


#visualization for count of Yeezy
ggplot(join_data, aes(fill = Yeezy_Count )) +
  geom_sf() +
  scale_fill_viridis_c(name = "Total Counts", label = scales::comma) +
  labs(title = "Counts of Yeezy in US States") +
  theme_minimal()



#Visualization3 bar graph of avg rating per adidas vs nike shoe to compare with stockx data set
ggplot(Adidas_Vs_Nike, aes(x = Brand, y = Rating, fill = Brand)) +
    geom_bar(stat = "summary", fun = "mean", position = "dodge") +
     labs(title = "Average Ratings for Adidas vs Nike", 
                  x = "Brand", 
                   y = "Average Rating") +
    scale_fill_manual(values = c("Adidas Adidas ORIGINALS" = "blue", "Adidas CORE / NEO" = "white", "Adidas ORIGINALS" = "green","Adidas SPORT PERFORMANCE" = "red", "Nike" = "orange")) 

   
#extracting yeezy so they are able to distiniguh color ways 
StockX_Data_Contest_2019_3 = StockX_Data_Contest_2019_3%>%
  mutate(colorway = str_extract(`Sneaker Name`, "(?<=Yeezy-).+"))
#Visualization4 density plot showing what yeezy shoe is more popular along the data
 
options(repr.plot.width=10, repr.plot.height=6)

ggplot(StockX_Data_Contest_2019_3, aes(x = as.factor(colorway), fill = colorway)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Yeezy Colorways", 
       x = "Yeezy Colorway", 
       y = "Density") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.5)) +
  facet_wrap(~ colorway, scales = "free_y", ncol = 2)


#Visualization 5 
off_white_data <- StockX_Data_Contest_2019_3 %>%
  filter(Brand == "Off-White")
#extracting nike and air so the new data can make a new varialbe with all models   
off_white_data = off_white_data%>%
  mutate(modeltype = str_extract(`Sneaker Name`, "(?<=Nike-).+"), 
         modeltype = str_extract(`Sneaker Name`, "(?<=Air-).+"))
#same visualzation for OFF WHITE 
 ggplot(off_white_data, aes(x = modeltype, fill = modeltype))+
   geom_density( alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Off-White Sneaker Colorways", 
       x = "Off-White Sneaker Colorway", 
       y = "Count") +
  theme(axis.text.x = element_blank(),  # Hide x-axis text for better readability
        axis.ticks.x = element_blank())+
 facet_wrap(~ modeltype, scales = "free_y", ncol = 2)
#text cloud 
library(tidytext) 
 library(textdata)
 library(wordcloud)
 library(RColorBrewer)
library(SnowballC)
 
 #cleaning word data from stop words and unnesting it to read word by word
 tidy_adidas_vs_nike = Adidas_Vs_Nike %>%
   unnest_tokens(output = word, input =Description)
#WORD CLOUD FOR NIKE  
 tidy_adidas_vs_nike_remove_stop = anti_join(x = tidy_adidas_vs_nike, y = stop_words)
tidy_adidas_vs_nike_remove_stop %>%
  filter(Brand =="Nike")%>%
  count(word)%>%
  arrange(desc(n))%>%
  slice_head(n=75)%>%
  with(wordcloud(words = word, freq = n))
str(StockX_Data_Contest_2019_3)


#word cloud for Adidas 
tidy_adidas_vs_nike = Adidas_Vs_Nike %>%
  unnest_tokens(output = word, input =Description)
#WORD CLOUD FOR NIKE  
tidy_adidas_vs_nike_remove_stop = anti_join(x = tidy_adidas_vs_nike, y = stop_words)
tidy_adidas_vs_nike_remove_stop %>%
  filter(Brand == c("Adidas Adidas ORIGINALS", "Adidas ORIGINAL","Adidas CORE / NEO","Adidas SPORT PERFORMANCE"))%>%
  count(word)%>%
  arrange(desc(n))%>%
  slice_head(n=75)%>%
  with(wordcloud(words = word, freq = n))
str(StockX_Data_Contest_2019_3)







#BAR graph shoe size 

# Convert 'Shoe Size' to a factor
StockX_Data_Contest_2019_3$`Shoe Size` <- as.factor(StockX_Data_Contest_2019_3$`Shoe Size`)

library(ggplot2)

# Convert 'Shoe Size' to numeric
StockX_Data_Contest_2019_3$`Shoe Size` <- as.numeric(as.character(StockX_Data_Contest_2019_3$`Shoe Size`))


# Creating a bar plot with conditional color PINK WOMEN MEN BLUE in Stockx
#since they do not have specifiactin of men or women 
#i told R if the shoe size is less than or = to 7, then it will be pink to represent women 
ggplot(StockX_Data_Contest_2019_3, aes(x = `Shoe Size`, fill = `Shoe Size` <= 7)) +
  geom_bar() +
  scale_fill_manual(values = c("TRUE" = "pink", "FALSE" = "blue")) 
  


#creating a bar plot for men and women in Adidas vs Nike 
#using detect to pick up the word Womena and Men in the dataframe 
Adidas_Vs_Nike = Adidas_Vs_Nike %>%
  mutate(
    gender = case_when(
      str_detect(`Product Name`, "(?i)Women") ~ "Women",
      str_detect(`Product Name`, "(?i)Men") ~ "Men",
      TRUE ~ "Unisex"))

ggplot(Adidas_Vs_Nike, aes(x = gender, fill = gender)) +
  geom_bar() +
scale_fill_manual(values = c("Men" = "blue", "Women" = "pink", "Unisex" = "gray"))+
  theme_minimal()




