# in this page I will do practice for data analysis in R
# I will Use a data data I will download from link 

#Load Libraries and Data 
##Load tidyverse
library(tidyverse)
# url where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz" #save this link as url

# download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# untar the file so we can get the csv only
# if you run this on your local machine, then can remove tar = "internal" 
untar("lax_to_jfk.tar.gz", tar = "internal") #untar means unzip

# read_csv only 
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(), 
                                         'DivArrDelay' = col_number()))
  
#sub_airline is a variable name that will store the data read from the CSV file.
#read_csv is a function in R provided by the "readr" package that reads data from CSV files.
#lax_to_jfk/lax_to_jfk.csv" is the path to the CSV file to be read. 
#This assumes that the file is located in a folder named "lax_to_jfk".
#col_types is an argument used to specify the data types of specific columns in the CSV file.
#cols() is a function used to define the column types.
#'DivDistance' = col_number() indicates that the column named 
#'"DivDistance" should be treated as numeric (number) data.
#'DivArrDelay' = col_number() indicates that the column named 
#'"DivArrDelay" should be treated as numeric (number) data.


#Missing Values and Formating
head(sub_airline) #shows first 6 rows in the data 
##identify missing values 
#2 ways to detect missing Data 
is.na(sub_airline) #shows TRUE or FALSE for each colomn #NA(Not Availible) means missing value
anyNA(sub_airline) # TRUE or FALSE as general
# counting missing values
sub_airline %>%
  summarize(count = sum(is.na(CarrierDelay))) #shows how many missing values in the column 
sub_airline %>% 
  summarize(count = (sum(is.na(.)))) # . shows all the columns 
map(sub_airline, ~sum(is.na(.))) #shows each column and number of missing data
# Check dimensions of the dataset
dim(sub_airline)


#handling with missing data

#Drop data
#a. Drop the whole column
#b. Drop the whole row

#Replace data
#a. Replace it by mean
#b. Replace it by frequency
#c. Replace it based on other functions

#Drop the whole column
drop_na_cols <- sub_airline %>% select(-DivDistance, -DivArrDelay) #select(-DivDistance, -DivArrDelay) 
#is an operation that removes two columns from the dataset: "DivDistance" and "DivArrDelay".

dim(drop_na_cols)
head(drop_na_cols)

# Drop the missing values
drop_na_rows <- drop_na_cols %>% drop_na(CarrierDelay) #drop_na(CarrierDelay) 
#is an operation that removes rows from the dataset where the value in the "CarrierDelay" column is missing or NA.

dim(drop_na_rows)
head(drop_na_rows)

#Convert NA to 0

# Replace the missing values in five columns
replace_na <- drop_na_rows %>% replace_na(list(CarrierDelay = 0,
                                               WeatherDelay = 0,
                                               NASDelay = 0,
                                               SecurityDelay = 0,
                                               LateAircraftDelay = 0))
head(replace_na)
view(replace_na)

#Correct data format

sub_airline %>% 
  summarize_all(class) %>% 
  gather(variable, class)
#summarize_all(class) is an operation that computes the class (data type) of each column in the dataset. 
#This is done to determine the data type of each column.
#gather(variable, class) is an operation used to reshape the summarized data. 
#It transforms the summarized results into a long format where each row represents a column from the original dataset. 
#The "variable" column will contain the column names, and the "class" column will contain the corresponding data types.


date_airline <- replace_na %>% 
  separate(FlightDate, sep = "-", into = c("year", "month", "day")) #makes 3 different columns for date

head(date_airline)

date_airline %>%
  select(year, month, day) %>%
  mutate_all(type.convert) %>%
  mutate_if(is.character, as.numeric)
date_airline %>% summarize_all(class)


#Data Normalization

#Normalization is the process of transforming values of several features (variables) into a similar range.
#There are different types of ways to normalize:
  
  #Simple scaling: divides each value by the maximum value in a feature. The new range is between 0 and 1.
#Xnew = Xold/Xmax
simple_scale <- sub_airline$ArrDelay / max(sub_airline$ArrDelay)
head(simple_scale)

#Min-max: subtracts the minimum value from the original and divides by the maximum minus the minimum. The minimum becomes 0 and the maximum becomes 1.
#Xnew = Xold − Xmin / Xmax - Xmin
minmax_scale <- (sub_airline$ArrDelay - min(sub_airline$ArrDelay)) /
  (max(sub_airline$ArrDelay) - min(sub_airline$ArrDelay))
head(minmax_scale)

#Standardization (Z-score): subtract the mean (𝜇) of the feature and divide by the standard deviation (𝜎).
#Xnew = Xold - 𝜇/𝜎
z_scale <- (sub_airline$ArrDelay - mean(sub_airline$ArrDelay)) / sd(sub_airline$ArrDelay)
head(z_scale)


#Binning is a process of transforming continuous numerical variables into discrete categorical 'bins', for grouped analysis.

ggplot(data = sub_airline, mapping = aes(x = ArrDelay)) +
  geom_histogram(bins = 100, color = "white", fill = "red") +
  coord_cartesian(xlim = c(-73, 682))

binning <- sub_airline %>%
  mutate(quantile_rank = ntile(sub_airline$ArrDelay,4))

head(binning)


sub_airline %>%
  mutate(dummy = 1) %>% # column with single value
  spread(
    key = Reporting_Airline, # column to spread
    value = dummy,
    fill = 0) %>%

sub_airline %>% 
  spread(Reporting_Airline, ArrDelay) %>% 
  slice(1:5)

sub_airline %>% # start with data
  mutate(Reporting_Airline = factor(Reporting_Airline,
                                    labels = c("AA", "AS", "DL", "UA", "B6", "PA (1)", "HP", "TW", "VX")))%>%
  
ggplot(aes(Reporting_Airline, fill = Reporting_Airline)) + scale_fill_manual( "legend", values =  c(AA = "red", AS = "blue", DL = "purple", UA = "gray", B6 = "yellow", PA = "black", HP = "pink", TW = "green", VX = "skyblue")) + stat_count(width = 0.5) + labs(x = "Number of data points in each airline") 


ssub_airline %>% # start with data
  mutate(Reporting_Airline = factor(Reporting_Airline,
                                    labels = c("AA", "AS", "DL", "UA", "B6", "PA (1)", "HP", "TW", "VX")))%>%
  ggplot(aes(Reporting_Airline)) +
  stat_count(width = 0.5) +
  labs(x = "Number of data points in each airline")



#Analyzing Individual Feature Patterns using Visualization

#geom_boxplot(): The boxplot compactly displays the distribution of a continuous variable. 
  #It visualises five summary statistics (the median, two hinges and two whiskers), and all "outlying" points individually.
#geom_jitter(): The jitter geom is a convenient shortcut for geom_point(position = "jitter"). 
  #It adds a small amount of random variation to the location of each point, 
  #and is a useful way of handling overplotting caused by discreteness in smaller datasets.
#ggtitle(): Modifies plot titles (main title, axis labels and legend titles).
#guides(): Guides for each scale can be set scale-by-scale with the guide argument.
#theme_minimal(): A minimalistic theme with no background annotations.
#coord_cartesian(): The Cartesian coordinate system is the most familiar, and common, type of coordinate system. 
#Setting limits on the coordinate system will zoom the plot (like you're looking at it with a magnifying glass), 
#and will not change the underlying data like setting limits on a scale will.


# Boxplot
ggplot(data = sub_airline, mapping = aes(x = Reporting_Airline, y = ArrDelay)) +
  geom_boxplot(fill = "bisque",color = "black", alpha = 0.3) + #shows boxplot
  geom_jitter(aes(color = 'blue'), alpha=0.2) +
  labs(x = "Airline") +
  ggtitle("Arrival Delays by Airline") +
  guides(color = FALSE) +
  theme_minimal() +  #This applies a minimalistic theme to the plot.
  coord_cartesian(ylim = quantile(sub_airline$ArrDelay, c(0, 0.99))) # The limits are set from the 0th percentile to the 99th percentile of the "ArrDelay" values, #
# which helps focus on the bulk of the data and exclude potential outliers.

# list the data types for each column
str(sub_airline)

#Positive Linear Relationship

# calculating the correlation b/w DepDelayMinutes and ArrDelayMinutes
cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes)

ggplot(data = sub_airline, mapping = aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_point() + 
  geom_smooth(method = "lm", na.rm = TRUE)

#Negative Linear Relationship

ggplot(data = sub_airline, mapping = aes(x = WeatherDelay, y = ArrDelayMinutes)) +
  geom_point() + 
  geom_smooth(method = "lm", na.rm = TRUE)

cor(sub_airline$WeatherDelay, sub_airline$ArrDelayMinutes, use = "complete.obs")
#use = "complete.obs": This parameter indicates that only complete observations 
#(rows with non-missing values for both variables)should be used in calculating the correlation coefficient.


#Descriptive Statistical Analysis

summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(count = n(), 
            mean = mean(ArrDelayMinutes, na.rm = TRUE),
            std_dev = sd(ArrDelayMinutes, na.rm = TRUE), 
            min = min(ArrDelayMinutes, na.rm = TRUE), 
            median = median(ArrDelayMinutes, na.rm=TRUE),
            iqr = IQR(ArrDelayMinutes, na.rm = TRUE), 
            max = max(ArrDelayMinutes, na.rm = TRUE))

summary_airline_delays

#To identify the data type of each column in a dataframe
sapply(sub_airline, typeof)

#Basic Grouping
avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep')
head(avg_delays)

# The color is still hard to see and identify,  let's change the color
avg_delays %>% 
  ggplot(aes(x = Reporting_Airline, 
             y = DayOfWeek, 
             fill = mean_delays)) +
  # set the tile's borders to be white with size 0.2
  geom_tile(color = "white", size = 0.2) +
  # define gradient color scales
  scale_fill_gradient(low = "yellow", high = "red")

# This visualization will use lubridate package
library(lubridate)
# Let's take a simple average across Reporting_Airline and DayOfWeek
avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep') %>%
  # create a new variable "bins" from mean_delays
  # make the first range -0.1 to 0.1 to include zero values
  mutate(bins = cut(mean_delays,breaks = c(-0.1,0.1,10,20,30,50, max(mean_delays)),
                    labels = c("0","0-10","10-20","20-30","30-50",">50"))) %>%
  mutate(bins = factor(as.character(bins),levels = rev(levels(bins))))

ggplot(avg_delays, aes(x = Reporting_Airline, 
                       y = lubridate::wday(DayOfWeek, label = TRUE), #day of the week names 
                       fill = bins)) +
  geom_tile(colour = "white", size = 0.2) +
  geom_text(aes(label = round(mean_delays, 3))) +
  guides(fill = guide_legend(title = "Delays Time Scale"))+
  labs(x = "Reporting Airline",y = "Day of Week",title = "Average Arrival Delays")+
  # Define color palette for the scale
  scale_fill_manual(values = c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4"))


#Pearson Correlation

sub_airline %>% 
  select(DepDelayMinutes, ArrDelayMinutes) %>% 
  cor(method = "pearson")

sub_airline %>% 
  cor.test(~DepDelayMinutes + ArrDelayMinutes, data = .) 

#Correlation between multible variables
correlation <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, 
         CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay) %>% 
  cor(use = "pairwise.complete.obs", method = "pearson")

correlation
# Download the corrplot library first if you have not already.
install.packages("corrplot")
library(corrplot)

numerics_airline <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, CarrierDelay,
         WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)

airlines_cor <- cor(numerics_airline, method = "pearson", use='pairwise.complete.obs')

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(airlines_cor, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45, #Text label color and rotation
)

#Anova Analysis

Summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(Average_Delays = mean(ArrDelayMinutes, na.rm = TRUE))

Summary_airline_delays %>%  
  ggplot(aes(x = Reporting_Airline, y = Average_Delays)) + 
  geom_bar(stat = "identity") +
  ggtitle("Average Arrival Delays by Airline") 

aa_as_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'AS')

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_as_subset)
summary(ad_aov)

aa_pa_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'PA (1)')

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_pa_subset)
summary(ad_aov)
