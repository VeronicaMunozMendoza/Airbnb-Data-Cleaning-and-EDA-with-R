# Airbnb Data Cleaning and EDA with R

## Table of Contents

- [Project Details](#project-details)
- [Data Sources](#data-sources)
- [Project Structure](#project-structure)
    - [Data Understanding (preliminary data understanding analysis)](#data-understanding-preliminary-data-understanding-analysis)
    - [Data Preparation (Data cleaning)](#data-preparation-data-cleaning)
    - [Data Exploration (Exploratory Data Analysis (EDA))](#data-exploration-exploratory-data-analysis-eda)
- [Project Reflection](#project-reflection)
    - [Findings](#findings)
    - [Challenges](#challenges)
    - [Issues / Weaknesses](#issues--weaknesses)

#### Raw Data
<img width="1466" alt="Screenshot 2024-07-19 at 11 30 16" src="https://github.com/user-attachments/assets/6d0f8efc-9387-4aba-a489-dcabd97a6941">

#### cleaned data
<img width="1466" alt="Screenshot 2024-07-19 at 11 30 25" src="https://github.com/user-attachments/assets/b3b6eff5-c206-48bd-9a5a-ab5184ea8fe2">


## Project Details

Author: Veronica Munoz Mendoza

Project Summary:

This project's goal is to analyse two Airbnb listings Data set using RStudio with a city of my choice (Valencia, Spain) and compare it with the city of Dublin. We are also aiming at answering the following questions:

-   Q1. What is the mean, median and standard deviation of the number of reviews per property in the selected city?

-   Q2. What is the mean price per night for a Private Room in the selected city.

-   Q3. How much above the average price would it cost to stay for a night in the most expensive private room in the city?

-   Q4. How do the prices in Q2 & Q3 compare to Dublin?

-   Q5. What is the name and ID of the host with the most properties listed on Airbnb for the selected city?

-   Q6. How much could this host (as identified in Q5) potentially earn per week from their Airbnb listings.

-   Q7. Does this host (as identified in Q5) have any properties listed in Dublin? If yes, how many?

-   Q8. Write a function (called “mostHomes”) to determine which neighborhood in a city has the most entire homes/apartments listed on Airbnb and use this function to answer that question for your selected city and for Dublin?

## Data Sources

Source: <http://insideairbnb.com/get-the-data.html>

Data set files: Valencia_listings.csv and Dublin_listings.csv found in the data sets folder in this repository.

## Project Structure

### Data understanding (preliminary data understanding analysis)

Reading files:

```{r}
# Importing data from .cvs files into R.

Valencia_listing <- read.csv('~/Desktop/Data Analytics/DA/Airbnb - Data_Cleaning_with_R/Datasets/Valencia_listings.csv')

Dublin_listing <- read.csv('~/Desktop/Data Analytics/DA/Airbnb - Data_Cleaning_with_R/Datasets/Dublin_listings.csv')

```

Importing libraries and performing a preliminary analysis:

```         
library(DescTools)

head(Valencia_listing)     # Viewing dataset
ncol(Valencia_listing)     # Checking number of columns 
nrow(Valencia_listing)     # Checking number of rows 
names(Valencia_listing)    # Checking column names
str(Valencia_listing)      # Checking the structure of  dataset
summary(Valencia_listing$price)  # Checking summary statistic per price
```

```{r}
str(Valencia_listing)   # Checking the structure of  dataset
```

```{r}
summary(Valencia_listing$price)  # Checking summary statistic per price
```

-   On the preliminary analysis I can identify that the dataset has some extreme and missing values.
-   Before doing an in depth analysis (EDA) I am going to some data preparation (data cleaning).

### Data Preparation (Data cleaning)

The column "name" has very interesting information that I can use to analyse later.

I want to extract this information and clean that column, to do so I will be creating a function to perform several actions and clean the column name. I would like to use this same function on the Dublin listing dataset at a later stage allowing me to be more efficient.

```{r}
# Load necessary libraries 
library(dplyr) 
library(stringr) 
library(tidyr)

 # Creating a new object
  Valencia <- Valencia_listing
  
 # Defining the function
   process_data <- function(df) {
    
  # Splitting column name into five columns based on the separator "·"
    df <- df %>%
      separate(name, into = c("part1", "part2", "part3", "part4", "part5"), sep = "·", fill = "right") %>%
      
  # Triming white spaces from the start and end of each part
    mutate(across(starts_with("part"), str_trim)) %>%
      
  # Assigning values to 'Type_Location', 'Rating', 'Bedrooms', 'Beds', and 'Baths' based on the content of each part
    mutate(
      Type_Location = part1,
      Rating = case_when(
        str_detect(part2, "★") ~ part2,
        str_detect(part3, "★") ~ part3,
        str_detect(part4, "★") ~ part4,
        str_detect(part5, "★") ~ part5,
        TRUE ~ NA_character_
      ),
      Bedrooms = case_when(
        str_detect(part2, "bedroom") ~ part2,
        str_detect(part3, "bedroom") ~ part3,
        str_detect(part4, "bedroom") ~ part4,
        str_detect(part5, "bedroom") ~ part5,
        str_detect(part2, "Studio") ~ "Studio",
        str_detect(part3, "Studio") ~ "Studio",
        str_detect(part4, "Studio") ~ "Studio",
        str_detect(part5, "Studio") ~ "Studio",
        TRUE ~ NA_character_
      ),
      Beds = case_when(
        str_detect(part2, "bed") & !str_detect(part2, "bedroom") ~ part2,
        str_detect(part3, "bed") & !str_detect(part3, "bedroom") ~ part3,
        str_detect(part4, "bed") & !str_detect(part4, "bedroom") ~ part4,
        str_detect(part5, "bed") & !str_detect(part5, "bedroom") ~ part5,
        TRUE ~ NA_character_
      ),
      Baths = case_when(
        str_detect(part2, "bath") ~ part2,
        str_detect(part3, "bath") ~ part3,
        str_detect(part4, "bath") ~ part4, 
        str_detect(part5, "bath") ~ part5,
        TRUE ~ NA_character_
      )
      ) %>%
      
    # Removing the temporary "part" columns
    select(-starts_with("part")) %>%
      
    # Cleaning up the 'Rating' column: 
      #  - removing the "★" symbol, replace "New" with "3.5",
      #  - replacing NA with "0.0", 
      #  - convert to numeric, 
      #  - rounding to one decimal place
    mutate(
      Rating = str_replace(Rating, "★", ""),
      Rating = if_else(Rating == "New", "3.5", Rating),
      Rating = if_else(is.na(Rating), "0.0", Rating),
      Rating = as.numeric(Rating),
      Rating = round(Rating, 1)
      ) %>%
    
    # Cleaning up the 'Bedrooms' column: 
      # - extracting the numeric part and convert to numeric, 
      # - leaving "Studio" value as is. 
    mutate(
      Bedrooms = case_when(
        Bedrooms != "Studio" ~ as.character(as.numeric(str_extract(Bedrooms, "\\d+"))),
        TRUE ~ Bedrooms)
      ) %>%
  
    # Cleaning up the 'Beds' column: 
      #  - extracting the numeric part and convert to numeric.
    mutate(
      Beds = as.numeric(str_extract(Beds, "\\d+"))
      )  %>%
  
    # Clean up the 'Baths' column: 
      # - replacing "Half-bath", "Private half-bath", and "Shared half-bath" with "0.5", 
      # - extracting the numeric part from the rest of the values and convert to numeric.
    mutate(
      Bath_Type = case_when(
        str_detect(Baths, "shared") ~ "Shared",
        str_detect(Baths, "private") ~ "Private",
        str_detect(Baths, "Half-bath") ~ "Half",
        TRUE ~ "Regular"
        ),
      Baths = case_when(
        Baths %in% c("Half-bath", "Private half-bath", "Shared half-bath") ~ "0.5",
        TRUE ~ as.character(Baths)),
        Baths = as.numeric(str_extract(Baths, "\\d+(\\.\\d+)?"))
      ) %>%
      
    # Selecting only the most relevant columns.
    select(
      "Type_Location",
      "Rating",
      "Bedrooms", 
      "Beds", 
      "Baths",
      "Bath_Type", 
      "host_id", 
      "host_name", 
      "neighbourhood", 
      "room_type",
      "price", 
      "minimum_nights", 
      "number_of_reviews", 
      "calculated_host_listings_count"
      )
    
    return(df)
  }
  
  # Calling the function
  Valencia <- process_data(Valencia)
  head(Valencia)
```

#### Handling missing values and outliers.

Handling missing values

```{r}
# Identifying total NA values on data set and which columns have NA values. 
    
    # Counting total of missing values in the dataset 
    print(count_missing_val <- sum(is.na(Valencia)))
    # Identifying columns containing NA values 
    print(na_columns <- colnames(Valencia)[colSums(is.na(Valencia)) > 0]) 
    # Breakdown NA per column
    print(na_count_per_column <- colSums(is.na(Valencia[, na_columns])))   
```

There is a total of 729 Missing Values, mostly in the column price.

Since Bedrooms and baths have very small NA, only 3 values a good approached will be to calculate the mode for each column (most frequent value) and replace it with it.

```{r}
 summary(Valencia[c("Bedrooms", "Baths")])
  
  # Replacing NA values in Bedrooms with mode
  Valencia$Bedrooms[is.na(Valencia$Bedrooms)] <- as.character(names(which.max(table(Valencia$Bedrooms))))
  
  # Replacing NA values in Baths with mode
  Valencia$Baths[is.na(Valencia$Baths)] <- which.max(table(Valencia$Baths))
```

To make my life easier, I am going to create a function to check for missing values on my data set as I have to also evaluate the Dublin data set at a later stage:

```{r}
  # Creating a function to check for missing values
  check_missing <- function(df) {
    # Checking if there are any missing values in the dataframe
    any_na <- any(is.na(df))
    
    # Calculating the number of missing values in each column
    col_na <- colSums(is.na(df))
    
    # Returning a list with the results
    return(list("Any NA" = any_na, "Column NA" = col_na))
  }
  
  # Calling check_missing() to confirm NA values on the dataset
  print(check_missing(Valencia))
```

```{r}
  # Imputing median to deal with the 67 NA values on the column "Beds"
  Valencia$Beds[is.na(Valencia$Beds)] <- bed_median <- median(Valencia$Beds, na.rm = TRUE)

  # Calling check_missing() to confirm NA values on the dataset
  print(check_missing(Valencia))
```

I currently have 656 missing values in the price column. Before starting the imputation process, I would like to check for any correlations between variables and identify any outliers, as these factors could skew the results.

During my analysis, I used three methods to determine the best approach for handling the missing values:

1.  **Identify outliers and perform median imputation** on the outliers and price column.

2.  **Remove all rows with NA values** in the price column.

3.  **Fit a linear regression model** to predict the missing prices.

The conclusion was that the best approach was using a linear regression model to predict the missing price values. This method helps preserve the original data distribution and allows the model to provide reasonable estimates for the missing values.{r}

```{r}
# Checking correlation before imputation of price column
  
  # Selecting only numeric columns
  Valencia_numeric <- Valencia[sapply(Valencia, is.numeric)]
  
  # Calculating the correlation matrix
  cor_matrix <- cor(Valencia_numeric, use = "complete.obs")
  
  # Printing the correlation matrix
  print(cor_matrix)
```

Conclusion:

-   The highest correlation is between Beds and Baths (0.575765008), properties with more bedrooms often have more bathrooms.

-   Rating has a positive correlation with number_of_reviews (0.31295074), suggesting that listings with higher ratings tend to have more reviews.

-   host_id has a negative correlation with number_of_reviews (-0.30966217), indicating that newer hosts (with higher host IDs) tend to have fewer reviews.

-   There is no multidisciplinary.

#### Handling outliers

```{r}
  # Checking unique values in the price column. Checking for outliers. 
  print(sort(unique(Valencia$price), na.last = TRUE))
```

It seems that there are very high values on the data set starting from 6175 to 9999.

It seems that the listing with 9999 euros per night might be an error and is not representative of the data.

```{r}
  # Removing row 2179 where price_per_night is equal to 9999
  Valencia <- Valencia[-2179, ]
```

```{r}
# Creating a linear regression model: 
  
  # Creating a new object for Method 3
  Valencia_Method3 <- Valencia
  
  # Fitting a linear regression model using non-missing values
  model_val <- lm(price ~ Bedrooms + Baths + Beds, data = Valencia_Method3[!is.na(Valencia_Method3$price),])
  
  # Predicting missing values
  predicted_prices <- predict(model_val, newdata = Valencia_Method3[is.na(Valencia_Method3$price),])
  
  # Rounding predicted prices to whole numbers
  predicted_prices <- round(predicted_prices,2)
  
  # Replacing missing values with rounded predicted prices
  Valencia_Method3$price[is.na(Valencia_Method3$price)] <- predicted_prices
  
  # Calling check_missing() to confirm NA 
  print(check_missing(Valencia_Method3))
  
```

Finalizing the data cleaning section on Valencia Dataset.

```{r}
 # Defining the function
    clean_and_rename <- function(df) {
      if ("Bedrooms" %in% names(df)) {
        df <- df %>%
          mutate(
            Bedrooms = if_else(trimws(tolower(Bedrooms)) == "studio", "0", Bedrooms),
            Bedrooms = as.numeric(str_extract(Bedrooms, "\\d+"))
          )
      }
      
      names(df) <- tolower(names(df))
      
      return(df)
    }
    
    # Using the function on all my objects. 
    Valencia <- clean_and_rename(Valencia)
    Valencia_Method3 <- clean_and_rename(Valencia_Method3)
    
    # Confirming results. 
      head(Valencia_Method3)
```

Now that the dataset it is clean I am going to plot the data and analyse it further to identify trends and points of interest.

### Data Exploration (Exploratory Data Analysis (EDA))

#### Plot data and identify trends and/or points of interest.

```{r}
 # Load the necessary library
      library(ggplot2)
      library(forcats)
      
      # Creating a barplot for the room_type column
      ggplot(Valencia_Method3, aes(x = fct_infreq(room_type))) +
        geom_bar(fill = "wheat3") +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        xlab("Room Type") +
        ylab("Count") +
        ggtitle("Distribution of Room Types in Valencia")
      
      # Creating a histogram for the Ratings column
      ggplot(Valencia_Method3, aes(x = rating)) +
        geom_histogram(fill = "wheat3", color = "grey99", size = 0.1, bins = 30) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        xlab("Rating") +
        ylab("Frequency") +
        ggtitle("Distribution of Ratings in Valencia")
      
      # Creating a Scatter Plots of Price vs. Number of Reviews by room_type
      ggplot(Valencia_Method3, aes(x = number_of_reviews, y = price, color = room_type)) +
        geom_point() +
        theme_minimal() +
        xlab("Number of Reviews") +
        ylab("Price") +
        ggtitle("Price vs. Number of Reviews") +
        scale_color_brewer(palette = "Set1") +
        theme(legend.title = element_blank()) +
        facet_wrap(~ room_type)
      
      # Boxplot of Prices per Room Type:
        ggplot(Valencia_Method3, aes(x = room_type, y = price)) +
        geom_boxplot() +
        theme_minimal() +
        xlab("Room Type") +
        ylab("Price") +
        ggtitle("Boxplot of Prices per Room Type")
      
```

![im04](https://github.com/user-attachments/assets/d99f4c11-52da-4e74-b46d-60ec7834656b)

![im01](https://github.com/user-attachments/assets/8b5384cf-46e7-48e8-8360-0b58f6a7661c)

![im00](https://github.com/user-attachments/assets/a88ffa44-fc28-4af2-a916-8d8dbb1b6dab)

![im02](https://github.com/user-attachments/assets/bcc93732-d018-4a7a-bbe0-b22a5dcb9004)


#### Conclusions/findings:

-   Bar plot: Half of the listings accounts for the room_type Entire home/apt.

-   Histogram: While there are more than 1500 new listings, with no ratings yet (new). Most of the

-   Ratings fall between 4 and 5 stars.

-   Scatter plot prices Vs. Number of reviews: The data suggests that Entire home/apt (red) and private rooms (green) tend to receive more reviews. Since the number of reviews is generally low, we can conclude it does not seem to have a correlation between number of reviews and price.

-   Box plot prices per room_type: Entire home/apt have wider range of prices and shared rooms will be the cheaper option.

#### Answer Questions: Q1 to Q8

I will have applied the same methodology used in the Valencia Data set to the Dublin Data set. You can find all the code in the Code.R file in this repository.

*Q1. What is the mean, median and standard deviation of the number of reviews per property in the selected city?*

```{r}
# Defining function to calculate mean, median and std for the number_of_reviews column in Valencia_Method3
  calculate_stats <- function(df) {
    mean_val = round(mean(df[["number_of_reviews"]], na.rm = TRUE),2)
    median_val = round(median(df[["number_of_reviews"]], na.rm = TRUE),2)
    sd_val = round(sd(df[["number_of_reviews"]], na.rm = TRUE),2)
    
    paste("Mean:", mean_val, ", Median:", median_val, ", SD:", sd_val)
  }
  
  # Calling function and printing results
  print(paste("Statistics - Number of reviews per property: ", calculate_stats(Valencia_Method3)))
  

```

**Conclusion**: It seems that the number of reviews per property is around 43.53,the std 74.28 and the median 13. This suggests that there are a few properties with a very high number of reviews, while most properties have a smaller number of reviews.

*Q2. What is the mean price per night for a Private Room in the selected city.*

```{r}
# Creating a pipeline to calculate the mean price per night for a private room. 
 Valencia_Method3 %>% 
  subset(room_type == "Private room") %>% 
  summarise(mean_price = round(mean(price, na.rm = TRUE), 2)) %>% 
  print()
  
```

*Q3. How much above the average price would it cost to stay for a night in the most expensive  private room in the city?*

The mean price for a private room is 39.78. To answer this question first I need to calculate the max price of private room and then substract it from the average price.

```{r}
# Defining function to calculate above average
   above_avg_cost <- function(df, room_type, mean_price) {
     max_price <- max(df$price[df$room_type == room_type], na.rm = TRUE)
     above_avg_cost <- round(max_price - mean_price)
     return(above_avg_cost)
   }
   
   above_avgcost_pr_val <- above_avg_cost(Valencia_Method3, "Private room", 39.78)
   print(above_avgcost_pr_val)
```

*Q4. How do the prices in Q2 & Q3 compare to Dublin?*

```{r}
 # Calculating mean price for Dublin
 Dublin_Method3 %>% 
   subset(room_type == "Private room") %>% 
   summarise(mean_price = round(mean(price, na.rm = TRUE), 2)) %>% 
   print()
 
 above_avgcost_pr_dub <- above_avg_cost(Dublin_Method3, "Private room", 90.29)
 print(above_avgcost_pr_dub)

 # I would like to create a comparison table, to compare the results: 
   # Creating objects for mean price and above-average cost for Valencia
   valencia_mean_price <- 60.04
   valencia_above_avg_cost <- 7160
   
   # Creating objects for mean price and above-average cost for Dublin
   dublin_mean_price <- 143.56
   dublin_above_avg_cost <- 8699
   
   # Create a data frame for the comparison table
   comparison_table <- data.frame(
     City = c("Valencia", "Dublin"),
     Mean_Price = c(valencia_mean_price, dublin_mean_price),
     Above_Avg_Cost = c(valencia_above_avg_cost, dublin_above_avg_cost)
   )
   
   # Print the comparison table
   print(comparison_table)
```

**Conclusion**:

-   Q2: We can see in the comparison table that the mean price for a private room in Valencia is lower than Dublin.

-   Q3: The above average cost of spending a night in the most expensive private room in Valencia is also lower than Dublin.

*Q5. What is the name and ID of the host with the most properties listed on Airbnb for your selected city?*

```{r}
   # Creating a pipeling to group the dataset by host_id and host_name, 
   # counting the number of listings for each host, and sorting it in descending order
   most_prop_val <- Valencia_Method3 %>%
     group_by(host_id, host_name) %>%
     summarise(count = n()) %>%
     arrange(desc(count))
   
   # Creating an object to store the first row (the host with the most listings)
   top_host <- most_prop_val[1,]
   
   # Printing the host_id and host_name of the host with the most properties
   print(paste("Host:", top_host$host_name, "ID:", top_host$host_id))
 
```

*Q6. How much could this host (as identified in Q5) potentially earn per week from their Airbnb listings.*

```{r}
# Creating pipeline that will filter the host with most listings, this pipeline will: 
  # filter the rows with the host with most listings. 
  # adds the price and multiply it by 7 to know the earnings per week and
  # formats the result adding a symbol "€" and a comma for the thousands for the price. 
  
   # Extracting the host_id from top_host
   top_host_id <- top_host$host_id
   
   Valencia_Method3 %>%
     filter(host_id == top_host_id) %>%
     summarise(potential_earnings = sum(price, na.rm = TRUE) * 7) %>%
     mutate(potential_earnings = paste0("€", format(potential_earnings, big.mark = ",", scientific = FALSE))) %>%
     print()
```

Q7. Does this host (as identified in Q5) have any properties listed in Dublin? If yes, how many?

```{r}
   # Creating a pipeline to filter Dublin_Method3 to check if host_id has any properties in Dublin city. 
   
     Dublin_Method3 %>%
     filter(host_id == 83066665) %>%
     nrow() %>% 
     print()
```

**Conclusion**: This host does not have any properties listed in Dublin.

*Q8. Write a function (called “mostHomes”) to determine which neighbourhood in a city has the most entire homes/apartments listed on Airbnb and use this function to answer that question for the selected city and for Dublin?*

```{r}
 # Defining function
   mostHomes <- function(df) {
    # Filtering dataframe by Entire home/apt and group by neighbourhood
      df <- df %>% 
       select("neighbourhood", "room_type") %>% 
       filter(room_type == "Entire home/apt") %>% 
       group_by(neighbourhood)
         
   # Counting neighbourhoods and arrange in descending order
      hood <- df %>% 
       summarise(count = n()) %>%  
       arrange(desc(count))
         
  # Return the neighbourhood with the most homes
      return(hood[1,])
       }
       
       
# Calling function on Valencia_Method3 and Dublin_Method3 
mostHomes(Valencia_Method3) 
mostHomes(Dublin_Method3)
```

Result:

```         
 # CABANYAL-CANYAMELAR   728
 # Dublin City           3494
```

## Project Reflection

This section includes a brief reflection section on the analysis undertaken on this project. There reflection is composed of 3 sections: Findings, Challenges and Issues / Weaknesses.

#### Findings

In the Valencia data set there are listings with only 4 room types: Entire home/apt, hotel, shared room, private room and half of the listings are accounted for “Entire home/apt”. Most of the listings received between 4 and 5 rating stars (positive guest feedback). On the other hand there are over 1500 new listings which no ratings yet, this suggests recent additions to the platform.

When I compared prices and number of reviews, "Entire home/apt" and private rooms received the highest number of reviews compared with the other room types. However, the overall number of reviews was generally low, I would also conclude that there was no strong correlation between the reviews and the prices.

In pricing terms, the listings with "Entire home/apt" room type had a wider price ranged compared with the other ones, while shared rooms were the most affordable.

After my analysis, the mean number of reviews for Valencia was 43.53 (median 13 and std 74.28). This suggests that while a few properties have a very high number of reviews, most properties have a smaller number of reviews.

On the other hand, the price per night for a private room in Valencia was €60.04, significantly cheaper than in Dublin, when the mean price was €143.56. The host with most properties in Valencia was SingularStays and it was estimated that this host could potentially earn €95,879 per week from their Airbnb listings. However, this host did not have any properties listed in Dublin.

The neighborhoods with the most entire homes/apartments listed on Airbnb in Valencia was 'Cabanyal-Canyamelar' with 728 listings and 'Dublin City' in Dublin with 3494.

#### Challenges

During my analysis I faced several challenges, one of them was missing values. I used different methods to handle them, such as calculating the mode for certain columns, imputing the median for others or fitting a linear regression model to predict the missing prices.

Another challenge was identifying and handling outliers in the data sets. For instance, in the Valencia dataset, there were listings with extremely high prices per night, which were likely errors and not representative of the data. These outliers were removed to prevent them from skewing the results of the analysis.

#### Issues / Weaknesses

One of the weakness in the analysis will be the potential inaccuracy of the predicted prices. Although I had used linear regression model to predict the missing prices, these predictions may not be entirely accurate and could affect the reliability of the analysis.

Another weakness was the assumption made when calculating potential earnings for the host with the most properties. The calculation performed, assumed that all of the host's properties were booked for the entire week, which may not be the case in reality.

Lastly, the analysis was limited to only two cities - Valencia and Dublin. While this provided interesting comparisons, the findings may not be generalize to other cities or countries. A possible next steps would be to use more advance statistical techniques such as Robust Regression that that can reduce the impact of outliers in the dataset or to include more cities for a broader comparison.
