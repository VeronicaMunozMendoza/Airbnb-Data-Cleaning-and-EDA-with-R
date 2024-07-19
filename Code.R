  #  ======== PROJECT DETAILS ===============================================
  
  #    Author: Veronica Munoz Mendoza
  #    Date: April, 2024
  #    Project Summary: This project's goal is to analyse two Airbnb listings Dataset using RStudio
  #    with a city of my choice (Valencia, Spain) and compare it with the city of Dublin. 
  
  #    This project is going to be structured in the following sections:
  
  #       • Data understanding (preliminary data understanding analysis).
  #       • Data Preparation (Data cleaning)
  #           -  handling missing values and outliers. 
  #       • Data Exploration (Exploratory Data Analysis (EDA))
  #           - Plot data and identify trends and/or points of interest. 
  #           - Answer Questions: Q1 to Q8
  
  #  I will start the analysis with the chosen city (Valencia, Spain) and after 
  #  I will apply the same methodology for the Dublin dataset.
  
  #  ======== IMPORTING FILES ====================================================

  # Importing data from .cvs files into R. 
  Valencia_listing <- read.csv('~/Desktop/Data Analytics/DA/Airbnb - Data_Cleaning_with_R/Datasets/Valencia_listings.csv')
  Dublin_listing <- read.csv('~/Desktop/Data Analytics/DA/Airbnb - Data_Cleaning_with_R/Datasets/Dublin_listings.csv')

######################## VALENCIA DATASET   #################################### 
  
  # ======= # Data understanding (preliminary data analysis) # ========= # 
  
  # Importing libraries
    library(DescTools)
  
    View(Valencia_listing)     # Viewing dataset
    ncol(Valencia_listing)     # Checking number of columns 
    nrow(Valencia_listing)     # Checking number of rows 
    names(Valencia_listing)    # Checking column names
    str(Valencia_listing)      # Checking the structure of  dataset
    summary(Valencia_listing$price)  # Checking summary statistic per price
  
    
  # On the preliminary analysis I can identify that the dataset has some extreme and missing values. 
    
  # Before doing an in depth analysis (EDA) I am going to some data preparation (data cleaning). 
    
  # ======= # Data Preparation (Data cleaning) # ========= # 

  # Load necessary libraries
  library(dplyr)
  library(stringr)
  library(tidyr)

  # The column "name" has very interesting information that I can use to analyse later. 
  # I want to extract this information and clean that column.
  
  # Creating a new object
  Valencia <- Valencia_listing
  
  # Creating a function to perform several actions and clean the column name.
  # I would like to use this same function on the Dublin listing dataset at a later stage. 
  
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
  View(Valencia)

  
 # Identifying Missing values:
   # Identifying total NA values on data set and which columns have NA values. 
    
    # Counting total of missing values in the dataset 
    print(count_missing_val <- sum(is.na(Valencia)))
    # Identifying columns containing NA values 
    print(na_columns <- colnames(Valencia)[colSums(is.na(Valencia)) > 0]) 
    # Breakdown NA per column
    print(na_count_per_column <- colSums(is.na(Valencia[, na_columns])))   
    
  # Handling missing values for  "Bedrooms", "Beds", "Baths", "price".
    # There is a total of 729 Missing Values. Mostly in the column price
    # Missing values Breakdown: 
            # Bedrooms     Beds    Baths    price 
            #        3       67        3      656 
          
  # Since Bedrooms and baths have very small NA values a good approached will be 
  # to calculate the mode for each column (most frequent value) and replace it with it. 
  
  summary(Valencia[c("Bedrooms", "Baths")])
  
  # Replacing NA values in Bedrooms with mode
  Valencia$Bedrooms[is.na(Valencia$Bedrooms)] <- as.character(names(which.max(table(Valencia$Bedrooms))))
  
  # Replacing NA values in Baths with mode
  Valencia$Baths[is.na(Valencia$Baths)] <- which.max(table(Valencia$Baths))
  

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
  
  
  # Imputing median to deal with the 67 NA values on the column "Beds"
  Valencia$Beds[is.na(Valencia$Beds)] <- bed_median <- median(Valencia$Beds, na.rm = TRUE)

  # Calling check_missing() to confirm NA values on the dataset
  print(check_missing(Valencia))
  
  # Now we only have 656 NA values on the price column. 
  # Before I start imputation I would like to check for any correlation 
  # between variables or any outliers as this can skew my results. 
  
  # Checking correlation before imputation of price column
  
  # Selecting only numeric columns
  Valencia_numeric <- Valencia[sapply(Valencia, is.numeric)]
  
  # Calculating the correlation matrix
  cor_matrix <- cor(Valencia_numeric, use = "complete.obs")
  
  # Printing the correlation matrix
  print(cor_matrix)

  # Conclusion: 
  # The highest correlation is between Beds and Baths (0.575765008), properties with more bedrooms often have more bathrooms.
  # Rating has a positive correlation with number_of_reviews (0.31295074), suggesting that listings with higher ratings tend to have more reviews.
  # host_id has a negative correlation with number_of_reviews (-0.30966217), indicating that newer hosts (with higher host IDs) tend to have fewer reviews.
  # There is no multicolienarity.  
  
  # Checking unique values in the price column. Checking for outliers. 
  print(sort(unique(Valencia$price), na.last = TRUE))
  
  # It seems that there are very high values on the data set
  # starting from 6175 to 9999.
  
  # It seems that the listing with 9999 euros per night might be an error and is not representative of the data. 
  
  # Removing row 2179 where price_per_night is equal to 9999
  Valencia <- Valencia[-2179, ]
  
   
  # I am going to approach the 656 NA values on the column price with different methods:  
  
  # 1st Method: Identify outliers and perform median imputation on the outliers and price column.
  # 2nd Method: Removing only all NA values.
  # 3rd Method: Fitting a linear regression model to predict the prices
  # After I will check the statistics to decide which method I will be using to respond questions Q1 to Q8. 
  
  # METHOD 1 - Outlier Detection and Median Imputation for "price" Column
  Desc(Valencia$price)
  
  # Creating a new object for Method 1
  Valencia_Method1 <- Valencia
  
  # Identifying outliers
  print(sort(unique(Valencia_Method1$price), na.last = TRUE))

  # Removing NA values and calculate IQR
  price_no_na <- na.omit(Valencia_Method1$price)
  Q1 <- quantile(price_no_na, 0.25)
  Q3 <- quantile(price_no_na, 0.75)
  IQR <- Q3 - Q1
  
  # Calculating the lower and upper bounds
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Calculating the median of the non-outlier prices
  median_price <- median(price_no_na[price_no_na >= lower_bound & price_no_na <= upper_bound], na.rm = TRUE)
  
  # Replacing outliers with the median of the non-outlier prices
  Valencia_Method1$price[Valencia_Method1$price < lower_bound | Valencia_Method1$price > upper_bound] <- median_price

  # Performing Median imputation 
  
  # Calculating the median of the non-outlier prices
  median_price <- median(Valencia_Method1$price, na.rm = TRUE)
  
  # Replacing NA values with the median of the non-missing prices
  Valencia_Method1$price[is.na(Valencia_Method1$price)] <- median_price
  
  # Calling check_missing() to confirm NA 
  print(check_missing(Valencia_Method1))
  
  #  METHOD 2 - Removing only all NA values 
    
  # Creating a new object for Method 2
  Valencia_Method2 <- Valencia
  
  # Removing all rows with NA values
  Valencia_Method2 <- na.omit(Valencia_Method2)
  
  # Calling check_missing() to confirm NA 
  print(check_missing(Valencia_Method2))
  
  
  # METHOD 3 - fitting a linear regression model to predict the values (including outliers)

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
  

  # Comparing Imputation methods: 
  
  # Combining the price data from the three methods into a single data frame
  combined_data <- data.frame(
    Method1 = Valencia_Method1$price,
    Method2 = c(Valencia_Method2$price, rep(NA, nrow(Valencia_Method1) - nrow(Valencia_Method2))),
    Method3 = Valencia_Method3$price
  )
  
  # Creating a boxplot
  boxplot(combined_data, main = "Comparing Imputation Methods", 
          ylab = "Price", xlab = "Methods")
  
  # Conclusion: Based on the results I will be discarding method 1 it seems
  # that this method has reduced dramatically the variability of the dataset. On the other hand, it 
  # seems that method 2 and 3 will be a good option. I will do more analysis on the 
  # Data Exploration section that will help me decide which method I will select. 

  # Finalizing the data cleaning section on Valencia Dataset.
  
    # Creating a function to: 
      # Cleaning the column Bed and change the Studio string to O. 
      # Changing the column names to lower case. 
      # Converting the data type as numeric. 
    
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
    Valencia_Method1 <- clean_and_rename(Valencia_Method1)
    Valencia_Method2 <- clean_and_rename(Valencia_Method2)
    Valencia_Method3 <- clean_and_rename(Valencia_Method3)
    
    # Confirming results. 
      View(Valencia)
      View(Valencia_Method1)
      View(Valencia_Method2)
      View(Valencia_Method3)
    
  # Now that the dataset it is clean I am going to plot the data and analyse it 
  # further to identify trends and points of interest. 
 
  # ========= #   Data Exploration (Exploratory Data Analysis (EDA))  # ========= # 
  
  #   - Plot data and identify trends and/or points of interest
    
    # Before Plotting any data i would like to check the statistics to decide which method I would choose.
      Desc(Valencia_Method1$price)  # Plot Valencia_Method1 price per night
      Desc(Valencia_Method2$price)  # Plot Valencia_Method2 price per night
      Desc(Valencia_Method3$price)  # Plot Valencia_Method3 price per night
  
    # Results: 
      # Method 1- Median Imputation
        # Mean: 81.67
        # Std: 39.69 - lowest variability
        
      # Method 2 - Removing All NA Values:
        # Mean: 100.64
        # Std: 178.67 - highest variability
        
      # Method 3 - Linear Regression Model:
        # Mean: 100.91 - similar to Method 2
        # Std: 172.16 - Moderate variability, higher than Method 1 and lower than Method 2
        
    # Conclusion: 
      # From the 3 models it seems that the most suitable will be Method 3 Linear Regression Model. 
      # It seems that the mean is similar to method 2 however, the standard deviation is lower indicating a moderate variability. 
      # Based on the statistics Method 3 will be the most suitable since I want to preserve the original distribution of the data
      # and make my model able to provide a reasonable estimate for the missing values. 
    
    # Plotting
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
      
  # Conclusions/findings: 
        # Barplot: Half of the listings accounts for the room_type Entire home/apt. 
        # Histogram: While there are more than 1500 new listings, with no ratings yet (new). Most of the 
        # ratings fall between 4 and 5 stars. 
        # Scatterplot prices Vs. Number of reviews: The data suggests that Entire home/apt (red) and private rooms (green)
        # tend to receive more reviews. Since the number of reviews is generally low, we can conclude
        # it does not seem to have a correlation between number of reviews and price. 
        # Boxplot prices per room_type: Entire home/apt have wider range of prices and shared rooms will be the cheaper option.
  
######################## DUBLIN DATASET     #################################### 
        
 # Before answering Q1 to Q8 I will apply the same methodology used in 
 # the Valencia Dataset to the Dublin Dataset. 
   # - Data understanding (preliminary data understanding analysis).
   # - Data Preparation (Data cleaning) -  handling missing values.
        
  # ======= # Data understanding (preliminary data analysis) # ========= # 
        
  View(Dublin_listing)     # Viewing dataset
  ncol(Dublin_listing)     # checking number of columns 
  nrow(Dublin_listing)     # checking number of rows 
  names(Dublin_listing)    # checking column names
  str(Dublin_listing)      # checking the structure of the dataset
  summary(Dublin_listing$price)  # checking summary statistics per price
        
  # CONCLUSION: 
  # The price column has some missing values. I might need to do some regression analysis or other methods to 
  # deal with the missing values. 
        
  # ======= # Data Preparation (Data cleaning) # ========= # 
        
  # Creating a new object 
   Dublin <- Dublin_listing
        
  # Calling function to process column name
   Dublin <- process_data(Dublin)
   View(Dublin)
        
  # Calling function check_missing to confirm missing values
   print(check_missing(Dublin))
        
  # There are missing values in Baths (5), Bedrooms (12), Beds (105) and Price (3793)
        
  # Since Baths and Bedroms have very small NA 5 and 12 values a good approached will be 
  # to calculate the mode (most frequent value) for each column and replace it with it. 
    summary(Dublin[c("Baths", "Bedrooms", "Beds")])
        
  # Replacing NA values in Bedrooms with mode
    Dublin$Bedrooms[is.na(Dublin$Bedrooms)] <- as.character(names(which.max(table(Dublin$Bedrooms))))
        
  # Replacing NA values in Baths with mode
    Dublin$Baths[is.na(Dublin$Baths)] <- which.max(table(Dublin$Baths))
        
  # Confirming NA values on the dataset. 
  print(check_missing(Dublin))
        
  # Dealing with 105 NA from the column Beds. 
  # Checking the distribution and the frequencies. 
    Desc(Dublin$Bedrooms)
        
  # It seems that approx 60% of the listings has 1 bedroom. My approach to deal with the 
  # missing values here will be to impute the mode, as it is the most frequent occurring value. 
        
  # Replace NA values in Beds with mode
    Dublin$Beds[is.na(Dublin$Beds)] <- as.character(names(which.max(table(Dublin$Beds))))
        
  # Confirming NA values on the dataset. 
    print(check_missing(Dublin))
        
  # Boxplot to visualize outliers. 
    boxplot(Dublin$price, main = "Dublin Price Boxplot idenfitying outliers")
  
  # It seems that the there are certain listings with very high price 8820 45703 and 17400. 
  # This might be an error and might not be representative of the data. Since I would like 
  # to create a predictive model that calculates the prices accurately and these outliers might
  # affect the model performance. 
        
  # Removing rows where price_per_night is equal to 8820.00, 17400.00 and 45703.00.
  Dublin <- Dublin[!(Dublin$price %in% c(8820.00, 17400.00, 45703.00)), ]
  
  # Dealing with 3797 NA on the column price. 
  # Using METHOD 3 - fitting a linear regression model to predict the values
        
  # Checking correlation before before creating a linear regression model to predict price values. 
  # Select only numeric columns
    Dublin_numeric <- Dublin[sapply(Dublin, is.numeric)]
        
    
  # Calculating the correlation matrix
    cor_matrix <- cor(Dublin_numeric, use = "pairwise.complete.obs")
        
  # Printing the correlation matrix
   print(cor_matrix)
        
  # Conclusion:
  # Based on this results it seems there is no issues with multicolienarity. 
  # none of the correlation coefficients are close to 1 or -1. 
        
  # Creating a linear regression model: 
    Dublin_Method3 <- Dublin
        
  # Cleaning bathrooms column
    Dublin_Method3 <- clean_and_rename(Dublin_Method3)
        
  # Fitting a linear regression model using non-missing values
    model_val_Dub <- lm(price ~ bedrooms + baths + beds, data = Dublin_Method3[!is.na(Dublin_Method3$price),])
        
  # Predicting missing values
    predicted_prices <- predict(model_val_Dub, newdata = Dublin_Method3[is.na(Dublin_Method3$price),])
        
  # Rounding predicted prices to whole numbers
    predicted_prices <- round(predicted_prices,2)
        
  # Replacing missing values with rounded predicted prices
    Dublin_Method3$price[is.na(Dublin_Method3$price)] <- predicted_prices
        
 # Calling check_missing() to confirm NA 
   print(check_missing(Dublin_Method3))
   
 # Viewing resutls
   View(Dublin_Method3)
        
 # Plotting column price
   boxplot(Dublin_Method3$price, main=" Dublin Method 3 - Fitting a linear regression model")
 
 

######################## Questions: Q1 - Q8 ####################################

  # Q1. What is the mean, median and standard deviation of the number of reviews per property in your selected city?
    
    # Defining function to calculate mean, median and std for the number_of_reviews column in Valencia_Method3
      calculate_stats <- function(df) {
        mean_val = round(mean(df[["number_of_reviews"]], na.rm = TRUE),2)
        median_val = round(median(df[["number_of_reviews"]], na.rm = TRUE),2)
        sd_val = round(sd(df[["number_of_reviews"]], na.rm = TRUE),2)
        
        paste("Mean:", mean_val, ", Median:", median_val, ", SD:", sd_val)
      }
      
      # Calling function and printing results
      print(paste("Statistics - Number of reviews per property: ", calculate_stats(Valencia_Method3)))
      
    # Conclusion: It seems that the number of reviews per property is around 43.53, 
    # the std 74.28 and the median 13. This suggests that there are a few properties 
    # with a very high number of reviews, while most properties have a smaller number of reviews.
  
  # Q2. What is the mean price per night for a Private Room in your selected city
    
    # Creating a pipeline to calculate the mean price per night for a private room. 
     Valencia_Method3 %>% 
      subset(room_type == "Private room") %>% 
      summarise(mean_price = round(mean(price, na.rm = TRUE), 2)) %>% 
      print()
      
  # Q3. How much above the average price would it cost to stay for a night in the most expensive
  #  private room in the city?
     
    # The mean price for a private room is 39.78. 
    # To answer this question first I need to calculate the max price 
    # of private room and then substract it from the average price. 
     
      # Defining function to calculate above average
       above_avg_cost <- function(df, room_type, mean_price) {
         max_price <- max(df$price[df$room_type == room_type], na.rm = TRUE)
         above_avg_cost <- round(max_price - mean_price)
         return(above_avg_cost)
       }
       
       above_avgcost_pr_val <- above_avg_cost(Valencia_Method3, "Private room", 39.78)
       print(above_avgcost_pr_val)
   
  # Q4. How do the prices in Q2 & Q3 compare to Dublin?
     
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
       
    # Conclusion: 
    # Q2: We can see in the comparison table that the mean price for a private room 
    # in Valencia is lower than Dublin. 
    # Q3: The above average cost of spending a night in the most expensive private room in Valencia
    # is also lower than Dublin.
     
  # Q5. What is the name and ID of the host with the most properties listed on Airbnb for your
  #  selected city?
 
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
     
  # Q6. How much could this host (as identified in Q5) potentially earn per week from their Airbnb
  #  listings
  
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
       
  # Q7. Does this host (as identified in Q5) have any properties listed in Dublin? If yes, how many?
       
       # Creating a pipeline to filter Dublin_Method3 to check if host_id has any properties in Dublin city. 
       
         Dublin_Method3 %>%
         filter(host_id == 83066665) %>%
         nrow() %>% 
         print()
  
    # Conclusion: This host does not have any properties listed in Dublin. 
       
  # Q8. Write a function (called “mostHomes”) to determine which neighbourhood in a city has the
  #  most entire homes/apartments listed on Airbnb and use this function to answer that
  #  question for your selected city and for Dublin?
      
     # Definint function
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

  # Result: 
     # CABANYAL-CANYAMELAR   728
     # Dublin City           3494
  
  


     