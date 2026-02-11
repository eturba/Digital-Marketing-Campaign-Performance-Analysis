# Digital-Marketing-Campaign-Performance-Analysis

## Context and Objective

This analysis examines social media campaign data from XYZ company run on Facebook (Meta). **Ideally, we would have revenue return data to calculate the actual ROI of each campaign.** However, since we don't have this information, we'll treat this problem as a **cost optimization exercise.**

Our objectives are:

-   Identify and cut campaigns with high spending and low performance
-   Discover demographic and segmentation patterns that work
-   Recommend adjustments to maximize investment efficiency

The file contains 1,143 observations distributed across 11 variables:

1.  **ad_id**: Unique identifier for each ad
2.  **xyz_campaign_id**: XYZ company campaign ID
3.  **fb_campaign_id**: Facebook campaign tracking ID
4.  **age**: Target audience age range
5.  **gender**: Target audience gender
6.  **interest**: Interest category code
7.  **Impressions**: Total number of times the ad was shown
8.  **Clicks**: Total number of times users clicked on the ad
9.  **Spent**: Total amount of money invested by XYZ Company to run the ad
10. **Total_Conversion**: Total number of people who showed interest in the product after viewing the ad
11. **Approved_Conversion**: Total number of people who completed a purchase after engaging with the ad

First, we will load the necessary libraries for this project:

```R
# Loading Libraries
library(readr)
library(tidyverse)
library(ggplot2)
library(skimr)
library(DataExplorer)
library(gridExtra)
library(scales)
library(colorspace)
```

Next, we'll standardize plot dimensions to a default width and height to improve readability and visual clarity.

```R
options(repr.plot.width = 15, repr.plot.height = 10)
```

Before we start the analysis, we will create a conversion funnel. The following source was used to assist in the creation process: <https://gist.github.com/jjesusfilho/fd14b58becab4924befef5be239c6011>

This funnel will be used to visually represent how users move through different stages of a digital marketing campaign, for example, from seeing an ad (impressions) to clicking it, showing interest, and finally making a purchase. This type of visualization helps quickly identify where audience drop-offs occur in the conversion process, making it easier to pinpoint inefficiencies in campaign performance.

We will utilize this funnel later in our analysis.

```R
# Function to create conversion funnel charts
# Source: https://gist.github.com/jjesusfilho/fd14b58becab4924befef5be239c6011

gg_funnel <- function(x, text = NULL, color = NULL, lbl_size = 4){
  
  ### Type Validation ###
  if (!is.numeric(x)){
    stop("x must be a numeric vector")
  }
  
  if (any(x < 0)){
    stop("This function does not accept negative values")
  }
  
  x <- sort(x, decreasing = TRUE)
  
  if (is.null(color)){
    color <- colorspace::qualitative_hcl(length(x), palette = "Dark 3")
  }
  
  if (is.null(text)){
    text <- as.character(x)
  }
  
  if(!all.equal(length(x), length(text), length(color))){
    stop("x, text, and color must have the same length")
  }
  
  ### Create x coordinates ###
  l1 <- vector("list", length(x))
  
  for (i in 1:length(x)){
    if (i == 1){
      x3 <- x[1]
      x4 <- 0
      x1 <- seq(x4, x3, length.out = 6)[2]
      x2 <- seq(x4, x3, length.out = 6)[5]
    } else {
      x4 <- l1[[i-1]][1]
      x3 <- l1[[i-1]][2]
      x1 <- seq(x4, x3, length.out = 6)[2]
      x2 <- seq(x4, x3, length.out = 6)[5]
    }
    
    l1[[i]] <- c(x1, x2, x3, x4)
  }
    
  ### Create y coordinates ###
  l2 <- purrr::map(length(x):1, ~{
    c(.x*5-5, .x*5-5, .x*5, .x*5)
  })
  
  ## Create data.frame based on coordinates ###
  dfs <- purrr::map2(l1, l2, ~{
    data.frame(x = .x, y = .y)
  })
  
  ### Create individual plots and stack them ###
  p <- ggplot2::ggplot()
  
  for (i in 1:length(dfs)){
    p<- p +
      ggplot2::geom_polygon(data = dfs[[i]], ggplot2::aes(x = x, y = y),
                            fill = color[i]) +
      ggplot2::annotate("text", label = text[i], x = x[1]/2,
                        y = mean(dfs[[i]]$y),
                        size = lbl_size,
                        fontface = "bold",
                        color = "black") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
  }
    
    p
}
```

Once the funnel is created, we will now load the data.

```R
# Loading data
df <- read_csv("KAG_conversion_data.csv")
```

The next few steps will allow us to get an introduction into the data set.

```R
# Data structure overview
glimpse(df)
```

```R
# First rows of the dataset
head(df)
```

```R
# Complete statistical summary
df %>% skimr::skim() %>% print()
```

Now let's get an introductory visualization of data quality.

This plot tells us how many variables are continuous versus discrete and what proportion of rows and cells contain missing values. This will serve as an initial data quality check before getting into exploratory analysis.

```R
# Introductory visualization of data quality 
plot_intro(df)
```

To begin our exploratory analysis, let's look at the distribution of ads by campaign:

```R
# Distribution of ads by campaign
df %>% 
  group_by(xyz_campaign_id) %>% 
  summarise(total = n())
```

## Exploratory Analysis: Distribution by Campaign

We observe that **Campaign 916** had a much lower volume of ads compared to the others, while **Campaign 1178** had the most ads. **Campaign 916** only having **54** ads seems like an outlier compared to **Campaign 936** and **Campaign 1178** which had **464** and **625** ads respectfully.

Let's investigate how age groups are distributed in the database and how they fare in each campaign.

```R
# General distribution by age range
df %>% 
  group_by(age) %>% 
  summarise(total = n())
```

## Age Concentration

Approximately **60% of the database** is concentrated in the **30-39 years** range with the age range between **30-34** being the highest at **426**.

How are these groups distributed when we consider each campaign individually?

```R
# Age distribution by campaign
df %>% 
  group_by(xyz_campaign_id, age) %>% 
  summarise(total = n(), .groups = "drop_last") %>% 
  mutate(percentage = round(total * 100 / sum(total), 2))
```

## Age Segmentation Pattern

All campaigns focus predominantly on the **30-34 years** range, with the difference being less pronounced in **Campaign 1178** where **32.2%** of ads were focused on that age range which is the lowest percentage compared to the other two campaigns.

Now let's analyze how genders are distributed when we consider campaign and age range simultaneously.

```R
# General distribution by gender
df %>% 
  group_by(gender) %>% 
  summarise(total = n())
```

## Gender Balance

The distribution between genders is quite balanced, with a slightly higher numbers of ads going towards the **male** audience.

Now let's break it down by campaign.

```R
# Gender distribution by campaign
df %>% 
  group_by(xyz_campaign_id, gender) %>% 
  summarise(total = n(), .groups = "drop_last") %>% 
  mutate(percentage = round(total * 100 / sum(total), 2))
```

## Difference in Campaign 936

Only **Campaign 936** shows a higher overall percentage of **women** with **55.2%** of ads being directed towards **women**.

What if we include age ranges in this analysis?
