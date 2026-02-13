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

```R
# Visualization: Gender distribution by age and campaign
df %>% 
  group_by(xyz_campaign_id, age, gender) %>% 
  summarise(total = n(), .groups = "drop_last") %>% 
  mutate(percentage = round(total * 100 / sum(total), 2)) %>% 
  ggplot(aes(x = age, y = percentage, fill = gender)) +
  geom_col() +
  geom_text(aes(label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  facet_wrap(~ xyz_campaign_id) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Gender Distribution by Age and Campaign",
    y = "Proportion (%)",
    x = "Age Range"
  ) +
  theme_minimal()
```
**Campaign 936** was spread out evenly for **men** and **women** in the age range **30-34** but targeted more **women** in all other age categories.

**Campaign 1178** had a fairly even gender spread across age ranges with about **55%** of ads being directed towards **men** in each age range.

## Introduction of Performance Metrics

Now let's calculate standard digital marketing metrics:

-   **CTR(Click-Through Rate)**: The percentage of users who click on an ad after seeing it.
-   **CPC(Cost Per Click)**: The average amount paid each time a user clicks on an ad.
-   **CPA(Cost Per Acquisition)**: The average cost incurred for each completed conversion or sale.

```R
# Performance metrics calculation
df <- df %>% 
  mutate(
    CTR = ifelse(Impressions > 0, (Clicks / Impressions) * 100, NA),
    CPC = ifelse(Clicks > 0, Spent / Clicks, NA),
    CPA = ifelse(Approved_Conversion > 0, Spent / Approved_Conversion, NA)         
  )
```

Let's first verify that the columns have been created correctly:

```R
# Verification of new columns
head(df, 3)
```

Now we will aggregate the performance metrics for each campaign in the data set and then we will create a series of funnel plots to visualize the performance of each campaign using the gg_funnel function that we created earlier.

```R
# Metrics aggregation by campaign 
df_performance <- df %>% 
  group_by(xyz_campaign_id) %>% 
  summarise(
    ads_run = n_distinct(ad_id),
    investment = sum(Spent),
    impressions = sum(Impressions),
    clicks = sum(Clicks),
    leads = sum(Total_Conversion),
    sales = sum(Approved_Conversion),
    ctr_global = (clicks / impressions) * 100,
    cpc_global = investment / clicks,
    lead_to_sale_rate = (sales / leads) * 100,
    cpa_global = investment / sales,
    .groups = "drop"
  )
```

```R
plot_list <- list()

for(campaign in unique(df_performance$xyz_campaign_id)) {
  
  campaign_data <- df_performance %>% 
    filter(xyz_campaign_id == campaign)
  
  funnel_vec <- c(
  campaign_data$impressions,
  campaign_data$clicks,
  campaign_data$leads,
  campaign_data$sales
  )
  
texts<- c(
  paste0("Imp: ", scales::comma(funnel_vec[1])),
  paste0("Clicks: ", scales::comma(funnel_vec[2]), "\n(",
         round(funnel_vec[2]/funnel_vec[1]*100, 2), "%)"),
  paste0("Leads: ", funnel_vec[3], "\n(",
         round(funnel_vec[3]/funnel_vec[2]*100, 2), "%)"),
  paste0("Sales: ", funnel_vec[4], "\n(",
         round(funnel_vec[4]/funnel_vec[3]*100, 2), "%)")
)

p <- gg_funnel(funnel_vec, text = texts) +
  labs(
    title = paste("Campaign", campaign),
    subtitle = paste("CPA: $", round(campaign_data$cpa_global, 2))
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
  
plot_list[[as.character(campaign)]] <- p
}

grid.arrange(grobs = plot_list, ncol = 3)
```

```R
# Performance summary table
print(df_performance)
```

## Preliminary Performance Insights

**Campaign 916**: Shows low relative investment and few ads run, but has the **best lead→sale conversion rate** and consequently the **lowest CPA**. This may be the result of efficient segmentation, good niche choice (interest) or simply statistical luck. Given the small sample size (compared to the other campaigns) it is tough to come away with any conclusions.

**Campaign 1178**: Shows a huge investment was done for the campaign and in turn, generated by far the most **impressions, clicks, leads, and sales** compared to the other campaigns. However, **Campaign 1178** also showed a **CPA over 4x worse** than other campaigns. The campaign did have a similar **lead→sale conversion rate** compared to **Campaign 936** at **32.67%**.

### Question to Investigate:

-   Are there differences in interests that were targeted in **Campaign 916** compared to the interests that were targeted in **Campaign 1178**?
-   In **Campaign 1178**, did some ads perform well but were "pulled down" by others resulting in the higher CPA?
-   Are there specific patterns of **interests** and **ages** that explain the differences between the success of one ad to the next?

To further our analysis, let's group the ads based on interest and campaign and then we'll aggregate the performance metrics.

Then, we'll create a matrix style graph which shows the sales volume and CPA for interest code in each ad campaign. The goal is to understand **where campaigns invested, which interests generated sales, and how cost-efficient those investments were**.

```R
# Performance analysis by interest and campaign
interest_analysis <- df %>%
  mutate(xyz_campaign_id = as.character(xyz_campaign_id)) %>%
  group_by(interest, xyz_campaign_id) %>%
  summarise(
    sales = sum(Approved_Conversion),
    cpa = sum(Spent) / sum(Approved_Conversion),
    .groups = "drop"
  ) %>%
  filter(sales > 0)

# Visualization: Interest matrix
ggplot(interest_analysis, aes(x = xyz_campaign_id, y = as.factor(interest))) +
  geom_point(aes(size = sales, color = cpa)) +
  scale_color_gradient(low = "green", high = "red") +
  labs (
    title = "Interest Matrix: Where Did Campaigns Invest?",
    y = "Interest Code",
    x = "Campaign",
    color = "CPA ($)",
    size = "Sales Vol."
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())
```

## Interest Analysis

### Campaign 1178 drove the majority of volume — but at a cost

From the prior analysis, we already knew **Campaign 1178** was the highest spender and that was confirmed by the matrix as the campaign had by far the most bubbles and the largest bubbles overall. What we did learn from the matrix is that **Campaign 1178** has a broad investment across many interest segments. However, many of these high-volume bubbles skew yellow to red including some of the bubbles with the highest sales volume.

### Campaign 916 shows limited investment and narrow impact

Likewise, we already knew from the prior analysis that **Campaign 916** had the lowest investment and the matrix now proves that the campaign had a narrow scope as few interests were tested, however, all interests performed well and were in the green.

### Campaign 936 seems to be cost-efficient overall

Most bubbles under **Campaign 936** were green and the CPA is consistently low across many interest segments. Additionally, Several interests show moderate sales volume with strong efficiency. Campaign 936 appears well-optimized, balancing conversion volume with low acquisition cost.

### Diminishing returns appear in high-volume interests

The largest bubbles in **Campaign 1178** are often orange/red. This suggests that as spending increased, CPA increased.

Will including age groups in our segmentation allow us to extract more robust insights?

Let's first identify the top 5 interests by spending across all campaigns.

```R
# Identification of top 5 interests by spending
top_interest_codes <- df %>% 
  group_by(interest) %>% 
  summarise(spending = sum(Spent)) %>% 
  slice_max(spending, n = 5) %>% 
  pull(interest)
```

```R
#Display of most invested interest codes
top_interest_codes %>% print()
```

Now, we will create a heat map showing the CPA for each interest in relation to the age group that it was targeting. This graph will hopefully provide us insights into what interests performed better or worse for certain age groups.
