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
