Porject Proposal
================
Jiying Han, Liucheng Shi, Yatong feng, Zhe Chen, Wenhao Gou

# P8105 Project Proposal-Box-office prediction model based on IMDB

## 1． Group members

  - Wenhao wg2364
  - Zhe Chen zc2556
  - Yatong Feng yf2563
  - Liucheng Shi ls3751
  - Jiying Han jh4324

## 2． Title

Box-office prediction model based on IMDB

``` r
library(tidyverse)
imdb_raw = read_csv("./data/movie_metadata.csv")
imdb = read_csv("./data/imdb_model_clean.csv")
#imdb_raw %>% 
  #group_by(country) %>% 
  #summarize(n = n()) %>% 
  #arrange(desc(n))
```

## 3． Motivation

Given the scenario of social distancing and temporary closure of movie
theaters across the United States, Box-office prediction has never been
such a crucial problem in the film industry that profitability is
directly associated with strategy made by investors and producers.

Our main goal is to:

  - use skinny to explore the association between the box-office revenue
    and other variables including critical review, genre, budget, and
    content rating
  - create word cloud, radar map on contributing factors
  - generate linear regression model to predict box-office revenue

We hope this site would help you retrieve useful information and provide
you a thorough understanding on key terms that tied with box office
success.

## 4． Intended final products

1)  Preliminary descriptive statistics
2)  Prediction model and tests on assumption and validity
3)  Interactive visualization panels for grouped data (e.g. gross
    revenue vs. Facebook likes directors/actors received)
4)  Overlaid plots of estimation and real data

## 5． Data source

imdb （kaggle）

## 6． Planned analysis

1)  Model: Linear regression
2)  Visualization: word cloud, trend map

## 7． Coding challenge

1)  Data cleaning

<!-- end list -->

  - 1.  Processing string

  - 2.  Might explore more data based on the existing dataset to expand
        the content and do further study or predict

  - 3.  Processing and analysing outliers

<!-- end list -->

2)  Check assumptions
3)  Visualization of the interactive map

## 8． Timeline

  - 0.5w for data cleaning

We will observe and explore the raw dataset and then using knowledge
from module data wangling1 and 2 to clean the raw data.

  - 1w for model building (initial model)

We will use linear regression to analyse the factors that might
influence the box-office revenue basing on the dataset and do the
prediction.

  - 1w for visualization plots

Based on the clean data and model, we will make some plots and dashboard
to visualize the dataset.

  - 1w for website building

Using R studio to build the website, including about page, dashboard,
tables. And we will try our best to make the website be interactive as
much as possible.

  - 0.5 week for finalization.

We organize and review the whole work, and might do some small changes
in order to unpload the best version.
