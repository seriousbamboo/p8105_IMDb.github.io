some plots we can use
================
Jiying Han
11/29/2020

Explore–

find the association between director’s facebook like and gross

``` r
dataset = read.csv("./data/imdb_model_clean.csv") 
ep_dataset =read.csv("./data/imdb_explore_clean.csv")


dataset %>% 
  filter(director_facebook_likes != 0) %>% 
  mutate(log_director = log(director_facebook_likes)) %>% 
  filter(gross > 5000000 & log_director > 1 & log_director < 7 ) %>% 
  ggplot(aes(x = log_director, y = gross)) +
  geom_point()+
  geom_smooth(method='lm', se=TRUE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](some-plots_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
dataset %>% 
  filter(director_facebook_likes != 0) %>% 
  mutate(log_director = log(director_facebook_likes),
         log_gross = log(gross)) %>% 
  filter(gross > 5000000 & log_director > 1 & log_director < 7 ) %>% 
  ggplot(aes(x = log_director, y = log_gross)) +
  geom_hex()
```

![](some-plots_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
 # From above plot we can see that there is no direct association between director's facebook like and movie's gross
```

explore the characteristics of some variables

``` r
ep_dataset %>% 
  select(actor_1_facebook_likes,actor_3_facebook_likes,cast_total_facebook_likes) %>% 
  filter(
    actor_1_facebook_likes != 0,
    cast_total_facebook_likes!= 0,
    actor_3_facebook_likes != 0
         ) %>% 
  distinct() %>% 
  pivot_longer(
    actor_1_facebook_likes:cast_total_facebook_likes,
    names_to = "facebook_likes",
    values_to = "number_likes"
  ) %>% 
  mutate(log_number_likes = log(number_likes)) %>% 
  ggplot(aes(x = reorder(facebook_likes, -log_number_likes) , y = log_number_likes, fill = facebook_likes)) + 
    geom_violin() +
    geom_boxplot(width=0.1, color="black", alpha=0.2) +
    scale_fill_viridis(discrete = TRUE) +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("Facebook likes of different group in movie")+
    xlab("Facebook likes") +
    ylab("Log of Facebook likes")
```

![](some-plots_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

explore the relationship between move genre and its duration(don’t need)

``` r
orderds = ep_dataset %>% 
  select(duration,genres) %>% 
  distinct() 

orderds %>% 
  filter(genres != "News",
         genres !="Film-Noir") %>% 
  ggplot(aes(x = reorder(genres,-duration, na.rm = TRUE), y = duration, color = genres)) + 
    geom_boxplot() +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))
```

    ## Warning: Removed 3 rows containing non-finite values (stat_boxplot).

![](some-plots_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

the association between imbd\_score and gross

``` r
dataset %>% 
  select(imdb_score, gross) %>% 
  filter(imdb_score>3.75 & imdb_score<8.5) %>% 
  ggplot(aes(x = imdb_score, y = gross)) +
  geom_point() +
  theme_bw(base_size=20) +
  geom_smooth(method='lm', se=TRUE) +
  labs(x="IMDB score of movie", y="Movie gross")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](some-plots_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# the association between imdb score and movie gross are proportional, althought it is not obvious.
```

the association between cast total facebook like and gross

``` r
dataset %>% 
  select(cast_total_facebook_likes, gross) %>% 
  mutate(log_cast_total_facebook_likes = log(cast_total_facebook_likes)) %>% 
  filter(log_cast_total_facebook_likes<9 & log_cast_total_facebook_likes>5) %>% 
  ggplot(aes(x = log_cast_total_facebook_likes, y = gross)) +
  geom_hex()
```

![](some-plots_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

association between budget and genre

``` r
ep_dataset %>% 
  select(budget, genres) %>% 
  distinct() %>% 
  filter(budget<3e+08) %>% 
  filter(genres != "News",
         genres !="Film-Noir") %>% 
  ggplot( aes(x=reorder(genres,-budget, na.rm = TRUE), y=budget, fill=genres)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))+
    ggtitle("A boxplot with budget") +
    xlab("Movie genre") +
    ylab("Budget of movie")   
```

![](some-plots_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ep_dataset %>% 
  select(budget, genres) %>% 
  distinct() %>% 
  filter(budget<3e+08) %>% 
  filter(genres != "News",
         genres !="Film-Noir") %>% 
  mutate(log_budget = log(budget)) %>% 
  ggplot( aes(x=reorder(genres,-budget, na.rm = TRUE), y=log_budget, fill=genres)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))+
    ggtitle("A boxplot with budget") +
    xlab("Movie genre") +
    ylab("Budget of movie")   
```

![](some-plots_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

association between budget and gross

``` r
ep_dataset %>% 
  select(budget, gross, genres) %>% 
  filter(budget<3e+08) %>% 
  ggplot(aes(x=budget, y=gross)) +
  geom_hex()
```

![](some-plots_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

number of critics for reviews and gross

``` r
ep_dataset %>% 
  select(num_critic_for_reviews , gross) %>% 
  distinct() %>% 
  mutate(gross = log(gross)) %>% 
  ggplot(aes(x = num_critic_for_reviews, y = gross)) +
  geom_hex()
```

    ## Warning: Removed 1 rows containing non-finite values (stat_binhex).

![](some-plots_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

number of voted users and gross

``` r
ep_dataset %>% 
  select(gross, num_voted_users) %>% 
  distinct() %>% 
  mutate(gross = log(gross)) %>% 
  ggplot(aes(x = num_voted_users, y = gross)) +
  geom_point(
    color="black",
        fill="#69b3a2",
        shape=22,
        alpha=0.5,
        size=3,
        stroke = 1
        ) +
  theme_ipsum()
```

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列

![](some-plots_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

gross and facebook likes

``` r
ep_dataset %>% 
  select(cast_total_facebook_likes,gross, genres) %>% 
  distinct() %>% 
  mutate(cast_total_facebook_likes = log(cast_total_facebook_likes)) %>% 
  ggplot(aes(x = cast_total_facebook_likes, y = gross,color = genres )) +
  geom_point() +
  coord_cartesian()
```

![](some-plots_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ep_dataset %>% 
  select(movie_facebook_likes,gross, genres) %>% 
  mutate(gross = log(gross)) %>% 
  ggplot(aes(x = movie_facebook_likes, y = gross, color = genres)) +
  geom_point( 
    color="orange",
    fill="#69b3a2",
    shape=21,
    alpha=0.3,
    size=2,
    stroke = 1)
```

![](some-plots_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
ep_dataset %>% 
  select(actor_1_facebook_likes, actor_2_facebook_likes, actor_3_facebook_likes, gross) %>%
  distinct() %>% 
  pivot_longer(
    actor_1_facebook_likes:actor_3_facebook_likes,
    names_to = "actor",
    values_to = "facebook_likes"
  ) %>% 
  mutate(
    actor = as.factor(actor),
    gross = log(gross)
  ) %>% 
  filter(facebook_likes <0.5e+05) %>% 
  ggplot(aes(x=facebook_likes, y = gross, color = actor)) +
  geom_point(
        fill="#69b3a2",
        shape=22,
        alpha=0.5,
        size=2,
        stroke = 1
        ) +
    theme_ipsum()
```

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列
    
    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## Windows字体数据库里没有这样的字体系列

![](some-plots_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->