# explore different dimensions of diamond 
  library(magrittr)
  library(ggplot2)
  library(lvplot)
  library(ggbeeswarm)
  library(dplyr)
  library(hexbin)
  library(modelr)
  ggplot2 :: diamonds%>% 
  ggplot2 :: ggplot() +
  ggplot2 :: geom_histogram(mapping = ggplot2 :: aes(price,y = ..density..), binwidth =10)+
  ggplot2::xlim(0,1000)
  #ggplot2::coord_cartesian(xlim = c(0,1000))

  # difference between having 0.99 carat diamond and 1 carat diamond
  
  ggplot2 :: diamonds %>%
    dplyr :: group_by(carat)%>%
    dplyr :: filter(carat %in% c(0.99,1))%>%
    ggplot2::ggplot(mapping = ggplot2 :: aes(carat))+
    ggplot2 :: geom_histogram()
  
  # missing values in histogram vs bar chart how is it handled ?
  
  # 1.  histogram.
  
  missing_df <- dplyr :: tribble(
    ~col1, ~col2,
    "row1",122,
    NA, 70,
    "row3", NA,
    NA, 12
  )
  ggplot2::ggplot(missing_df, mapping = ggplot2 :: aes(col2))+
    ggplot2 :: geom_histogram(bins= 5)
  # handled using bins  = n. default bar chart uses more bins than histogram.
  
  # understanding box plot features ..
  
  ggplot2 :: ggplot(ggplot2 :: diamonds, 
                    mapping = ggplot2 :: aes(x = cut, y = price)) +
    ggplot2 :: geom_boxplot()
  
  ggplot2 :: ggplot(ggplot2 :: mpg,
                    mapping = ggplot2 :: aes(x = reorder(class,hwy, FUN = median),y = hwy))+
      ggplot2::geom_boxplot()+
    ggplot2 :: coord_flip()
  
  # testing for ggstance :: geom_boxploth()
  ggplot2 :: ggplot(ggplot2 :: mpg,mapping = ggplot2 :: aes(x = hwy, y = class, fill = factor(cyl)))+
    ggstance :: geom_boxploth()
  
  # testing letter value plot package ..
 
  ggplot(ggplot2 :: mpg)+
    geom_freqpoly(mapping = aes(x = displ))+
    geom_violin(mapping = aes(x = displ, y = hwy)) 
  
  # testing ggbeeswarm package methods similar to geom_jitter()
  ggplot(ggplot2 :: mpg,position=position_quasirandom())+
    geom_freqpoly(mapping = aes(x = displ))
    #geom_beeswarm(mapping = aes(x = displ, y = hwy),groupOnX = FALSE)
    #geom_quasirandom(mapping = aes(x = displ, y = hwy),groupOnX = FALSE)
    diamonds %>%
      count(color, cut) %>%
      ggplot(mapping = aes(x = color, y = cut))+
      geom_tile(mapping = aes(fill = n))
    
    # geom_tile with dplyr to check for flight delays ..
    nycflights13::flights %>%
      group_by(arr_delay, dest) %>%
      summarise(mean_delays = mean(arr_delay, na.rm = T))%>%
      ggplot(mapping = aes(dest,mean_delays)) +
      geom_tile(mapping = aes(fill = arr_delay))+
      coord_cartesian(xlim = c(0.5, 100.5))

    # bins in 2 directions .
    diamonds %>%
      ggplot(mapping = aes(x = carat, y = price)) +
     # geom_bin2d()
      geom_hex()
    
    # patterns and models ..
    mod <- lm(log(price) ~ log(carat), data = diamonds)
    diamonds2 <- diamonds %>%
      add_residuals(mod)%>%
      mutate(resid = exp(resid))
      ggplot(data = diamonds2)+geom_point(mapping = aes(x = carat, y = price))
      
      # plotting resid vs cut  ..
      mod <- lm(log(price) ~ log(carat), data = diamonds)
      diamonds2 <- diamonds %>%
        add_residuals(mod)%>%
        mutate(resid = exp(resid))
      ggplot(data = diamonds2)+geom_boxplot(mapping = aes(x = cut, y = resid))
      ggsave("diamonds.pdf")
      
      # subsetting for tibbles ..
      df <- tibble(x = runif(5), y = rnorm(5))
      df$x
      df$y
      df[["x"]]
      df[[1]]
      
      # subsetting using pipes 
      df %>% .$x 
      df %>% .[["x"]]
      df %>% .[[1]]

      # tibble as data frame for older functions not compatible...
      as.data.frame(df)
      
      # tibble :: enframe()
      x <- c(1,2,3,4,5,6)
      tibble::enframe(x)
      