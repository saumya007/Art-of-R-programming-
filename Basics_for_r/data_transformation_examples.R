# using dplyr functions :

# filter..
  y <- dplyr :: filter(nycflights13 :: flights, month == 1, day == 1)
  y # here filter was taking the default filter function and not dplyr filter so 
  # needed to explicitly define dplyr :: filter(dataframe,filters...)
  
  # boolean operators in filter ..
  y <- dplyr :: filter(nycflights13 :: flights, month %in% c(11,12))
  y
  
  # demorgan's laws with filter....
  z <- dplyr :: filter(nycflights13 :: flights, !(arr_delay >120 | dep_delay >120))
  z # ude goku
  
  # filter exercises
    # delay more than 2 hrs
    z <- dplyr :: filter(nycflights13 :: flights (arr_delay >= 2 ))
    z
    
    # destination  IAH or HOU
    options(dplyr.width = Inf)
    z <- dplyr :: filter(nycflights13 :: flights, dest %in% c("IAH", "HOU"))
    z
    
    # operated by United American or Detla
    z <- dplyr::filter(nycflights13 :: flights, carrier %in% c("UA", "DL"))
    z
    
    # departed in july, august or september ..
    z <- dplyr::filter(nycflights13 :: flights, month %in% c(7,8,9))
    z
    
    # arrived more than 2 hrs late but didnt leave late
    
    z <- dplyr::filter(nycflights13 :: flights, arr_delay >=2 & dep_delay ==0)
    z
    
    # testing for between
    z <-dplyr :: between(nycflights13 :: flights$dep_time, 600,1200)
    z
    # testing for select function :
    z <-dplyr :: select(nycflights13 :: flights, year, month, day,dep_delay, arr_delay)
    z
    
    # selection from a range
    z <- dplyr :: select(nycflights13 :: flights, year : month)
    z
    
    # excluding a range of columns...
    z <- dplyr :: select(nycflights13 :: flights, - (year :day))
    z

    # name of a variable multiple times in select :
    z <- dplyr :: select(nycflights13 :: flights, year, year)
    z

    # testing for one_of() function 
    var1 <-  c("year", "month", "day", "dep_delay", "arr_delay")
    z<- dplyr :: select(nycflights13 :: flights,dplyr::one_of(var1))
    z
    
    # checking for case handling in tibble with helpers ..
    z <- dplyr :: select(nycflights13 :: flights, dplyr :: contains("time"))
    z

    # using mutate() to add columns as a function of other columns ..
    flights_small <-dplyr :: select(nycflights13 :: flights, year : day, 
                                    dplyr :: ends_with("delay"),
                                    distance, air_time)
    dplyr :: mutate(flights_small, gain = arr_delay - dep_delay,
           speed  = distance / air_time * 60)
    dplyr :: mutate(flights_small, gain = arr_delay - dep_delay, hours = air_time/60, gain_per_hr = gain/hours)
    
    # testing cum sum and cum mean , cum prod, cum max, cum min
    x <- c (1,2,3,4,5)
    cumsum(x)
    dplyr :: cummean(x)
    cumprod(x)   
    cummin(x)
    cummax(x)

    # testing ranking functions ..
    dplyr :: min_rank(x)
    dplyr :: min_rank(dplyr :: desc(x))
    dplyr :: row_number(x)
    dplyr :: dense_rank(x)
    dplyr :: percent_rank(x)
    dplyr :: cume_dist(x)
    dplyr::ntile(5,x)
    
    # testing summarise 
    dplyr::summarise(nycflights13 :: flights, mean(dep_delay, na.rm = T))
    
    # without pipelining code for delay vs distance for nycflights13 :: flights dataset
    by_dest <- dplyr::group_by(nycflights13 :: flights, dest)
    delay <- dplyr :: summarise(by_dest, count = n(),
                                dist = mean(distance, na.rm = T),
                                delay = mean(arr_delay, na.rm = T))    
    delay <- dplyr :: filter(delay, count >20, dest != "HNL")    
      ggplot2 :: ggplot(data = delay, mapping = ggplot2 :: aes(x = dist, y = delay)) +
      ggplot2 :: geom_point(ggplot2 :: aes(size = count), alpha = 1/3)+
      ggplot2 :: geom_smooth(se = FALSE)
    
    # another way using pipe 
      library(magrittr)
      #delay <- nycflights13::flights %>%
      #dplyr::group_by(dest) %>%
      #dplyr::summarise(count = n(),
       #                dist = mean(distance, na.rm = T,
        #               delay = mean(air_delay, na.rm = T))
         #              ) %>%
      #dplyr :: filter(count > 20, dest != "HNL")
      delays <- nycflights13::flights %>% 
        dplyr::group_by(dest) %>% 
        dplyr::summarise(
          count = n(),
          dist = mean(distance, na.rm = TRUE),
          delay = mean(arr_delay, na.rm = TRUE)
        ) %>% 
        dplyr:: filter(count > 20, dest != "HNL")
      ggplot2 :: ggplot(data = delays, mapping = ggplot2 :: aes(x = dist, y = delay)) +
      ggplot2 :: geom_point(ggplot2 :: aes(size = count), alpha = 1/3)+
      ggplot2 :: geom_smooth(se = FALSE)
    
      # handling misssing values with pipe operator ..
      not_cancelled <- nycflights13::flights %>%
        dplyr :: filter(!is.na(dep_delay), !is.na(arr_delay))
      not_cancelled %>% 
        dplyr :: group_by(year, month, day) %>%
        dplyr :: summarise(mean = mean(dep_delay))
      
    # counts . Either count for observations which have values by n() or count na by is.na()
      delays <- not_cancelled %>%
        dplyr :: group_by(tailnum) %>%
        dplyr :: summarise(delay = mean(arr_delay))
        ggplot2:: ggplot(data = delays, mapping = ggplot2 :: aes(x = delay)) +
          ggplot2 :: geom_freqpoly(binwidth = 10)
      
    #  scatter plots of no of flights vs average delay ..
        delays <- not_cancelled %>%
          dplyr :: group_by(tailnum) %>%
          dplyr :: summarise(delay = mean(arr_delay, na.rm = T),
                             n = n())
        ggplot2::ggplot(data = delays, mapping = ggplot2 :: aes(x = n, y = delay)) + 
          ggplot2 :: geom_point(alpha = 1/10)
        
        delays %>% 
          dplyr :: filter(n >25) %>%
          ggplot2::ggplot(data = delays, mapping = ggplot2 :: aes(x = n, y = delay)) +
          ggplot2::geom_point(alpha = 1/10)
        
        # batter average from lahman package ..
        batting <- dplyr ::as_tibble(Lahman :: Batting)
        batters <- batting %>%
          dplyr :: group_by(playerID) %>%
          dplyr :: summarise(
            ba = sum(H, na.rm = T)/ sum(AB, na.rm = T),
            ab = sum(AB, na.rm = T)
          )
        batters %>%
          dplyr :: filter(ab > 100) %>%
          ggplot2::ggplot(mapping = ggplot2 :: aes(x = ab, y = ba))+
          ggplot2 :: geom_point() +
          ggplot2 :: geom_smooth(se = F)
        
        # test sd, first, last, n_distinct ( calculates distinct values ), mutate 
        
        not_cancelled %>%
          dplyr::group_by(dest) %>%
          dplyr :: summarise(distance_sd = sd(distance)) %>%
          dplyr :: arrange(desc(distance_sd)) 
        
        not_cancelled %>%
          dplyr :: group_by(year, month, day)%>%
          dplyr :: summarise(
            first = dplyr :: first(dep_time),
            last = dplyr :: last(dep_time)
          )
        
        not_cancelled %>%
          dplyr::group_by(dest) %>%
          dplyr :: summarise(carriers = dplyr :: n_distinct(carrier)) %>%
          dplyr :: arrange(desc(carriers)) 
        
        # exercises 
        not_cancelled %>%
          dplyr :: group_by(dest) %>%
          dplyr :: summarise(n = n())
        
        not_cancelled %>%
          dplyr :: group_by(tailnum) %>%
          dplyr :: tally(wt = distance)

        not_cancelled %>%
          dplyr :: count(tailnum, wt = distance)
        
        # relation between cancelled flights and daily delay of flights ..
        
        nycflights13::flights %>%
          dplyr :: group_by(day)%>%
          dplyr :: summarise(
            cancelled = mean(is.na(dep_delay)),
            mean_dep = mean(dep_delay, na.rm = T),
            mean_arr = mean(arr_delay, na.rm = T)
          )%>%
          ggplot2::ggplot(mapping = ggplot2 :: aes(y = cancelled)) +
          ggplot2 :: geom_point(mapping = ggplot2 :: aes(x = mean_dep), color = "red") +
          ggplot2 :: geom_point(mapping = ggplot2 :: aes(x = mean_arr), color = "blue") +
          ggplot2 :: labs(x = "Average delay per day", y = "Cancelled flights per day")
        
        # which career has worst delays ..
        nycflights13::flights %>%
          dplyr :: group_by(carrier, dest) %>%
          dplyr :: summarise(
            dep_max = max(dep_delay, na.rm = T),
            arr_max = max(arr_delay, na.rm = T)
          )%>%
          dplyr::arrange(desc(dep_max,arr_max))%>%
          dplyr :: filter(1 : n() ==1)
        
        # check if career is bad or airport is bad ?
        nycflights13::flights %>%
          dplyr::summarise(n_car = n_distinct(carrier),
                           n_air = n_distinct(dest),
                           n_or = n_distinct(origin))
          nycflights13::flights %>%
            dplyr::group_by(carrier)
            mutate(avg_carrier = mean(dep_delay, na.rm = T))
            dplyr :: group_by(carrier, origin)
            mutate(origin_mean = mean())
            
        # plane with the worst ontime record ... 
            nycflights13::flights %>%
              dplyr :: group_by(tailnum)
              dplyr :: filter(!is.na(arr_delay))
              dplyr :: summarise(
                prop_time = sum(arr_delay <=30)/ n(),
                mean_arrtime = mean(arr_delay, na.rm = T),
                fl = n()
              )%>%
              dplyr :: arrange(dplyr :: desc(prop_time))
              
        # what time of day to travel to avoid the delay as muc as possible  .. 
              nycflights13::flights %>%
                dplyr :: group_by(hour)%>%
                dplyr :: filter(!is.na(dep_delay))%>%
                dplyr :: summarise(
                  delay = mean(dep_delay >0 , na.rm = T)
                )%>%
                ggplot2::ggplot(mapping = ggplot2 :: aes(hour, delay, fill = delay)) + 
                ggplot2::geom_col()

          # total delay of destination and delay proportion of flights ..
              nycflights13::flights%>%
                dplyr :: group_by(dest)%>%
                dplyr :: filter(!is.na(dep_delay))%>%
                dplyr :: summarise(total_delay = sum(dep_delay[dep_delay >0]))
              
              nycflights13::flights%>%
                dplyr :: group_by(tailnum, dest)%>%
                dplyr :: filter(!is.na(dep_delay))%>%
                dplyr :: summarise(m = mean(dep_delay >0), n = n())%>%
                dplyr :: arrange(desc(m))

              # lag to check how delay of flight is related to delay of pervious flight 
              flights <-nycflights13::flights
              flights%>%
              dplyr :: mutate(perv_delay = lag(dep_delay))%>%
              dplyr :: select(origin, dep_delay, perv_delay) 
                
              # flights which are suspiciously fast ?  get by min air time
              nycflights13::flights%>%
                dplyr :: arrange(air_time)%>%
                dplyr :: select(air_time)%>%
                dplyr :: arrange(air_time)
              
              # air time of the flight relative to fastest carrier in that route
              nycflights13::flights %>%
                dplyr :: group_by(dest)%>%
                dplyr :: mutate(shortest_time = air_time- min(air_time, na.rm = T))%>%
                dplyr :: arrange(-air_time)%>%
                dplyr :: top_n(1,air_time)%>%
                dplyr :: select(shortest_time)
              
              # destinations flown by atleast 2 carriers ..
              nycflights13::flights%>%
                dplyr :: group_by(dest)%>%
                dplyr :: filter(n_distinct(carrier) >2)%>%
                dplyr :: group_by(carrier)%>%
                dplyr :: summarise(n = n_distinct(dest))%>%
                dplyr ::arrange(-n)
              