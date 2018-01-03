# testing for reading data .
library("tidyverse")
heights <- read_csv("a,b,c
               1,2,3
               4,5,6")
heights

# testing for skip and comments.
 heights <- read_csv("This is the meta-data on line 1
                     There may be the same here
                     x,y,z
                     1,2,3", skip=2)
 heights
 heights <- read_csv("# This is the meta-data on line 1
                     x,y,z
                     1,2,3", comment="#")
 heights
 
 # data with no col names.
 heights <- read_csv("1,2,3\n4,5,6", col_names = F)
 heights
 
 heights <- read_csv("1,2,3\n4,5,.", col_names = F, na = ".")
 heights
 
 heights <- read_delim("x,y\n1,'a,b'", delim = ",", quote = "'")
 heights
 
 # parsing a vector ..
 parse_double("1,34", locale = locale(decimal_mark = ","))
 parse_number("This mac book air costs $800")
 parse_number("123'456.789", locale = locale(grouping_mark = "'"))
 charToRaw("Saumya")
 parse_date("01/02/15", "%m/%d/%y")
 parse_date("01/02/15", "%y/%m/%d")
 parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))
 
 parse_number("123.456.789", locale = locale(decimal_mark = ","))
 
 d1 <- "January 1, 2010"
 d2 <- "2015-Mar-07"
 d4 <- c("August 19 (2015)", "July 1 (2015)")
 
parse_date(d1,"%B %d, %Y")
parse_date(d2,"%Y-%b-%d")
parse_date(d4,"%B %d ( %Y )")

# testing guess_parser
guess_parser(c("1","2","3","5"))
challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
tail(challenge)
z <- tribble(
    ~x, ~y,
    "1","1.1",
    "2", "1.2"
)
z
type_convert(z)

# writing into files using write_csv , write_tsv, write_excel_csv ( this exports the data to 
# excel into csv format)
write_csv(challenge, "challenge-2.csv")
read_csv("challenge-2.csv", 
         col_types = cols(
           x = col_double(),
           y = col_date()
         ))

# working with tidy data ...
table1 %>%
  mutate(rate = cases/population * 1000)
table1 %>%
  count(year, wt = cases)
library(ggplot2)
ggplot(table1, aes(year,cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))

table2 %>%
  group_by(year)%>%
  filter(type == "cases") # number of tb cases per country per year 
table2 %>% 
  group_by(year)%>%
  filter(type == "population") # population per country per year
table2 %>%
  group_by(year) %>%
  spread(type, count) %>%
  mutate(rate = cases/population * 1000)%>%
ggplot(aes(year,cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))

# gathering
table4a %>%
  gather(`1999`,`2000`, key = "year", value = "cases")
table4 <-bind_rows(as.data.frame(table4a), as.data.frame(table4b))
table4
tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)

# spreading..
spread(table2, key = type, value = count)

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks
stocks %>% 
  spread(year, return)  
  gather("year", "return", `2015`:`2016`)
  
  table4a %>% 
    gather(1999, 2000, key = "year", value = "cases")
  people <- tribble(
    ~name,             ~key,    ~value,
    #-----------------|--------|------
    "Phillip Woods",   "age",       45,
    "Phillip Woods",   "height",   186,
    "Phillip Woods",   "age",       50,
    "Jessica Cordero", "age",       37,
    "Jessica Cordero", "height",   156
  )
  people%>%
  spread(key,value)
  
  # separating and uniting ..
  table3 %>%
    separate(rate, into = c("cases","population"), sep = "/", convert = T)
  
# relational databases. 
  library(nycflights13)
  
  # primary keys
  planes%>%
    count(tailnum)%>%
    filter(n>1)

  # mutating joins ..  
  flights2 <- flights %>% 
    select(year:day, hour, origin, dest, tailnum, carrier)
    flights2
  flights2 %>%
    select(-origin, -dest)%>%
    left_join(airlines, by = "carrier")
  
  # average delay by destination an join on airports
 flights  %>%
    group_by(dest) %>%
    summarise(mean_delay = mean(arr_delay,na.rm = T))%>%
    inner_join(airports, by = c(dest = "faa"))%>%
   ggplot(aes(lon, lat, colour = mean_delay)) +
   borders("state") +
   geom_point() +
   coord_quickmap()
 
 # attaching location of origin and destination to flights ..
 
 flights %>%
   left_join(airports, by = c(dest = "faa"))%>%
   left_join(airports, by = c(origin = "faa"))%>%
   head()
 
 # relationship between age of planes and delays ..
 planes_ages <- planes%>%
   mutate(age = 2013- year) %>%
   select(tailnum, age)
 
 flights %>%
   inner_join(planes_ages, by = "tailnum")%>%
   group_by(age)%>%
   filter(!is.na(dep_delay))%>%
   summarise(mean_delay = mean(dep_delay))%>%
   ggplot(aes(x = age, y = mean_delay))+
   geom_point()+
   geom_line()
 
 # what whether conditions would cause more delays ??
 
 flights_weather <- flights %>%
   inner_join(weather, by = c("origin" = "origin", 
                              "year" = "year",
                              "month" = "month",
                              "day" = "day",
                              "hour" = "hour"))
 flights_weather %>%
   group_by(precip)%>%
   summarise(mean_delay = mean(dep_delay, na.rm = T)) %>%
   ggplot(aes(x = precip, y = mean_delay)) +
   geom_line()+
   geom_point()
 
 flights %>%
   anti_join(planes, by = "tailnum") %>%
   count(tailnum, sort = TRUE) 
 