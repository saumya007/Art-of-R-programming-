# basic plots in ggplot . 

# loading mpg dataset and plotting using ggplot
ggplot(data = mpg)+geom_point(mapping = aes(x = drv, y = class))

# asthetic mappigns for visualizing classes of points. So they can be distinguished from the class theu belong
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy,color = class))

# mapping size aesthetic with classes
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy,size = class))

# alpha aesthetic mapping with class
ggplot(data = mpg) + geom_point(mapping = aes(x = displ,  y = hwy, alpha = class))

# mapping shape aesthetic to the class

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy),color = "blue") # when mapping ...
#  ... color , write it outside asthetic . Color will be applied on the whole mapping 
#  .. and not inside asthetic function.

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class, alpha = class, size = class))

# stroke asthetics
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, stroke = displ), shape = 21)

# mapping aesthetic to non variable names
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy,color = displ <5))

# faceting the data with facet_wrap
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5)) + 
  facet_wrap(~hwy,nrow = 2)

# facetign data using facet_grid
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy , color = displ < 5)) +
  facet_grid(drv ~ cyl)

# geom_smooth geom
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy))

# line type aesthetic in geom
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

# grouping in geom 
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

# color in geom
ggplot(data = mpg)+ geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))

# multiple geoms in single plot
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# global mappings for multiple geoms.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point()+
  geom_smooth()

# local and global mappings
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv))+
  geom_smooth()

# using filter in geoms
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

# line chart, boxplot, histogram, area chart
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_line(mapping = aes(color = drv))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_boxplot(mapping = aes(color = drv)) +
  geom_area(mapping =aes(color = drv))

# code for graph generation in ex 3.6

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(size = 4, color = "white") + 
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(se = FALSE, mapping = aes(linetype = drv))

# statistical transformations testing
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

# over riding default stat
demo <- tribble(
  ~cut, ~freq,
  "Fair",1610,
  "Good",4906,
  "Very Good",12081,"Premium", 13791,"Ideal",21551)
ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq),stat = "identity")

#over ride default mapping from transformed variables to asthetics
ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut , y = ..prop..,group = 1))

# generating summary of statstical transformations ..
ggplot(data = demo)+
stat_summary(mapping = aes(x = cut, y = depth),
             fun.ymin = min,
             fun.ymax = max,
             fun.y = median
             )

# color , fill asthetics
ggplot(data = demo)+
geom_bar(mapping = aes(x = cut, color = cut))
ggplot(data = demo)+
  geom_bar(mapping = aes(x = cut, fill = cut))
ggplot(data = diamonds, mapping = aes(x = cut, color = clarity))+
  geom_bar(fill = NA,position = "identity")

# jitter
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

# positional arguments exercise
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point(position ="jitter", alpha = 0.5) +
  geom_smooth(method ="lm")

ggplot(data = mpg, mapping = aes(x=drv, y=cty, fill=as.factor(cyl))) +
  geom_boxplot(position = "dodge")

# testing coordinates flip

ggplot(data = mpg, mapping = aes(x=drv, y=cty, fill=as.factor(cyl))) +
  geom_boxplot(position = "dodge") +
  coord_flip() +
  labs(fill = "cylinders")

# testing coordinates with quick map for maps

# without quick map
nz <- map_data("nz")
ggplot(data = nz, mapping = aes(long, lat, group = group))+
  geom_polygon(fill = "white", color = "black") +
  coord_polar()

# with quick map
nz <- map_data("nz")
ggplot(data = nz, mapping = aes(long, lat, group = group))+
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

# geom_abline plots
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_abline() +
  coord_fixed()
