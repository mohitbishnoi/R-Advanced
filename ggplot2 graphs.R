#Graphical Analysis using R  - using ggplot2 package
# Creating a graph
# Density plots
# Dot plots
# Bar plots, Stacked bar plot , Group Bar Plot
# Histograms
# Line plots
# Pie chart
# Box plot
# Scatter plots

install.packages("ggplot2")
library(ggplot2)

data(mtcars)
View(mtcars)

#creating scatter plots
#using default function
plot(mtcars$wt,mtcars$mpg)

#using ggplot2 function
qplot(mtcars$wt,mtcars$mpg)
#qplot - is short for quick plot

#big difference when using qplot instead of plot comes when you
#want to assign colours or sizes or shapes to the points on your plot. With
#plot, its your responsibility to convert a categorical variable in your data
#(e.g., apples, bananas, pears) into something that plot knows how to
#use (e.g., red, yellow, green). qplot can do this for you automatically,
#and it will automatically provide a legend that maps the displayed attributes
#to the data values. This makes it easy to include additional data on the plot.

data("diamonds")
str(diamonds)
View(diamonds)

qqplot(diamonds$carat,diamonds$price)

#Creating Sample
set.seed(3) # Make the sample reproducible
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
View(dsmall)

qplot(carat, price, data = dsmall, colour = color)
qplot(carat, price, data = dsmall, shape = cut)

ggplot(dsmall,aes(carat,price))+geom_point()

#first define x and y axis variables that will be done by aestheic property
#second geometry of graph - type of graph that you wan to draw

p<-ggplot(dsmall,aes(carat,price))

p+geom_point()

p+geom_point()+geom_smooth()


#Colour, size and shape are all examples of aesthetic attributes, visual
#properties that affect the way observations are displayed. 
#For every aesthetic attribute, there is a function, 
#called a scale, which maps data values to valid values for that aesthetic.

qplot(carat, price, data = dsmall, geom = c("point", "smooth"))

#qplot is not limited to scatterplots, but can produce almost any kind of plot
#by varying the geom. Geom, short for geometric object, describes the type
#of object that is used to display the data.

#Boxplots - is used to know how the values of the continuous variables vary 
#with the levels of the categorical variable. 
#Boxplots and jittered points offer two ways to do this.

qplot(color, price / carat, data = diamonds,geom = "jitter")
qplot(color, price / carat, data = diamonds,geom = "boxplot")


#Histogram and density plots
#Histogram and density plots show the distribution of a single variable. They
#provide more information about the distribution of a single group than boxplots
#do, but it is harder to compare many groups

qplot(carat, data = diamonds, geom = "histogram")
qplot(carat, data = diamonds, geom = "density")

qplot(carat, data = diamonds, geom = "histogram", binwidth = 1,
      xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1,
      xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01,
      xlim = c(0,3))

#To compare the distributions of different subgroups, just add an aesthetic
#mapping, as in the following code.
qplot(carat, data = diamonds, geom = "density", colour = color)
qplot(carat, data = diamonds, geom = "histogram", fill = color)

#Bar charts
#The discrete analogue of histogram is the bar chart, geom = "bar". The bar
#geom counts the number of instances of each class so that you don???t need to
#tabulate your values beforehand, as with barchart in base R. If the data has
#already been tabulated or if you???d like to tabulate class members in some other
#way, such as by summing up a continuous variable, you can use the weight
#geom. 

qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) +
  scale_y_continuous("carat")

#group bar charts
qplot(x=color, data = diamonds)+facet_grid(~cut)

qplot(x=color, data = diamonds)+facet_grid(~cut)+geom_bar(aes(fill=clarity))

#Line chart
#Line and path plots are typically used for time series data. Line plots join the
#points from left to right, while path plots join them in the order that they
#appear in the dataset
data(economics)
View(economics)
nrow(economics)
qplot(date, unemploy / pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")

year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy / pop, uempmed, data = economics,
      geom = c("point", "path"))
qplot(unemploy / pop, uempmed, data = economics,
      geom = "path", colour = year(date))

# Stacked barchart
(pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1))

# Pie chart - to check contribution of categorial in total
pie + coord_polar(theta = "y")
# The bullseye chart
pie + coord_polar()

#dot plot - is a statistical chart consisting of data points plotted on a fairly 
#simple scale, typically using filled in circles.
View(mpg)
mpg2 <- subset(mpg, cyl != 5 & drv %in% c("4", "f"))
mpg3 <- within(mpg2, {
  model <- reorder(model, cty)
  manufacturer <- reorder(manufacturer, -cty)
})

View(mpg)
models <- qplot(cty, model, data = mpg3)
models

qplot(cty, manufacturer, data = mpg3)
