




library(ggplot2)
ggplot(data  = iris, aes( x = Sepal.Length)) + geom_histogram( bins=20)
ggplot(data = iris , aes(x=Sepal.Length)) + geom_histogram(color="black", fill="blue", bins = 10) 
ggplot(iris, aes(x=Sepal.Length, color=Species)) + geom_histogram(fill="white", binwidth = 1)
ggplot(iris, aes( x = Sepal.Length)) + geom_density( )
ggplot(iris, aes(x=Sepal.Length, color=Species)) + geom_density( )
ggplot(mpg, aes(x= class)) + geom_bar() 
ggplot(mpg, aes(x= class)) + geom_bar() + coord_flip()


p = ggplot(mpg, aes(x= class)) + geom_bar() 
p + labs(title = "Number of Cars in each type", x = "Type of car", y = "Number of cars")+coord_flip()


p = ggplot(mpg, aes(x= class)) + geom_bar()
p = p + labs(title = "Number of Cars in each type", x = "Type of car", y = "Number of cars")
p + geom_text(stat='count', aes(label=..count..), vjust=-1)    
         
library(plyr)
library(dplyr)
count(mpg,class) %>% arrange(-n) %>%
  mutate(class = factor(class,levels= class)) %>%
  ggplot(aes(x=class, y=n)) + geom_bar(stat="identity")

mpg
df = mpg %>% group_by(class) %>% summarise(mean = mean(displ)) %>%
  arrange(-mean) %>% mutate(class = factor(class,levels= class))

p = ggplot(df, aes(x=class, y=mean)) + geom_bar(stat="identity")
p + geom_text(aes(label = sprintf("%0.2f", round(mean, digits = 2))),
              vjust=1.6, color="white", fontface = "bold", size=4)


p <- ggplot(data=mpg, aes(x=class, y=displ, fill=drv))
p + geom_bar(stat = "identity")


p + geom_bar(stat="identity", position=position_dodge())


mtcars$cyl = factor(mtcars$cyl)
ggplot(mtcars, aes(x=cyl, y=disp)) + geom_boxplot()


# Creating a scatter plot denoting various species.
ggplot(data = iris, aes( x = Sepal.Length, y = Sepal.Width,shape = Species, color = Species)) + geom_point()

ggplot(data = subset(mtcars,am == 0),aes(x = mpg,y = disp,colour = factor(cyl))) + geom_point()

ggplot(data = mtcars, aes(x = mpg,y = disp,colour = hp))  + geom_point() + geom_smooth()

# Plotting the horsepower using geom_line
ggplot(data = mtcars, aes(x = mpg,y = disp,colour = hp))  + geom_point(size = 2.5) + geom_line(aes(y = hp))+ geom_smooth()
ggplot(mtcars,aes(x = mpg,y = disp)) + geom_point() + labs(title = "Scatter plot") 
a <- ggplot(mtcars,aes(x = mpg, y = disp, color = factor(cyl))) + geom_point()
a + labs(color = "Cylinders") + xlab("Mileage") + ylab("Displacement") + ggtitle(label = "Scatter plot", subtitle = "mtcars data in R")


#Changing the themes.
ggplot(mtcars,aes(mpg,disp)) + geom_point()  + labs(title = "Scatter Plot") + theme(plot.title = element_text(color = "blue",size = 17),plot.background = element_rect("orange"))
x = "Deepanshu"
y ="Bhalla"
paste(x, y)
paste("x", seq(1,10), sep = "")
x = 0.25
sprintf("%.0f%%",x*100)
sprintf("%.2f%%",x*100)
