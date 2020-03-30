sample_n(mydata,3)
sample_frac(mydata,0.1)
x1 = distinct(mydata)
x2 = distinct(mydata, Index, .keep_all= TRUE);View(x2)
mydata2 = select(mydata, Index, State:Y2008)
mydata10 = filter(mydata, grepl("Ar", State))
summarise(mydata,mean(Y2015), Y2015_med=median(Y2015))
summarise_at(mydata, vars(Y2005, Y2006), funs(n(), mean, median))
summarise_at(mydata, vars(Y2011, Y2012),
             funs(n(), missing = sum(is.na(.)), mean(., na.rm = TRUE), median(.,na.rm = TRUE)))
set.seed(222)
mydata <- data.frame(X1=sample(1:100,100), X2=runif(100))
summarise_at(mydata,vars(X1,X2), function(x) mean(x - mean(x)))
mydata = read_csv("sampledata1.csv")
summarise_if(mydata, is.numeric, funs(n(),mean,median))
a=summarise_if(mydata, is.numeric, funs(n(),mean,median))
library(dplyr)
summarise_all(mydata,Index, funs(nlevels(.), nmiss=sum(is.na(.))))
mydata %>% group_by(Index) %>%
  summarise_at(vars(Y2011:Y2015), funs(n(), mean(., na.rm = TRUE)))

mydata %>% filter(Index %in% c("A", "C","I"))  %>%arrange(Index,desc(Y2002))
mydata %>% select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  do(arrange(.,desc(Y2015))) %>%  slice(3)

mydata %>%
  group_by(Index)%>%
  summarise(Mean_2014 = mean(Y2014, na.rm=TRUE),
            Mean_2015 = mean(Y2015, na.rm=TRUE)) %>%
  arrange(desc(Mean_2015))

mydata13 = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(desc(.))))

out = mydata %>% group_by(Index) %>% filter(min_rank(desc(Y2015)) == 1) %>%
  select(Index, State, Y2015)

mydata %>% group_by(Index) %>% mutate(Total=cumsum(Y2015)) %>%
  select(Index, Y2015, Total)

df =data.frame(x = c(1,5,6,NA))
df %>% mutate(newvar=if_else(x<5, x+1, x+2,0))


mydf =data.frame(x = c(1:5,NA))
mydf %>% mutate(newvar= if_else(is.na(x),"I am missing",
                                if_else(x==1,"I am one",
                                        if_else(x==2,"I am two",
                                                if_else(x==3,"I am three","Others")))))

mydf %>% mutate(flag = case_when(is.na(x) ~ "I am missing",
                                 x == 1 ~ "I am one",
                                 x == 2 ~ "I am two",
                                 x == 3 ~ "I am three",
                                 TRUE ~ "Others"))