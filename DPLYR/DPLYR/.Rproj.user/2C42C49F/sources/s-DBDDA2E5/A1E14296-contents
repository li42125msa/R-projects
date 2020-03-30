library(data.table);library(readr)

netflix <- fread("netflix_2017.csv")
imdb <- fread("imdb_ratings.csv")

# What data.tables are in my R session?
tables()

# View the first six rows 
head(netflix)
head(imdb)

# Print the structure
str(netflix)
str(imdb)

merge(x = demographics, y = shipping, 
      by.x = "name", by.y = "name")

merge(x = demographics, y = shipping, 
      by = "name")

merge(x = demographics, y = shipping, 
      by = "name", all = TRUE)

# Full join netflix and imdb
merge(netflix, imdb, by = "title", all = TRUE)

merge(x = demographics, y = shipping, by = "name", all.x = TRUE)

# Left join imdb to netflix
merge(netflix, imdb, by = "title", all.x = TRUE)

# Identify the key for joining capitals and population
capitals_population_key <- "city"

# Left join population to capitals
capital_pop <- merge(capitals, population, by = capitals_population_key, all.x = TRUE)
capital_pop

# Right join population to capitals using data.table syntax
capitals[population, on = .(city)]

# Inner join with the data.table syntax
capitals[population, on = .(city), nomatch = 0]

# Anti-join capitals to population
population[!capitals, on = .(city)]

# Set the keys
setkey(netflix, "title")
setkey(imdb, "title")

# Inner join
netflix[imdb, nomatch = 0]

# Check for keys
haskey(netflix)
haskey(imdb)

# Find the key
the_key <- key(netflix)

# Set the key for the other data.table
setkeyv(imdb, the_key)

# Inner join capitals to population
population[capitals, on = .(city), nomatch = 0]

customers <- data.table(name   = c("Mark", "Matt", "Angela", "Michelle"), 
                        gender = c("M", "M", "F", "F"), 
                        age    = c(54, 43, 39, 63))

purchases <- data.table(name  = c("Mark", "Matt", "Angela", "Michelle"),
                        sales = c(1, 5, 4, 3),
                        spent = c(41.70, 41.78, 50.77, 60.01))

customers[purchases, 
          on = .(name)][sales > 1, 
                        j = .(avg_spent = sum(spent) / sum(sales)), 
                        by = .(gender)]

# Complete the code to build a data.table containing the number of matches in continents for each row in life_exp.

# How many continents is each country listed in?
continents[life_exp, on = .(country), .N, 
           by = .EACHI]

# Calculate average life expectancy per continent:
avg_life_expectancy <- continents[life_exp, on = .(country), 
                                  nomatch = 0][, j = mean(years), 
                                               by = .(continent)]
avg_life_expectancy

customers[web_visits, on = .(name = person)]

customers[web_visits, on = c("name" = "person")]

key <- c("name" = "person")
customers[web_visits, on = key]

#multiple keys with merge()
merge(purchases, web_visits, by = c("name", "date"))

merge(purchases, web_visits, 
      by.x = c("name", "date"),  
      by.y = c("person", "date")
      
#data.table syntax
purchases[web_visits, on = c("name", "date")]

purchases[web_visits, on = .(name = person, date)]
purchases[web_visits, on = c("name" = "person", "date")]

# Full join
merge(students, guardians, by = "name", all = TRUE)

# Right join
subjects[locations, on = .(subject, semester)]

# Identify and set the keys
join_key <- c("topic" = "subject")

# Right join
teachers[locations, on = join_key]

parents <- as.data.table(parents, keep.rownames = "parent")
parents

# Inner join
capital_pop <- merge(capitals, population, by = "city")

# Left join
merge(capital_pop, area, by = "state", all.x = TRUE)

# Convert netflix to a data.table
netflix_dt <- as.data.table(netflix, keep.rownames = "series")

# Right join
imdb[netflix_dt, on = .(title = series)]

# data.table syntax
site1_ecology[site2_ecology, on = .(genus), allow.cartesian = TRUE]

# merge()
merge(site1_ecology, site2_ecology, by = "genus", allow.cartesian = TRUE)

# Try an inner join
merge(heart, cardio, by = "gene", allow.cartesian = TRUE)

# Filter missing values
heart_2 <- heart[!is.na(gene)]
cardio_2 <- cardio[!is.na(gene)]

# Inner join the filtered data.tables
merge(heart_2, cardio_2, by = "gene")

# Keep only the last probe for each gene
heart_3 <- unique(heart_2, by = "gene", fromLast = TRUE)
cardio_3 <- unique(cardio_2, by = "gene", fromLast = TRUE)

# Inner join
reproducible <- merge(heart_3, cardio_3, by = "gene", suffixes = c(".heart", ".cardio"))
reproducible

# Right join taking the first match
heart_2[framingham, on = .(gene), mult = "first"]

# Anti-join
reproducible[!framingham, on = .(gene)]


rbind("2015" = sales_2015, "2016" = sales_2016, idcol = "year")
rbind(sales_2015, sales_2016, idcol = TRUE)


rbind("2015" = sales_2015, "2016" = sales_2016, idcol = "year", 
      fill = TRUE)


# Read in a list of data.tables
table_files <- c("sales_2015.csv", "sales_2016.csv")
list_of_tables <- lapply(table_files, fread)


names(list_of_tables) <- c("2015", "2016")
rbindlist(list_of_tables, idcol = "year")

rbind("2015" = sales_2015, "2016" = sales_2016, idcol = "year", 
      use.names = TRUE)


# Concatenate case numbers from weeks 50 and 51
rbind(ebola_W50, ebola_W51)

# Concatenate its data.tables
gdp_all_1 <- rbindlist(gdp)


fintersect(dt1, dt2)

fintersect(dt1, dt2, all = TRUE)

fsetdiff(dt1, dt2)
funion(dt1, dt2)

funion(dt1, dt2, all = TRUE) # rbind() only keep unique

# Obtain countries in both Asia and Europe
fintersect(gdp$asia, gdp$europe)

# Concatenate all data tables
gdp_all <- rbindlist(gdp)

# Find all countries that span multiple continents
gdp_all[duplicated(gdp_all)]

# Get all countries in either Asia or Europe
funion(gdp$asia, gdp$europe)

# Concatenate all data tables
gdp_all <- rbindlist(gdp)

# Print all unique countries
unique(gdp_all)

# Which countries are in Africa but not considered part of the middle east?
fsetdiff(gdp$africa, middle_east)

# Which countries are in Asia but not considered part of the middle east?
fsetdiff(gdp$asia, middle_east)

# Which countries are in Europe but not considered part of the middle east?
fsetdiff(gdp$europe, middle_east)


# Print gdp_per_capita
gdp_per_capita

# Reshape gdp_per_capita to the long format
melt(gdp_per_capita, id.vars = "year")

melt(ebola_wide, measure.vars = c("Week_50", "Week_51"), 
     variable.name = "period", value.name = "cases")


mydata = fread("https://github.com/arunsrinivasan/satrdays-workshop/raw/master/flights_2014.csv")

dat1 = mydata[ , origin] # returns a vector
dat1 = mydata[ , .(origin)] # returns a data.table

dat3 = mydata[, .(origin, year, month, hour)]

dat4 = mydata[, c(2:4), with=FALSE]

dat6 = mydata[, !c("origin", "year", "month"), with=FALSE]
dat7 = mydata[,names(mydata) %like% "dep", with=FALSE]

setnames(mydata, c("dest","origin"), c("Destination", "origin.of.flight"))

dat8 = mydata[origin == "JFK"]

dat9 = mydata[origin %in% c("JFK", "LGA")]

dat11 = mydata[origin == "JFK" & carrier == "AA"]

# Indexing (Set Keys)
setkey(mydata, origin)
data12 = mydata[c("JFK", "LGA")]

setkey(mydata, origin, dest)
# First key column 'origin' matches “JFK” and second key column 'dest' matches “MIA”
mydata[.("JFK", "MIA")]
mydata[origin == "JFK" & dest == "MIA"]

mydata01 = setorder(mydata, origin)

mydata02 = setorder(mydata, -origin)
mydata03 = setorder(mydata, origin, -carrier)

mydata[, dep_sch:=dep_time - dep_delay]

mydata[, c("dep_sch","arr_sch"):=list(dep_time - dep_delay, arr_time - arr_delay)]

mydata[, dep_sch:=dep_time - dep_delay][,.(dep_time,dep_delay,dep_sch)]

setDF(mydata)

set.seed(123)
X = data.frame(A=sample(3, 10, TRUE),
               B=sample(letters[1:3], 10, TRUE)
               setDT(X, key = "A")
               setDT(X, key = "A")

mydata[, .(mean = mean(arr_delay, na.rm = TRUE),
           median = median(arr_delay, na.rm = TRUE),
           min = min(arr_delay, na.rm = TRUE),
           max = max(arr_delay, na.rm = TRUE))]

dt = mydata[, rank:=frank(-distance,ties.method = "min"), by=carrier]
# In this case, we are calculating rank of variable 'distance' by 'carrier'. We are assigning rank 1 to the highest value of 'distance' within unique values of 'carrier'.

dat = mydata[, cum:=cumsum(distance), by=carrier]

DT <- data.table(A=1:5)
DT[ , X := shift(A, 1, type="lag")]
DT[ , Y := shift(A, 1, type="lead")]
