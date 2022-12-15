# Source https://github.com/sparklyr/sparklyr

# Task: Working with SPark in R\
# Date created: 15th December 2022
# Last modified on 15th December 

# devtools::install_github("sparklyr/sparklyr")
library(sparklyr)
# spark_install() # This is 219 Mbs zipped, quite big package

sc <- spark_connect(master = "local")

library(dplyr)

# Copying tables to a spark cluster

iris_tbl <- copy_to(sc, iris, overwrite = TRUE)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights", overwrite = TRUE)
batting_tbl <- copy_to(sc, Lahman::Batting, "batting", overwrite = TRUE)

src_tbls(sc)

# Filtering flights by departure and returning the first few records

flights_tbl %>% filter(dep_delay == 2)

delay <- flights_tbl %>% 
  group_by(tailnum) %>% 
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>% 
  filter(count > 20, dist < 2000, !is.na(delay)) %>% 
  collect()

library(ggplot2)

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)

# Window functions

batting_tbl %>% 
  select(playerID, yearID, teamID, G, AB:H) %>% 
  arrange(playerID, yearID, teamID) %>% 
  group_by(playerID) %>% 
  filter(min_rank(desc(H)) <= 2 & H > 0)

# Working with SQL code

library(DBI)

iris_preview <- dbGetQuery(sc, "SELECT * 
                           FROM iris 
                           LIMIT 10")
iris_preview

# Machine Learning 

mtcars_tbl <- copy_to(sc, mtcars, overwrite = TRUE)

partitions <- mtcars_tbl %>% 
  filter(hp >= 100) %>% 
  mutate(cyl8 = cyl == 8) %>% 
  sdf_random_split(trainig = 0.5, test = 0.5, seed = 1099)

# Fitting a linear model to the training dataset

fit <- partitions$trainig %>% 
  ml_linear_regression(response = "mpg", features = c("wt", "cyl"))

# Trying with the normal lm model

lm(mpg ~ wt + cyl, data = partitions$trainig) # Exactly same results

summary(fit)

# Reading and writing data

temp_csv <- tempfile(fileext = ".csv")
temp_parquet <- tempfile(fileext = ".parquet")
temp_json <- tempfile(fileext = ".json")

spark_write_csv(iris_tbl, temp_csv)
iris_csv_tbl <- spark_read_csv(sc, "iris_csv", temp_csv)

spark_write_parquet(iris_tbl, temp_parquet)
iris_parquet_tbl <- spark_read_parquet(sc, "iris_parquet", temp_parquet)

spark_write_json(iris_tbl, temp_json)
iris_json_tbl <- spark_read_json(sc, "iris_json", temp_json)

src_tbls(sc)


# Applying arbitrary function across data in cluster

spark_apply(iris_tbl, function(data){
  data[1:4] + rgamma(1, 2)
})

# Group by and perform operation

spark_apply(
  iris_tbl,
  function(e) broom::tidy(lm(Petal_Width ~ Petal_Length, e)),
  columns = c("term", "estimate", "std.error", "statistic", "p.value"),
  group_by = "Species"
)

# Caching a table

tbl_cache(sc, "batting")
tbl_uncache(sc, "batting")

# Connection utilities

spark_web(sc)
spark_log(sc, n = 10)

spark_disconnect(sc)

fit <- partitions$trainig %>% 
  ml_linear_regression(response = "mpg", features = c("wt", "cyl"))
spark_disconnect(sc)
