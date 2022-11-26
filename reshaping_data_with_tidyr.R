# source: https://rpubs.com/cliex159/866648 -----

netflix_df <- readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/netflix_duration.rds")
netflix_df %>% 
  # Split the duration column into value and unit columns
  separate(duration, into = c("value", "unit"), sep = " ", convert = TRUE)

phone_nr_df=tribble(~country,   ~country_code,  ~national_number,
"USA",  "+1",   "2025550117",
"United Kingdom",   "+44",  "1632960924",
"Brazil",   "+55",  "95552452220",
"Australia",    "+61",  "1900654321",
"China",    "+86",  "13555953217",
"India",    "+91",  "8555843898")
phone_nr_df %>%
  # Unite the country_code and national_number columns
  unite("international_number", country_code, national_number, sep = "")

tvshow_df=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/netflix_series.rds")
tvshow_df

tvshow_df %>% 
  # Separate the actors in the cast column over multiple rows
  separate_rows(cast, sep = ", ") %>% 
  rename(actor = cast) %>% 
  count(actor, sort = TRUE) %>% 
  head()

drink_df=tribble(~drink, ~ingredients,
"Chocolate milk", "milk 0.3 L; chocolate 40 g; sugar 10 g",
"Orange juice", "oranges 3; sugar 20 g", 
"Cappuccino", "milk 0.1 L; water 0.1 L; coffee 30 g; sugar 5 g")
drink_df %>% 
  # Separate the ingredients over rows
  separate_rows(ingredients, sep = "; ")

drink_df %>% 
  # Separate the ingredients over rows
  separate_rows(ingredients, sep = "; ") %>% 
  # Separate ingredients into three columns
  separate(
    ingredients, 
    into = c("ingredient", "quantity", "unit"), 
    sep = " ", 
    convert = TRUE
  )

drink_df %>% 
  # Separate the ingredients over rows
  separate_rows(ingredients, sep = "; ") %>% 
  # Separate ingredients into three columns
  separate(
    ingredients, 
    into = c("ingredient", "quantity", "unit"), 
    sep = " ", 
    convert = TRUE
  ) %>% 
  # Group by ingredient and unit
  group_by(ingredient, unit) %>% 
  # Calculate the total quantity of each ingredient
  summarize(quantity = sum(quantity))

director_df=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/netflix_directors.rds")
director_df %>% 
  # Spread the director column over separate rows
  separate_rows(director, sep = ", ")

director_df %>% 
  # Spread the director column over separate rows
  separate_rows(director, sep = ", ") %>% 
  # Count the number of movies per director
  count(director, sort = TRUE)

director_df %>% 
  # Drop rows with NA values in the director column
  drop_na(director) %>% 
  # Spread the director column over separate rows
  separate_rows(director, sep = ", ") %>% 
  # Count the number of movies per director
  count(director, sort = TRUE)

sales_df=tribble(~year,~quarter,~sales,
NA,"Q1","12498",
NA,"Q2","20461",
NA,"Q3","19737",
"2019","Q4","20314",
NA,"Q1","13494",
NA,"Q2","19314",
NA,"Q3","23640",
"2020","Q4","22920")
sales_df=sales_df %>% mutate(year=as.factor(year))

sales_df %>% 
  # Impute the year column
  fill(year, .direction = "up") %>%
  # Create a line plot with sales per quarter colored by year.
  ggplot(aes(x = quarter, y = sales, color = year, group = year)) +
  geom_line()

country_to_continent_df=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRPCvCEPOCd5KrjlVF_iXe2oDxIiXM_2wfoJzp-Za-5PusRoqUy3dkYJ22HCyyrBmT1KMha82C_SPeP/pub?gid=1254696327&single=true&output=csv")
nuke_df=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRpPo4a7Uj9jAtdmEC3p1YixmewdMQKKsKbuu3CoF4COKX7j-y0FpiRzxLGdaWVmb7mrvWOWEaph558/pub?gid=0&single=true&output=csv")
country_to_continent_df %>% 
  left_join(nuke_df, by = "country_code") %>% 
  # Impute the missing values in the n_bombs column with 0L
  replace_na(list(n_bombs = 0L))

country_to_continent_df %>% 
  left_join(nuke_df, by = "country_code") %>% 
  # Impute the missing values in the n_bombs column with 0L
  replace_na(list(n_bombs = 0L)) %>% 
  # Group the dataset by continent
  group_by(continent) %>% 
  # Sum the number of bombs per continent
  summarize(n_bombs_continent = sum(n_bombs))

country_to_continent_df %>% 
  left_join(nuke_df, by = "country_code") %>%  
  # Impute the missing values in the n_bombs column with 0L
  replace_na(list(n_bombs = 0L)) %>% 
  # Group the dataset by continent
  group_by(continent) %>% 
  # Sum the number of bombs per continent
  summarize(n_bombs_continent = sum(n_bombs)) %>% 
  # Plot the number of bombs per continent
  ggplot(aes(x = continent, y = n_bombs_continent)) +
  geom_col()

nuke_df=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/nuke_wide_country.rds")
nuke_df %>% 
  # Pivot the data to a longer format
  pivot_longer(-year)

nuke_df %>% 
  # Pivot the data to a longer format
  pivot_longer(
    -year, 
    # Overwrite the names of the two new columns
    names_to = "country", 
    values_to = "n_bombs"
  ) 

nuke_df %>% 
  # Pivot the data to a longer format
  pivot_longer(
    -year, 
    # Overwrite the names of the two new columns
    names_to = "country", 
    values_to = "n_bombs"
  ) %>% 
  # Replace NA values for n_bombs with 0L
  replace_na(list(n_bombs = 0L))

nuke_df %>% 
  # Pivot the data to a longer format
  pivot_longer(
    -year, 
    # Overwrite the names of the two new columns
    names_to = "country", 
    values_to = "n_bombs"
  ) %>% 
  # Replace NA values for n_bombs with 0L
  replace_na(list(n_bombs = 0L)) %>% 
  # Plot the number of bombs per country over time
  ggplot(aes(x = year, y = n_bombs, color = country)) +
  geom_line()

obesity_df=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRpPo4a7Uj9jAtdmEC3p1YixmewdMQKKsKbuu3CoF4COKX7j-y0FpiRzxLGdaWVmb7mrvWOWEaph558/pub?gid=278516960&single=true&output=csv")
obesity_df %>% 
  # Pivot the male and female columns
  pivot_longer(c(male, female),
               names_to = "sex",
               values_to = "pct_obese")

obesity_df %>% 
  # Pivot the male and female columns
  pivot_longer(c(male, female),
               names_to = "sex",
               values_to = "pct_obese") %>% 
  # Create a scatter plot with pct_obese per country colored by sex
  ggplot(aes(x = pct_obese, color = sex,
             y = forcats::fct_reorder(country, both_sexes))) +
  geom_point() +
  scale_y_discrete(breaks = c("India", "Nauru", "Cuba", "Brazil",
                              "Pakistan", "Gabon", "Italy", "Oman",
                              "China", "United States of America")) +
  labs(x = "% Obese", y = "Country")

bond_df=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRpPo4a7Uj9jAtdmEC3p1YixmewdMQKKsKbuu3CoF4COKX7j-y0FpiRzxLGdaWVmb7mrvWOWEaph558/pub?gid=595802601&single=true&output=csv")

colnames(bond_df)=c("Bond",1960,    1970,   1980,   1990,   2000,   2010,   2020)
bond_df %>% 
  # Pivot the data to long format and set the column names
  pivot_longer(
    -Bond, 
    names_to = "decade", 
    values_to = "n_movies"
  )

bond_df %>% 
  # Pivot the data to long format
  pivot_longer(
    -Bond, 
    # Overwrite the names of the two newly created columns
    names_to = "decade", 
    values_to = "n_movies", 
    # Drop na values
    values_drop_na = TRUE
  )

bond_df %>% 
  # Pivot the data to long format
  pivot_longer(
    -Bond, 
    # Overwrite the names of the two newly created columns
    names_to = "decade", 
    values_to = "n_movies", 
    # Drop na values
    values_drop_na = TRUE, 
    # Transform the decade column data type to integer
    names_transform = list(decade = as.integer)
  ) %>% 
  ggplot(aes(x = decade + 5, y = n_movies, fill = Bond))+
  geom_col()

bird_df=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQi_yI_hSJxlAZpWoqIrp8F9b-g6PLT0snajIporAW2hw8beu90XVaoL_SgfmZ0NAmSRqJyHcCNghPO/pub?gid=1305911804&single=true&output=csv")
bird_df %>%
  # Pivot the data to create a two column data frame
  pivot_longer(
    starts_with("points_"),
    names_to = "points",
    names_prefix = "points_",
    names_transform = list(points = as.integer),
    values_to = "species",
    values_drop_na = TRUE
  )

bird_df %>%
  # Pivot the data to create a 2 column data frame
  pivot_longer(
    starts_with("points_"),
    names_to = "points",
    names_prefix = "points_",
    names_transform = list(points = as.integer),
    values_to = "species",
    values_drop_na = TRUE
  ) %>%
  group_by(species) %>% 
  summarize(total_points = sum(points)) %>% 
  slice_max(total_points, n = 5)

stock_df=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRpPo4a7Uj9jAtdmEC3p1YixmewdMQKKsKbuu3CoF4COKX7j-y0FpiRzxLGdaWVmb7mrvWOWEaph558/pub?gid=1049805716&single=true&output=csv")
colnames(stock_df)=c("company",paste(2019,"_week",seq(1:52),sep=""),paste(2020,"_week",seq(1:53),sep=""))

stock_df %>% 
  # Pivot the data to create 3 new columns: year, week, price
  pivot_longer(
    -company,
    names_to = c("year", "week"),
    values_to = "price",
    names_sep = "_week",
    names_transform = list(
      year = as.integer,
      week = as.integer)
  )

stock_df %>% 
  # Pivot the data to create 3 new columns: year, week, price
  pivot_longer(
    -company,
    names_to = c("year", "week"),
    values_to = "price",
    names_sep = "_week",
    names_transform = list(
      year = as.integer,
      week = as.integer)
  ) %>%
  # Create a line plot with price per week, color by company
  ggplot(aes(x = week, y = price, color = company)) +
  geom_line() +
  facet_grid(. ~ year)

space_dogs_df=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrv7ECpC0Jwv5u4Y5KZFZOGJ2SnEruSs7xoGs_ZPLbLS5CI32JVRLAxpv0dyneZovOOQ_jyaqcZeoh/pub?gid=0&single=true&output=csv")
space_dogs_df %>% 
  pivot_longer(
    # Add the columns to pivot
    name_1:gender_2,
    names_sep = "_",
    # Complete the names_to argument to re-use the first part of the column headers
    names_to = c(".value", "dog_id"),
    # Make sure NA values are dropped
    values_drop_na = TRUE
  )

#who_df <- read.csv("https://raw.githubusercontent.com/Vishal0229/Data605/master/Week12/who.csv", header =T)
# who_df %>% 
#   # Put each variable in its own column
#   pivot_longer(
#     -country,
#     names_to = c("year", "sex", ".value"),
#     names_sep = "_", 
#     names_transform = list("year" = as.integer)
#   )

# who_df %>% 
#   # Put each variable in its own column
#   pivot_longer(
#     -country,
#     names_to = c("year", "sex", ".value"),
#     names_sep = "_", 
#     names_transform = list("year" = as.integer)
#   ) %>%
#   # Create a plot with life expectancy over obesity
#   ggplot(aes(x = pct.obese, y = life.exp, color = sex))+
#   geom_point()

 dog_df=tribble(~breed,            ~n_participants,
"Husky",                         5,
"Golden retriever",             12,
"Poodle",                       13,
"Shiba Inu",                     7)
dog_df %>% 
  # Create one row for each participant and add the id
  uncount(n_participants, .id = "dog_id")

space_dogs_df1=space_dogs_df %>% 
  pivot_longer(
    # Add the columns to pivot
    name_1:gender_2,
    names_sep = "_",
    # Complete the names_to argument to re-use the first part of the column headers
    names_to = c(".value", "dog_id"),
    # Make sure NA values are dropped
    values_drop_na = TRUE
  )

space_dogs_df1 %>% 
  # Pivot the data to a wider format
  pivot_wider(
    names_from = dog_id, 
              values_from = gender, 
              names_prefix = 'gender_'
    )

space_dogs_df=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrv7ECpC0Jwv5u4Y5KZFZOGJ2SnEruSs7xoGs_ZPLbLS5CI32JVRLAxpv0dyneZovOOQ_jyaqcZeoh/pub?gid=1588863802&single=true&output=csv")
space_dogs_df %>% 
  # Pivot the data to a wider format
  pivot_wider(names_from = dog_id, values_from = gender, names_prefix = "gender_") %>% 
  # Drop rows with NA values
  drop_na() 

space_dogs_df %>% 
  # Pivot the data to a wider format
  pivot_wider(names_from = dog_id, values_from = gender, names_prefix = "gender_") %>% 
  # Drop rows with NA values
  drop_na() %>% 
  # Create a Boolean column on whether both dogs have the same gender
  mutate(same_gender = gender_1 == gender_2) %>% 
  summarize(pct_same_gender = mean(same_gender))

planet_df=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/planet_metrics_long.rds")
planet_df %>% 
  # Give each planet variable its own column
  pivot_wider(names_from = "metric", values_from = "value")

planet_df %>% 
  # Give each planet variable its own column
  pivot_wider(names_from = "metric", values_from = "value") %>% 
  # Plot planet temperature over distance to sun
  ggplot(aes(x = distance_to_sun, y = temperature)) +
  geom_point(aes(size = diameter)) +
  geom_text(aes(label = planet), vjust = -1) +
  labs(x = "Distance to sun (million km)", 
       y = "Mean temperature (°C)") +
  theme(legend.position = "none")

planet_df=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/planet_metrics_wide.rds")

planet_df %>%
  # Pivot all columns except metric to long format
  pivot_longer(-metric, names_to = "planet")

planet_df %>%
  # Pivot all columns except metric to long format
  pivot_longer(-metric, names_to = "planet") %>% 
  # Put each metric in its own column
  pivot_wider(names_from = metric, values_from = value)

planet_df %>%
  # Pivot all columns except metric to long format
  pivot_longer(-metric, names_to = "planet") %>% 
  # Put each metric in its own column
  pivot_wider(names_from = metric, values_from = value) %>% 
  # Plot the number of moons vs planet diameter
  ggplot(aes(x = diameter, y = number_of_moons)) +
  geom_point(aes(size = diameter)) +
  geom_text(aes(label = planet), vjust = -1) +
  labs(x = "Diameter (km)", y = "Number of moons") +
  theme(legend.position = "none")

letters <- c("A", "C", "G", "U")

# Create a tibble with all possible 3 way combinations
codon_df <- expand_grid(
  letter1 = letters,
  letter2 = letters,
  letter3 = letters
)

codon_df

letters <- c("A", "C", "G", "U")

# Create a tibble with all possible 3 way combinations
codon_df <- expand_grid(
  letter1 = letters,
  letter2 = letters,
  letter3 = letters
)

codon_df %>%  
  # Unite these three columns into a "codon" column
  unite("codon", letter1:letter3, sep= "")

# Create a tibble with all combinations of years and species
full_df <- expand_grid(
  year = 1951:1970, 
  species = c("Human", "Dog")
)

full_df

space_df=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrv7ECpC0Jwv5u4Y5KZFZOGJ2SnEruSs7xoGs_ZPLbLS5CI32JVRLAxpv0dyneZovOOQ_jyaqcZeoh/pub?gid=1478550685&single=true&output=csv")
space_df %>% 
  # Join with full_df so that missing values are introduced
  right_join(full_df, by = c("year", "species")) %>% 
  arrange(year)

space_df %>% 
  # Join with full_df so that missing values are introduced
  right_join(full_df, by = c("year", "species")) %>% 
  # Create a line plot with n_in_space over year, color by species
  ggplot(aes(x = year, y = n_in_space, color = species)) +
  geom_line()

space_df %>% 
  # Join with full_df so that missing values are introduced
  right_join(full_df, by = c("year", "species")) %>% 
  # Overwrite NA values for n_in_space with 0L
  replace_na(list(n_in_space = 0L)) %>% 
  # Create a line plot with n_in_space over year, color by species
  ggplot(aes(x = year, y = n_in_space, color = species)) +
  geom_line()

dates=seq(as.Date("1986-01-01"), as.Date("1986-12-31"), "days")
reactors=c("A","B","C","D")
# Create a tibble with all combinations of dates and reactors
full_df <- expand_grid(date = dates, reactor = reactors)

# Find the reactor - date combinations not present in reactor_df
#full_df %>% 
#  anti_join(reactor_df, by = c("date", "reactor"))

planets = c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")

planet_df=tribble(~planet, ~n_moons,
"Earth", 1,
"Mars", 2,
"Jupiter", 79,
"Saturn", 62,
"Uranus", 27,
"Neptune", 14)
planet_df %>% 
  complete(
    # Complete the planet variable
    planet = planets,
    # Overwrite NA values for n_moons with 0L
    fill = list(n_moons = 0L)
  )

medal_df=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/olympic_medals_top_10.rds")
medal_df %>% 
  # Count the medals won per team and year
  count(team, year, name = "n_medals")

medal_df %>% 
  # Count the medals won per team and year
  count(team, year, name = "n_medals") %>% 
  # Plot n_medals over year, colored by team
  ggplot(aes(x = year, y = n_medals, color = team)) +
  geom_line() +
  scale_color_brewer(palette = "Paired")

medal_df %>% 
  # Count the medals won per team and year
  count(team, year, name = "n_medals") %>% 
  # Complete the team and year variables, fill n_medals with zeros
  complete(team, year, fill = list(n_medals = 0)) %>% 
  # Plot n_medals over year, colored by team
  ggplot(aes(x = year, y = n_medals, color = team)) +
  geom_line() +
  scale_color_brewer(palette = "Paired")

# Generate all years from 2020 to 2030
years <- full_seq(c(2020, 2030), period = 1)
years

# Generate all decades from 1980 to 2030
decades <- full_seq(c(1980, 2030), period = 10)
decades

outer_dates <- c(as.Date("1980-01-01"), as.Date("1980-12-31"))

# Generate the dates for all days in 1980
full_seq(outer_dates, period = 1)

cumul_nukes_1962_df=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/nukes_1962.rds")
cumul_nukes_1962_df %>% 
  # Complete the dataset
  complete(country, date = full_seq(date, period = 1))

cumul_nukes_1962_df %>% 
  # Complete the dataset
  complete(country, date = full_seq(date, period = 1)) %>% 
  # Group the data by country
  group_by(country) %>% 
  # Impute missing values with the last known observation
  fill(total_bombs)

cumul_nukes_1962_df %>% 
  # Complete the dataset
  complete(country, date = full_seq(date, period = 1)) %>% 
  # Group the data by country
  group_by(country) %>% 
  # Impute missing values with the last known observation
  fill(total_bombs) %>% 
  # Plot the number of bombs over time, color by country
  ggplot(aes(date, total_bombs, color = country)) +
  # These two lines will mark the Cuban Missile Crisis 
  geom_rect(xmin = as.Date("1962-10-16"), xmax = as.Date("1962-10-29"), ymin = -Inf, ymax = Inf, color = NA)+ 
  geom_text(x = as.Date("1962-10-22"), y = 15, label = "Cuban Missile Crisis", angle = 90, color = "white")+
  geom_line()

medal_df=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ7ryL3YFNP4S9bw-F31Ejavx2ZJIBM5yWWVclfVSONfXMeTzaJaIe8yeCkma0FJuudkTUbIbRyWlDz/pub?gid=1081899654&single=true&output=csv")
medal_df %>% 
  # Give each continent an observation at each Olympic event
  complete(
    continent, 
    nesting(season, year), 
    fill = list(medals_per_participant = 0)
  ) %>%
  # Plot the medals_per_participant over time, colored by continent
  ggplot(aes(x = year, y = medals_per_participant, color = continent)) +
  geom_line() +
  facet_grid(season ~ .)

patient_df=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ7ryL3YFNP4S9bw-F31Ejavx2ZJIBM5yWWVclfVSONfXMeTzaJaIe8yeCkma0FJuudkTUbIbRyWlDz/pub?gid=1724949310&single=true&output=csv")
patient_df=patient_df %>% mutate(infected=as.Date(infected),recovered=as.Date(recovered))
patient_df %>% 
  # Pivot the infected and recovered columns to long format
  pivot_longer(-patient, names_to = "status", values_to = "date")

patient_df %>% 
  # Pivot the infected and recovered columns to long format
  pivot_longer(-patient, names_to = "status", values_to = "date") %>% 
  select(-status) %>% 
  # Group by patient
  group_by(patient) %>% 
  # Complete the date range per patient using full_seq()
  complete(date = full_seq(date, period = 1)) %>% 
  # Ungroup the data
  ungroup()

patient_df %>% 
  # Pivot the infected and recovered columns to long format
  pivot_longer(-patient, names_to = "status", values_to = "date") %>% 
  select(-status) %>% 
  # Group by patient
  group_by(patient) %>% 
  # Complete the date range per patient using full_seq()
  complete(date = full_seq(date, period = 1)) %>% 
  # Ungroup the data
  ungroup() %>% 
  # Count the dates, the count goes in the n_sick variable
  count(date, name = "n_sick") %>% 
  ggplot(aes(x = date, y = n_sick))+
  geom_line()

sensor_df=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ7ryL3YFNP4S9bw-F31Ejavx2ZJIBM5yWWVclfVSONfXMeTzaJaIe8yeCkma0FJuudkTUbIbRyWlDz/pub?gid=1171603308&single=true&output=csv")
sensor_df=sensor_df %>% mutate(time=as.POSIXct(time))
sensor_df %>% 
  # Complete the time column with a 20 minute interval
  complete(time = seq(min(time), max(time), by = "20 min"),
           fill = list(enter = 0L, exit = 0L))

sensor_df %>% 
  # Complete the time column with a 20 minute interval
  complete(time = seq(min(time), max(time), by = "20 min"),
           fill = list(enter = 0L, exit = 0L)) %>%
  # Calculate the total number of people inside
  mutate(total_inside = cumsum(enter + exit))

sensor_df %>% 
  # Complete the time column with a 20 minute interval
  complete(time = seq(min(time), max(time), by = "20 min"),
           fill = list(enter = 0L, exit = 0L)) %>%
  # Calculate the total number of people inside
  mutate(total_inside = cumsum(enter + exit)) %>% 
  # Pivot the enter and exit columns to long format
  pivot_longer(enter:exit, names_to = "direction", values_to = "n_people")

sensor_df %>% 
  # Complete the time column with a 20 minute interval
  complete(time = seq(min(time), max(time), by = "20 min"),
           fill = list(enter = 0L, exit = 0L)) %>%
  # Calculate the total number of people inside
  mutate(total_inside = cumsum(enter + exit)) %>% 
  # Pivot the enter and exit columns to long format
  pivot_longer(enter:exit, names_to = "direction", values_to = "n_people") %>% 
  # Plot the number of people over time, fill by direction
  ggplot(aes(x = time, y = n_people, fill = direction)) +
  geom_area() +
  geom_line(aes(y = total_inside))

movie_list=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/star_wars_movie_list.rds")
# Create a movie column from the movie_list
tibble(movie = movie_list)

# Create a movie column from the movie_list
tibble(movie = movie_list) %>% 
  # Unnest the movie column
  unnest_wider(movie)

movie_planets_list=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/star_wars_movie_planet_list.rds")
# Create a tibble with a movie column
tibble(movie = movie_planets_list) %>% 
  # Unnest the movie column
  unnest_wider(movie)

# Create a tibble with a movie column
tibble(movie = movie_planets_list) %>% 
  # Unnest the movie column
  unnest_wider(movie) %>% 
  # Unnest the planets column
  unnest_wider(planets)

# Create a tibble from movie_planets_list
tibble(movie = movie_planets_list)

# Create a tibble from movie_planets_list
tibble(movie = movie_planets_list) %>% 
  # Unnest the movie column in the correct direction
  unnest_wider(movie)

# Create a tibble from movie_planets_list
tibble(movie = movie_planets_list) %>% 
  # Unnest the movie column in the correct direction
  unnest_wider(movie) %>% 
  # Unnest the planets column in the correct direction
  unnest_longer(planets)

planet_df=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/planet_moons_list_df.rds")
planet_df %>% 
  # Unnest the moons list column over observations
  unnest_longer(moons)

planet_df %>% 
  # Unnest the moons list column over observations
  unnest_longer(moons) %>% 
  # Further unnest the moons column
  unnest_wider(moons)

planet_df %>% 
  # Unnest the moons list column over observations
  unnest_longer(moons) %>% 
  # Further unnest the moons column
  unnest_wider(moons) %>% 
  # Unnest the moon_data column
  unnest_wider(moon_data)

planet_df %>% 
  # Unnest the moons list column over observations
  unnest_longer(moons) %>% 
  # Further unnest the moons column
  unnest_wider(moons) %>% 
  # Unnest the moon_data column
  unnest_wider(moon_data) %>% 
  # Get the top five largest moons by radius
  slice_max(radius, n = 5)

character_df=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/star_wars_characters.rds")
character_df %>%
  # Unnest the metadata column
  unnest_wider(metadata)

character_df %>% 
  # Unnest the metadata column
  unnest_wider(metadata) %>% 
  unnest_longer(films)

character_df %>% 
  hoist(metadata, first_film = list("films", 1))

#movie_df %>% 
  # Unnest the movie column
#  unnest_wider(movie)

#movie_df %>% 
  # Unnest the movie column
#  unnest_wider(movie) %>% 
#  select(Title, Year, Ratings) %>% 
  # Unnest the Ratings column
#  unnest_wider(Ratings)

# movie_df %>% 
#   hoist(
#     movie,
#     title = "Title",
#     year = "Year",
#     rating = list("Ratings", "Rotten Tomatoes")
#   )

model=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/ansur_weight_model.rds")
library(broom)
tidy(model)

glance(model)

# ansur_df=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/ANSUR_II.rds")
# ansur_df %>% 
#   # Group the data by branch, then nest
#   group_by(branch) %>% 
#   nest()

# ansur_df %>% 
#   # Group the data by branch and sex, then nest
#   group_by(branch, sex) %>% 
#   nest()

ansur_df=readRDS("/Users/apple/Documents/Rstudio/DataCamp/ReshapingDatawithtidyr/DATABASE/ANSUR_II.rds")
ansur_df %>%
  # Group the data by sex
  group_by(sex) %>% 
  # Nest the data
  nest() %>%
  mutate(
    fit = map(data, function(df) lm(weight_kg ~ waist_circum_m + stature_m, data = df)),
    glanced = map(fit, glance)
  ) %>% 
  # Unnest the glanced column
  unnest(glanced)
