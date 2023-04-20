
library(tidyverse)

# Creating a list with multiple items

chocolate <- list(brand = "celebrations", 
                  chocolate_bar = c("mars", "milky way", "bounty", "twix", "maltesers"), 
                  rating = c("worst",  "terrible", "awful", "nice", "best"))


titanic <- readr::read_csv("Data/titanic.csv", 
                           na = c("*", ".", "", "NULL"))

# Reading in excel data using the readxl package
# and the read_excel function
# assigning the file name police_data
# specifying which sheet to read, here it is the second sheet or Table P1
# specifying the range to only read the cells with data


police_data <- readxl::read_excel("Data/police_data.xlsx",
                                  sheet = 2, 
                                  range = "A5:AA48")

readr::write_csv(police_data, file = "Data/test.csv")

titanic <- janitor::clean_names(titanic)

titanic_renamed <- dplyr::rename(titanic, age = age_of_passenger)

titanic_sorted <- dplyr::arrange(.data = titanic,
                                 desc(age_of_passenger),
                                 desc(fare))

titanic_one_variable <- dplyr::select(.data = titanic,
                                      name_of_passenger)

titanic_three_variables <- dplyr::select(.data = titanic,
                                         name_of_passenger,
                                         age_of_passenger,
                                         pclass)

titanic_minus_three_variables <- dplyr::select(.data = titanic,
                                               -c(name_of_passenger,
                                               age_of_passenger,
                                               pclass))

titanic_number_select <- dplyr::select(.data = titanic,1:4, 7)

titanic_number_select <- dplyr::select(.data = titanic, 
                                       -2)

titanic_starts_with <- dplyr::select(.data = titanic,
                                     starts_with("s"))

# Select passengers who were in class (pclass) 2.

secondclass <- dplyr::filter(.data = titanic,
                             pclass == 2)

# To display the data

secondclass

# Returns the set of unique values in the data set.
# finding out how many unique values are in the titanic dataset pclass column

unique(titanic$pclass)

# finding out how many unique values are in the secondclass dataset pclass column
unique(secondclass$pclass)

# Select passengers who paid more than 200

fare_more_than_200 <- dplyr::filter(.data = titanic,
                                    fare > 200)

# finding out how many unique values are in the faremorethan200 dataset fare column

unique(fare_more_than_200$fare)

# SHow the row for the passenger named 'Birkeland, Mr. Hans Martin Monsen'

filter_specific_passenger <- dplyr::filter(.data = titanic,
       name_of_passenger == "Birkeland, Mr. Hans Martin Monsen")

# OR

titanic_using_str_detect<- dplyr::filter(.data = titanic,
                                         stringr::str_detect(name_of_passenger,'Hans'))

# How many passengers in the dataset are male?

male_passengers <- dplyr::filter(.data = titanic,
                                 sex_of_passenger == "male")

dplyr::count(male_passengers) #count using dplyr count
nrow(male_passengers) #count using nrow

#How many passengers are under 18 years of age?

passengers_under_18 <- dplyr::filter(.data = titanic,
                                     age_of_passenger < 18)

dplyr::count(passengers_under_18) # Count using dplyr count
nrow(passengers_under_18) # count using nrow

# What proportion of passenger in the dataset survived?

passengers_survived <- dplyr::filter(.data = titanic,
                                     survived == 1)

proportion_survived <- nrow(passengers_survived)/nrow(titanic)
proportion_survived

# Select passengers who were in class 1 AND female

passengers_class1_female <- dplyr::filter(.data = titanic,
                                          pclass == 1 & sex_of_passenger == "female")

passengers_class1_female

# Select passengers who were female OR children (under 18)

passengers_female_children <- dplyr::filter(.data = titanic,
                                            sex_of_passenger == "female" | age_of_passenger < 18)

passengers_female_children

# Select passengers who paid more than 100 but less than 250 for their fare

passengers_fare_100to250 <- dplyr::filter(.data = titanic,
                                          between(fare, left = 100, right = 250))
passengers_fare_100to250

#Select passengers who embarked in Southampton or Cherboug

passengers_embarked_Southampton_or_Cherbourg <- dplyr::filter(.data = titanic,
                                                              embarked == "C" | embarked == "S")

passengers_embarked_Southampton_or_Cherbourg <- dplyr::filter(.data = titanic,
                                        embarked %in% c('S','C'))

# Select passengers who are in class 2 or 3 and show percentage

passengers_class2or3 <- dplyr::filter(.data = titanic,
                                      pclass == 2 | pclass == 3)

proportion_class2or3 <- (nrow(passengers_class2or3)/nrow(titanic))*100
proportion_class2or3

# How many passengers were travelling alone?

passengers_alone <- dplyr::filter(.data = titanic,
                                  sibsp == 0)
nrow(passengers_alone)

#What proportion of passengers who ‘embarked’ in Cherbourg (‘C’) or Queenstown (‘Q’) survived?

embarked_cherbourg_and_queenstown <- dplyr::filter(.data = titanic,
                                                   embarked == 'C' | embarked == 'Q')

# filtering the dataset a single condition to find the survivors

survived <- dplyr::filter(embarked_cherbourg_and_queenstown, 
                          survived == 1)

# showing the number of rows in the filtered dataset

nrow(survived) / nrow(embarked_cherbourg_and_queenstown) * 100

# generating new variables

mutate_example <- dplyr::mutate(.data = titanic, 
                                string_twos = "two")

# Capitalise sex column
# Here we are over writing the  sex_of_passenger column
# If you give your new column the same name  as  an existing column 
# It will overwrite it.

mutate_example <- dplyr::mutate(.data = titanic,
                                sex_of_passenger = stringr::str_to_upper(sex_of_passenger))

# Determining family size on board

mutate_example <- dplyr::mutate(.data = titanic,
                                family_size = parch + sibsp + 1)

# Creating a logical column which is TRUE 
# if passenger is female, false otherwise.

mutate_example <- dplyr::mutate(.data = titanic,
                                is_female = (sex_of_passenger == "female"))

# Recoding variables
# Note Recode does not have a .data argument

dplyr::recode(titanic$survived, 
              '1' = TRUE, 
              '0' = FALSE)

# Generating new variables

recode_example <- dplyr::mutate(.data = titanic,
                                survived_logical = recode(survived, '1' = TRUE, '0' = FALSE))


# Create fare_dollars and convert the currency from £ to $ using exchange rate £1 to $1.39

titanic_fare_in_dollars <- dplyr::mutate(.data = titanic,
                              fare_dollars = round(fare * 1.39))
glimpse(titanic_fare_in_dollars)


# Generating new variables

titanic_mutate_exercise <- dplyr::mutate(.data = titanic,
                                         child = age_of_passenger < 18)
# To display the column names

names(titanic_mutate_exercise)

# Setting a column = NULL effectively removes it.

titanic_mutate_exercise <- dplyr::mutate(.data = titanic_mutate_exercise, 
                                         child = NULL)

# finding out the names of the columns in the dataset

names(titanic_mutate_exercise)

## Example of non pipey way to filter, select and sort.

# Filtering the titanic dataset  

first_class_survivors <- dplyr::filter(.data = titanic,
                                       pclass == 1 & survived == 1)

# Selecting only the columns needed 
# Note here I am using first_class_survivors as my dataset

selected_first_class_survivors <- dplyr::select(.data = first_class_survivors,
                                                name_of_passenger, 
                                                age_of_passenger, 
                                                embarked, 
                                                fare)

# Sorting the data
# Note here I am using selected_class_survivors as my dataset

sorted_first_class_survivors <- dplyr::arrange(.data = selected_first_class_survivors,
                                               desc(age_of_passenger)) 

## Example of nested, even more complicated way to do filter, select and sort

# This may be very hard to read and understand - the pipe will make this much more readable!

sorted_first_class_survivors <- dplyr::arrange(.data = 
                                                 dplyr::select(.data = 
                                                                 dplyr::filter(.data = titanic,
                                                                               pclass == 1 & survived == 1),
                                                               name_of_passenger,
                                                               age_of_passenger,
                                                               embarked,
                                                               fare)
                                               , desc(age_of_passenger))