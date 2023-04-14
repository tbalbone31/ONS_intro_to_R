


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

# Start with filtering next time
