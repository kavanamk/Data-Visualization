library(dplyr)

# Loading the datasets
shootings = read.csv("fatal-police-shootings-data.csv", header=T)
census = read.csv("acs2015_census_tract_data.csv", header=T)
education= read.csv("PercentOver25CompletedHighSchool.csv", header=T)

state_abbreviations = c('California' = 'CA', 'Texas' = 'TX', 'Florida' = 'FL', 'New York' = 'NY', 
                         'Pennsylvania' = 'PA', 'Illinois' = 'IL', 'Ohio' = 'OH', 'Georgia' = 'GA', 
                         'North Carolina' = 'NC', 'Michigan' = 'MI', 'New Jersey' = 'NJ', 'Virginia' = 'VA', 
                         'Washington' = 'WA', 'Arizona' = 'AZ', 'Massachusetts' = 'MA', 'Tennessee' = 'TN', 
                         'Indiana' = 'IN', 'Missouri' = 'MO', 'Maryland' = 'MD', 'Wisconsin' = 'WI', 
                         'Colorado' = 'CO', 'Minnesota' = 'MN', 'South Carolina' = 'SC', 'Alabama' = 'AL', 
                         'Louisiana' = 'LA', 'Kentucky' = 'KY', 'Oregon' = 'OR', 'Oklahoma' = 'OK', 
                         'Connecticut' = 'CT', 'Iowa' = 'IA', 'Utah' = 'UT', 'Nevada' = 'NV', 'Arkansas' = 'AR', 
                         'Mississippi' = 'MS', 'Kansas' = 'KS', 'New Mexico' = 'NM', 'Nebraska' = 'NE', 
                         'West Virginia' = 'WV', 'Idaho' = 'ID', 'Hawaii' = 'HI', 'New Hampshire' = 'NH', 
                         'Maine' = 'ME', 'Montana' = 'MT', 'Rhode Island' = 'RI', 'Delaware' = 'DE', 
                         'South Dakota' = 'SD', 'North Dakota' = 'ND', 'Alaska' = 'AK', 
                         'District of Columbia' = 'DC', 'Vermont' = 'VT', 'Wyoming' = 'WY')

census = census %>%
  mutate(State = recode(State, !!!state_abbreviations))

# missing values
colSums(is.na(shootings))
colSums(is.na(census))
colSums(is.na(education))

# Drop null values from the education dataset
education <- education %>%
  filter(percent_completed_hs != '-')

# Rename column 'Geographic Area' to 'state'
education <- education %>%
  rename(state = `Geographic.Area`)

# Convert 'percent_completed_hs' column to numeric
education <- education %>%
  mutate(percent_completed_hs = as.numeric(percent_completed_hs))

# Remove rows where 'State' is 'Puerto Rico'
census <- census %>%
  filter(State != 'Puerto Rico')

# Rename column 'State' to 'state'
census <- census %>%
  rename(state = State)

# Create a dataset for total population by state and sum of Men and Women
pop <- census %>%
  group_by(state) %>%
  summarise(
    TotalPop = sum(TotalPop, na.rm = TRUE),
    Men = sum(Men, na.rm = TRUE),
    Women = sum(Women, na.rm = TRUE)
  ) %>%
  ungroup()
head(pop)

# initial df with unique states
race_ratios <- data.frame(state = unique(census$state))

# calculate population share by race for each state
get_share <- function(race) {
  # Calculate shares 
  shares <- census %>%
    group_by(state) %>%
    summarise(
      share = sum(.data[[race]] * TotalPop, na.rm = TRUE) / sum(TotalPop, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Merge with race_ratios to maintain order and return only shares
  result <- race_ratios %>%
    left_join(shares, by = "state") %>%
    pull(share)
  
  return(result)
}

# Calculate shares for each race
race_ratios$White <- get_share('White')
race_ratios$Black <- get_share('Black')
race_ratios$Hispanic <- get_share('Hispanic')
race_ratios$Native <- get_share('Native')
race_ratios$Asian <- get_share('Asian')
race_ratios$Pacific <- get_share('Pacific')

head(race_ratios)

# Create socio_eco_factors dataframe with median values by state
socio_eco_factors <- census %>%
  group_by(state) %>%
  summarise(
    Citizen = median(Citizen, na.rm = TRUE),
    Income = median(Income, na.rm = TRUE),
    IncomePerCap = median(IncomePerCap, na.rm = TRUE),
    .groups = 'drop'
  )

# calculate weighted rates for each factor
get_rated <- function(data) {

  columns <- c('Poverty', 'ChildPoverty', 'Professional', 'Service', 'Office', 
               'Construction', 'Production', 'Drive', 'Carpool', 'Transit', 
               'Walk', 'OtherTransp', 'WorkAtHome', 'MeanCommute', 'Employed', 
               'PrivateWork', 'PublicWork', 'SelfEmployed', 'FamilyWork', 
               'Unemployment')
  
  # Calculate weighted rates for each column
  for(column in columns) {
    rates <- census %>%
      group_by(state) %>%
      summarise(
        rate = sum(.data[[column]] * TotalPop, na.rm = TRUE) / sum(TotalPop, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Add the calculated rates to socio_eco_factors
    socio_eco_factors[[column]] <<- rates$rate
  }
  
  return(socio_eco_factors)
}

# Calculate all rates
socio_eco_factors <- get_rated(socio_eco_factors)
head(socio_eco_factors)

# Merge these datasets on state
df <- shootings %>%
  left_join(pop, by = "state") %>%
  left_join(race_ratios, by = "state") %>%
  left_join(socio_eco_factors, by = "state")
head(df)

# Calculate the proportion of victims by race
df <- df %>%
  group_by(race) %>%
  mutate(percent_killed_race = n() / nrow(df) * 100) %>%
  ungroup()
head(df)

#Handling Nulls
# Remove rows with any missing values
df_clean <- na.omit(df)
colSums(is.na(df_clean))
dim(df_clean)
dim(df)

# Save the cleaned data frame to a CSV file
write.csv(df_clean, "cleaned_data.csv", row.names = FALSE)
