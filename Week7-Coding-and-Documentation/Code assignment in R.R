
# Sets the path to the parent directory of RR classes
setwd("C:\\Users\\kubas\\OneDrive\\Documents\\Reproducible Research\\Repo_RR\\RRcourse2026\\Week7-Coding-and-Documentation")

# Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load here
# are already cross-walked to ISCO-08 using: https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/

# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.

task_data = read.csv("Data\\onet_tasks.csv")
# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job

# read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)
library(readxl)                     

for(i in 1:9){
  assign(
    paste0("isco", i),
    read_excel("Data/Eurostat_employment_isco.xlsx", sheet = paste0("ISCO", i))
  )
}

# We will focus on three countries, but perhaps we could clean this code to allow it
# to easily run for all the countries in the sample?

# This will calculate worker totals in each of the chosen countries.
countries <- c("Belgium", "Czechia", "Denmark", "Spain", 
               "Italy", "Lithuania", "Poland", "Finland", "Sweden")

isco_list <- mget(paste0("isco", 1:9)) 

for (country in countries) {
  assign(
    paste0("total_", country),
    Reduce(`+`, lapply(isco_list, function(df) df[[country]]))
  )
}
# Let's merge all these datasets. We'll need a column that stores the occupation categories:
for (i in 1:9) {
  df <- get(paste0("isco", i))  
  df$ISCO <- i                  
  assign(paste0("isco", i), df) 
}

# and this gives us one large file with employment in all occupations.
all_data <- rbind(isco1, isco2, isco3, isco4, isco5, isco6, isco7, isco8, isco9)

# We have 9 occupations and the same time range for each, so we an add the totals by
# adding a vector that is 9 times the previously calculated totals
for (country in countries) {
  all_data[[paste0("total_", country)]] <- rep(
    get(paste0("total_", country)),
    times = 9
  )
}
# And this will give us shares of each occupation among all workers in a period-country
for (country in countries) {
  all_data[[paste0("share_", country)]] <- all_data[[country]] / all_data[[paste0("total_", country)]]
}
# Now let's look at the task data. We want the first digit of the ISCO variable only
library(stringr)

task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level 
# (more on what these tasks are below)

aggdata <-aggregate(task_data, by=list(task_data$isco08_1dig),
                    FUN=mean, na.rm=TRUE)
aggdata$isco08 <- NULL

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.

# These are the ones we're interested in:
# Non-routine cognitive analytical
# 4.A.2.a.4 Analyzing Data or Information
# 4.A.2.b.2	Thinking Creatively
# 4.A.4.a.1	Interpreting the Meaning of Information for Others

#Let's combine the data.
library(dplyr)

combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

#install.packages("Hmisc")
library(Hmisc)

# first task item
countries <- c("Belgium", "Czechia", "Denmark", "Spain", 
               "Italy", "Lithuania", "Poland", "Finland", "Sweden")
variables <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")

# Function to compute weighted standardized variable
compute_weighted_std <- function(df, var, country) {
  weight_col <- paste0("share_", country)
  value <- df[[var]]
  weight <- df[[weight_col]]
  
  mean_w <- wtd.mean(value, weight)
  sd_w <- sqrt(wtd.var(value, weight))
  
  new_col <- paste0("std_", country, "_", var)
  df[[new_col]] <- (value - mean_w) / sd_w
  return(df[[new_col]])
}

# Loop over all variables and countries
for (var in variables) {
  for (country in countries) {
    combined[[paste0("std_", country, "_", var)]] <- compute_weighted_std(combined, var, country)
  }
}

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:

for (country in countries) {
  std_vars <- paste0("std_", country, "_", variables)  # columns to sum
  combined[[paste0(country, "_NRCA")]] <- rowSums(combined[, std_vars])
}

for (country in countries) {
  NRCA_col <- paste0(country, "_NRCA")
  weight_col <- paste0("share_", country)
  
  temp_mean <- wtd.mean(combined[[NRCA_col]], combined[[weight_col]])
  temp_sd <- sqrt(wtd.var(combined[[NRCA_col]], combined[[weight_col]]))
  
  combined[[paste0("std_", NRCA_col)]] <- (combined[[NRCA_col]] - temp_mean) / temp_sd
}
# Finally, to track the changes over time, we have to calculate a country-level mean
for (country in countries) {
  combined[[paste0("multip_", country, "_NRCA")]] <- 
    combined[[paste0("std_", country, "_NRCA")]] * combined[[paste0("share_", country)]]
}

agg_list <- list()
for (country in countries) {
  multip_col <- paste0("multip_", country, "_NRCA")
  agg_list[[country]] <- aggregate(combined[[multip_col]],
                                   by = list(TIME = combined$TIME),
                                   FUN = sum, na.rm = TRUE)
}
# We can plot it now!
for (country in countries) {
  agg <- agg_list[[country]]
  
  plot(agg$x, xaxt = "n", main = paste0(country, " NRCA over time"), ylab = "Weighted NRCA")
  axis(1, at = seq(1, nrow(agg), 3), labels = agg$TIME[seq(1, nrow(agg), 3)])
}


# If this code gets automated and cleaned properly,
#  you should be able to easily add other countries as well as other tasks.
# E.g.:

# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment

