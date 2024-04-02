

# Set the file path
file_path <- "/Users/stephenreilly/Desktop/CSUN/Spring 2024/541 Data mining/Assignments/project1/gov_wages_cleaned_2022.csv"  # Replace 'your_file_path.csv' with the actual file path

# Read the CSV file
data <- read.csv(file_path)

# Columns to remove commas and quotes from
columns_to_clean <- c("TOT_EMP", "EMP_PRSE", "H_MEAN", "A_MEAN", "MEAN_PRSE", 
                      "H_PCT10", "H_PCT25", "H_MEDIAN", "H_PCT75", "H_PCT90", 
                      "A_PCT10", "A_PCT25", "A_MEDIAN", "A_PCT75", "A_PCT90")

# Remove commas and quotes from specified columns
data[columns_to_clean] <- lapply(data[columns_to_clean], function(x) gsub("[,']", "", x))

# Convert columns to numeric
data[columns_to_clean] <- lapply(data[columns_to_clean], as.numeric)
 
# Select total employment from the dataset
total_employees <- data$TOT_EMP[data$O_GROUP == "total" ]

# Select rows where the column 'column_name' has the value "main"
summary <- data[data$O_GROUP == "major", ]


# calculate what percent of the total these occupations categories make up
summary <- mutate(summary,
    percent_of_total = TOT_EMP/total_employees
)
 



print(summary)











# View the first few rows of the data to verify it was loaded correctly
head(data)