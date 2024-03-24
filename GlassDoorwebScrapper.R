

#create TABLE glassdoorsalaries (
#  job_title VARCHAR(800),
#  salaries_submitted VARCHAR(800),
#  min_salary INT,
#  max_salary INT,
#  median_salary INT,
#  median_bonus INT,
#  company_name VARCHAR(500),
#  company_industry VARCHAR(800),
#  url varchar(800)
#);
 
library(tidyverse)
library(rvest)
#set session directory to the directory of this file so that the next line can reference the file GlassDoorTableCleaner.R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("GlassDoorTableCleaner.R") #separated this process to another file to make this code more readable

# Load the RPostgreSQL library
library("RPostgres")

# Set up the PostgreSQL connection
con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "dm",
                 host = "localhost",
                 port = 5433,
                 user = "postgres",
                 password = "password")
 

#glassdoor limits number of companies to 10 per page, and 999 pages. So total of companies is 9,999 
#GLASSDOOR_COMPANY_LIMIT <- 2
GLASSDOOR_COMPANY_LIMIT <- 999
GLASSDOOR_COMPANY_PER_PAGE <- 10

#timeouts are caused by the http error 429 -- need to keep trying in these cases

#delay is how much to wait in seconds before making another request after getting blocked
DELAY_INCREMENT = 15
 

#loop through all the companies on glassdoor... 
for(i in 0:GLASSDOOR_COMPANY_LIMIT) { 
#for(i in 1:GLASSDOOR_COMPANY_LIMIT){  
  #companies_html  <- read_html("/Users/stephenreilly/Downloads/Browse Companies _ Glassdoor.html")
  
  #if we are at the first/last company on the page we need to get the next page of companies 
  if( i %% GLASSDOOR_COMPANY_PER_PAGE == 0 ) {
    #so page 1 is our first page, once we get to each next 10, the next page would be 2 or for 20 it would be 3
    next_page <- (i/GLASSDOOR_COMPANY_PER_PAGE)+1
    
    print( paste0("https://www.glassdoor.com/Explore/browse-companies.htm?page=", next_page ))
    
    #company page to get different companies on glassdoor 
    companies_html  <- read_html( paste0("https://www.glassdoor.com/Explore/browse-companies.htm?page=", next_page ))
    
    #need to get the industry of each company
    company_names = companies_html %>% 
      html_elements("h2[data-test='employer-short-name']") %>%
      html_text()
    
    #need to get the links of all the salaries 
    company_salary_links = companies_html %>% 
      html_elements("a[data-test='cell-Salaries-url']") %>%
      html_attr("href")
    
    #need to get the industry of each company
    industry = companies_html %>%
      html_elements("span[data-test='employer-industry']")  %>%   
      html_text()
    
  } 
  
  #so companies cards range from 1-10
  #modulo 10 gives 0, so in that case we need to make it 10 since r uses index starting at 1 
  company_num <- ifelse(i %% GLASSDOOR_COMPANY_PER_PAGE == 0, GLASSDOOR_COMPANY_PER_PAGE, i %% GLASSDOOR_COMPANY_PER_PAGE)
  
  #set the company name to the current company on the various pages
  companyname <- company_names[company_num]
  
  #need to make sure we escape apostrophes or else SQL query fails for example McDonald's  needs to be McDonald''s
  escaped_name <- gsub("'", "''", companyname)
  
  # Construct SQL query to check if the name exists in the table (ie we already scrapped it )
  query <- glue::glue("SELECT COUNT(*) FROM glassdoorsalaries WHERE company_name = '{escaped_name}'")
  
  # Execute the query
  result <- dbGetQuery(con, query)
  
  #check the DB to make sure the company has not been parsed yet. 
  if (result[[1]] > 0) {
    # Name exists, skip this iteration
    cat(paste("Company ", companyname, " exists in the database. Skipping...\n"))
    next
  } 
  
  #if glassdoor blocks us, there are no company names 
  if( is.na( company_salary_links[ company_num ]) ) { stop("Crawler was blocked by glassdoor. Exiting.") }
  
  #Go to the page of the company. and get how many pages of salaries exist
  #glassDoorUrl = "/Users/stephenreilly/Desktop/CSUN/Spring 2024/541 Data mining/Assignments/project1/glassdoor_amazon_salaries_html.html"
  glass_door_url =  paste0( "https://www.glassdoor.com", company_salary_links[ company_num ] )
  print(glass_door_url)
  next_page_url <- glass_door_url
  
  #sleep 5 seconds to avoid timeout requests
  Sys.sleep(5)
  
  #parse the page of html where the 
  company_html <- read_html(glass_door_url) 
  
  #Get the number of page numbers for a company's salaries on Glassdoor to know how many tables to loop through 
  total_salary_tables <-  company_html %>%  html_nodes(".pagination_ListItem__CqGNZ:nth-last-child(2)  ") %>% html_text()
  
  #convert the page number from text to number 
  companysalary_pages <- as.numeric(total_salary_tables)-1
  print(companysalary_pages)
  #loop through each page fo a company for all the salaries that are associated with the company
  for(j in 1:companysalary_pages){ 
  #for(j in 1:companysalary_pages){ 
    
    #try getting a salary list page of a company 
    parsedCompanyTable <- parseCompanySalaryPage(company_html)
    
    #add the company details to each row of the salary details
    parsedCompanyTable <- parsedCompanyTable %>%
      mutate(company_name = company_names[company_num],
             company_industry = industry[company_num],
             url = next_page_url
             
      )    
    print(parsedCompanyTable)
    
    # Generate a random integer between 10 and 30 to sleep for that long and throw off glassdoor timeout protection
    random_integer <- sample(10:30, 1)
    #sleep randomly between 30-60 seconds to avoid timeout requests
    Sys.sleep(random_integer)
      
    # Write the dataframe to the PostgreSQL table
    if( j != 0){
      dbWriteTable(con, "glassdoorsalaries", parsedCompanyTable, append = TRUE, row.names = FALSE) 
    }
      
    #need to alter link so that the page number is incremented each time
    #url schema::   https://www.glassdoor.com/Salary/company-name-Salries-EID.htm
    next_page_url <- str_replace(glass_door_url, ".htm", paste0("_P", as.character(j+1), ".htm"))
    
    print(next_page_url)
     
    # Set maximum number of attempts
    max_attempts <- 10
    attempts <- 0
    success <- FALSE
    
    #need to wrap this call in try catch since it keeps failing from timeout 
    while (attempts < max_attempts && !success) {
      tryCatch({
        #get the html of the next page to scrape
        company_html <- read_html(next_page_url)  
        # Try extracting tables from the webpage to make sure the call worked
        table_data <- html_table(company_html)
        
        #make sure it's not empty
        if( length(table_data) != 0) {
          print("Tables successfully extracted.")
          # If the read_html call is successful and we got the salary table, set success to TRUE
          success <- TRUE
        } else{
          print("table is empty, need to retry")
          attempts <- attempts + 1
          sleep_duration <- DELAY_INCREMENT * attempts
          Sys.sleep(sleep_duration)
        }
        
      }, error = function(e) {
        # Print the error message
        print(paste("Attempt", attempts, ": read html from glassdoor failed: ", e$message))
        
        # Increment the failed attempts count
        attempts <- attempts + 1
        
        # increase sleep duration each time
        sleep_duration <- DELAY_INCREMENT * attempts
        Sys.sleep(sleep_duration)
      })
    }
    #exit program if time out
    if( success == FALSE || attempts == max_attempts ) { 
      print("timed out after 10 retries") 
      stop()
    }
    
  } 
  
  #last record needs to be parsed 
  #try getting a salary list page of a company 
  parsedCompanyTable <- parseCompanySalaryPage(company_html)
  
  #add the company details to each row of the salary details
  parsedCompanyTable <- parsedCompanyTable %>%
    mutate(company_name = company_names[company_num],
           company_industry = industry[company_num],
           url = next_page_url
    )   
  
  # Write the dataframe to the PostgreSQL table
  dbWriteTable(con, "glassdoorsalaries", parsedCompanyTable, append = TRUE, row.names = FALSE)
  
  #sleep 1 second to avoid timeout requests
  Sys.sleep(10)  
}

 
 