 
parseCompanySalaryPage <- function(glassDoorCompanySalaryHtml) {
    
  
    #each page of glassdoor as an html table of the salaries 
    #extract table from html (will need to further clean it up in more steps below)
    salary_table = glassDoorCompanySalaryHtml %>% 
      html_element(".salarylist_salary-table__QjAUn") %>% 
      html_table()
    
    # rename columns for clarity 
    salary_table <- salary_table %>%
      rename("job_title" = `Job Title`)
    
    # rename columns for clarity 
    salary_table <- salary_table %>%
      rename("dirty_salary" = `Total PayBase | Additional`)
    
    #drop unnecessary column (open jobs)
    salary_table <- salary_table %>%
      select(-`Open Jobs`)
    
    
    # Define expression pattern that matches the salaries submitted section that the html parses wrong
    #[0-9]+                   matches 0-9 at the beginning going as far back as possible
    #[K]*                     matches K if it exists (ie could be 600 or 6K)
    #\\s                      matches space or new line character 
    #Salaries submitted\b     boundry to end matching after the word salaries submitted"
    pattern <- "[0-9]+[K]*\\sSalaries submitted\\b"
    
    #take out salaries submitted attribute and put it into a new column 
    salary_table <- salary_table %>%
      mutate(salaries_submitted = str_extract(job_title, pattern))  
    
    #take out the salaries submitted portion from the job title
    salary_table <- salary_table %>%
      mutate(job_title = str_replace( job_title, salaries_submitted, ""))
    
    # remove all the salary data that ended up in the job title (everything after the $)
    salary_table <- salary_table %>%
      mutate( job_title = str_replace(job_title, "\\$.+", "") )
     
    
    # We need to break out the salary ranges in medians into the individual parts
    #[0-9]+    pattern will group numbers into separate entities (there are 4 numbers to categorize)
    salary_table <- salary_table %>%
      mutate( salary_breakdown = str_extract_all(dirty_salary, "[0-9]+", simplify=TRUE))
    
    
    #the way glassdoor structures the Salary is as follows
    #first range $estimated minimum salary no bonus - $estimated maximum salary with bonus
    #second range $median salary|$median bonus
    #for this analysis we decided to go with second range = Median pay (including bonus)
    salary_table <- salary_table %>%
      mutate( min_salary = salary_breakdown[,1], 
              max_salary = salary_breakdown[,2], 
              median_salary = salary_breakdown[,3], 
              median_bonus = salary_breakdown[,4])
    
    #drop the split columns since we don't need it anymore
    salary_table <- salary_table %>%
      select( -salary_breakdown)
    
    #drop the part that's no good
    salary_table <- salary_table %>%
      select( -dirty_salary)
    
    
    #return the parsed out table
    return(salary_table)
}