library(shinydashboard)
library(shiny)
library(tidyverse)
library(googleVis)
library(dygraphs)
library(DT)

loans_c = read_csv('../loans_c.csv')

choices = colnames(loans_c)


loan_tots = reactive({
  loans_c %>%
    select(loan_amnt, funded_amnt, total_pymnt)
})

loan_purps = reactive({
  loans_c %>%
    select(
      purpose,
      emp_length,
      home_ownership,
      application_type,
      annual_inc,
      dti,
      grade,
      sub_grade
    ) #maybe add other characteristics/demographics
  
})

dt_vars = reactive ({
  loans_c %>%
    select(annual.inc = annual_inc, dti, yrs.emp = emp_length) %>%
    mutate(yrs.emp = as.numeric(gsub('[^0-9]', '', yrs.emp))) %>%
    na_if(999) %>%
    drop_na()
})

payments = reactive({
  loans_c %>% 
    select (issue_d, funded_amnt, total_pymnt) %>%
    mutate(issue_d = lubridate::parse_date_time(issue_d, orders = '%b-%Y')) %>% 
    group_by(issue_d) %>% 
    summarise(rec_monthly = round(sum(total_pymnt), 2), funded_monthly = round(sum(funded_amnt), 2) )
})





# May add this after additional column removal later
#
# loan_maps = reactive({
#   loans_c %>%
#     select(addr_state)
# })  