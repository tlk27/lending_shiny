library(shinydashboard)
library(shiny)
library(tidyverse)
library(googleVis)
library(dygraphs)
library(DT)
library(rsconnect)

loans_c = read_csv('./loans_c.csv')

######################## Home

loan_tots = reactive({
  loans_c %>%
    select(loan_amnt, funded_amnt, total_pymnt)
})

######################## Purpose

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
    ) %>% 
    mutate(purpose = gsub('_', ' ', purpose))
  
})


dt_vars = reactive ({
  loans_c %>%
    select('annual income' = annual_inc, dti, emp_length) %>%
    mutate('years employed' = as.numeric(gsub('[^0-9]', '', emp_length))) %>%
    select('annual income', dti, 'years employed') %>% 
    na_if(999) %>%
    drop_na()
})

######################## Payments
payments = reactive({
  loans_c %>% 
    select (issue_d, funded_amnt, total_pymnt) %>%
    mutate(issue_d = lubridate::parse_date_time(issue_d, orders = '%b-%Y')) %>% 
    group_by(issue_d) %>% 
    summarise(rec_monthly = round(sum(total_pymnt), 2), funded_monthly = round(sum(funded_amnt), 2) )
})

######################## Grades

grades_df = reactive({
  loans_c %>% 
    select(grade, purpose, loan_status, issue_d) %>%
    mutate(purpose = gsub('_', ' ', purpose),
           issue_d = lubridate::parse_date_time(issue_d, orders = '%b-%Y'))
})
  
######################## Map
loan_maps = reactive({
  loans_c %>%
    select(addr_state, loan_amnt, funded_amnt, total_pymnt, tot_coll_amt) %>%
    group_by(addr_state) %>% 
    summarise_all(list(sum)) %>% 
    select(addr_state, 
           'requested amount' = loan_amnt, 
           'funded amount' = funded_amnt, 
           'total payments received' = total_pymnt
           )
})

m_names = loans_c %>%
  select('requested amount' = loan_amnt, 
         'funded amount' = funded_amnt, 
         'total payments received' = total_pymnt
         )

choices = colnames(m_names)







