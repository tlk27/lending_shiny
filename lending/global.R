library(shinydashboard)
library(shiny)
library(tidyverse)
library(googleVis)
library(dygraphs)
library(DT)
library(rsconnect)

loans_c = read_csv('./loans_c.csv')

######################## Home


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




######################## Grades

grades_df = reactive({
  loans_c %>% 
    select(grade, purpose, loan_status, issue_d) %>%
    mutate(purpose = gsub('_', ' ', purpose),
           issue_d = lubridate::parse_date_time(issue_d, orders = '%b-%Y'))
})
  

######################## Funding & Payments

loan_tots = reactive({
  loans_c %>%
    select(loan_amnt, funded_amnt, total_pymnt)
})


payments = reactive({
  loans_c %>% 
    select (issue_d, last_pymnt_d, loan_amnt, funded_amnt, total_pymnt, term, installment ) %>%
    mutate(issue_d = lubridate::parse_date_time(issue_d, orders = '%b-%Y'),
           last_pymnt_d = lubridate::parse_date_time(last_pymnt_d, orders = '%b-%Y'),
           term_mnth = as.numeric(gsub("[^0-9]", '', term) ),
           app_mth = round(as.numeric((last_pymnt_d - issue_d) / 2.628e+6), 0), #approximate the number of payments made                    
           mnth_rmn = term_mnth - app_mth,
           mnth_rmn = term_mnth - app_mth,
           max_ttl = installment * term_mnth,
           min_amnt_remain = max_ttl - total_pymnt,
           amount_remain = installment * mnth_rmn)
  
})






