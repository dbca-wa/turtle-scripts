# Append new users to spreadsheet: name, email, phone, role
here::here("data", "users.csv") %>% readr::read_csv(col_types = "cccc") %>% wastdr::wastd_POST("users")
