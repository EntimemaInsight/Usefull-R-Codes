# NOTE: This may take a lot of time in specific versions of tidyr (Like 1.2.1)! Also it would not work with dates
mutate(across(everything(), ~tidyr::replace_na(.x, 0)))

# Better use if appropriate
df[is.na(df)] = 0