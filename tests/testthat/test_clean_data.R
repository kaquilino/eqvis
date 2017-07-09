context("Clean Data")

data(sample_eq)

test_that("Invalid Argument Generates Error", {
   expect_error(eq_clean_data(5))
   expect_error(eq_clean_data(data.frame(a=c(1,5,6),b=c("a","b","c"))))
   expect_error(eq_clean_data(subset(sample_eq,select = -YEAR)))
   expect_error(eq_clean_data(subset(sample_eq,select = -MONTH)))
   expect_error(eq_clean_data(subset(sample_eq,select = -DAY)))
   expect_error(eq_clean_data(subset(sample_eq,select = -LATITUDE)))
   expect_error(eq_clean_data(subset(sample_eq,select = -LONGITUDE)))
})

test_that("Date Column Created", {
   expect_is(eq_clean_data(sample_eq),"data.frame")
})