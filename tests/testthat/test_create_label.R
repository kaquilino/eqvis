context("Create Label")

data(sample_eq)
clean_eq <- eq_clean_data(sample_eq)


test_that("Invalid Argument Generates Error", {
   expect_error(eq_create_label(5))
   expect_error(eq_create_label(data.frame(a=c(1,5,6),b=c("a","b","c"))))
   expect_error(eq_create_label(subset(clean_eq, select = -LOCATION_NAME)))
   expect_error(eq_create_label(subset(clean_eq, select = -EQ_PRIMARY)))
   expect_error(eq_greate_label(subset(clean_eq, select = -DEATHS)))
})

test_that("Label Returned", {
   expect_is(eq_create_label(clean_eq),"character")
   expect_match(eq_create_label(clean_eq),"Location|Deaths|Magnitude")
})