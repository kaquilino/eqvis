context("Create Map")

data(sample_eq)
clean_eq <- eq_clean_data(sample_eq)
clean_labelled_eq <- eq_create_label(clean_eq)

test_that("Invalid Argument Generates Error", {
   expect_error(eq_map(5))
   expect_error(eq_map(data.frame(a=c(1,5,6),b=c("a","b","c"))))
   expect_error(eq_map(subset(clean_eq, select = -LONGITUDE)))
   expect_error(eq_map(subset(clean_eq, select = -LATITUDE)))
   expect_error(eq_map(subset(clean_eq, select = -EQ_PRIMARY)))
   expect_error(eq_map(subset(clean_eq, select = -DATE)))
   expect_error(eq_map(subset(clean_labelled_eq, select = -popup_text)))
})

test_that("HTML Widget Returned", {
   expect_is(eq_map(clean_eq),"htmlwidget")
})
