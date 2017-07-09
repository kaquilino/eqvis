context("Location Clean")

test_that("Invalid Argument Generates Error", {
   expect_error(eq_location_clean(5))
   expect_error(eq_location_clean(data.frame(a=c(1,5,6),b=c("a","b","c"))))
})

test_that("Country is Stripped", {
   expect_equal(eq_location_clean("USA: New York"),"New York")
   expect_equal(eq_location_clean("MEXICO: Cancun"),"Cancun")
   expect_equal(eq_location_clean("MEXICO: MEXICO: Cancun"),"Cancun")
   expect_equal(eq_location_clean("Cancun"),"Cancun")
   expect_equal(eq_location_clean("MEXICO, Cancun"),"Mexico, Cancun")
})

test_that("All caps are converted to Title Case", {
   expect_equal(eq_location_clean("USA: NEW YORK"),"New York")
   expect_equal(eq_location_clean("MEXICO: Cancun"),"Cancun")
   expect_equal(eq_location_clean("MEXICO: MEXICO: CANCUN"),"Cancun")
   expect_equal(eq_location_clean("cancun"),"Cancun")
   expect_equal(eq_location_clean("MEXICO, Cancun"),"Mexico, Cancun")
})
