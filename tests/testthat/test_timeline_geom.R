context("Create Timeline")

library(ggplot2)

data(sample_eq)
clean_eq <- eq_clean_data(sample_eq)

tl_geom <- ggplot(data=subset(eq_clean_data(sample_eq), !is.na(EQ_PRIMARY) & COUNTRY %in% c("USA","MEXICO")), 
             aes(x = DATE,
                 y = COUNTRY,
                 size = EQ_PRIMARY,
                 xmin = as.Date('2000-01-01',"%Y-%m-%d"), 
                 xmax = as.Date('2016-12-31',"%Y-%m-%d"),
                 color = DEATHS, 
                 fill = DEATHS,
                 label = LOCATION_NAME)) +
   geom_timeline(alpha = .5) +
   scale_size_continuous(name = "Richter Scale Value") +
   scale_color_continuous(name = "# Deaths") +
   scale_fill_continuous(guide=FALSE) +
   labs(x = "DATE", y = "") +
   geom_timeline_label(aes(n_max = 5)) +
   theme(legend.position = "bottom",
         legend.key = element_blank(),
         legend.box = "horizontal",
         panel.background = element_blank(), 
         axis.line.x = element_line(size = 1, color = "black"), 
         axis.ticks.x = element_line(size = 1)) 

tl_stat <- ggplot(data=subset(eq_clean_data(sample_eq), !is.na(EQ_PRIMARY) & COUNTRY %in% c("USA","MEXICO")), 
                  aes(x = DATE,
                      y = COUNTRY,
                      size = EQ_PRIMARY,
                      xmin = as.Date('2000-01-01',"%Y-%m-%d"), 
                      xmax = as.Date('2016-12-31',"%Y-%m-%d"),
                      color = DEATHS, 
                      fill = DEATHS,
                      label = LOCATION_NAME)) +
   geom_timeline(alpha = .5) +
   scale_size_continuous(name = "Richter Scale Value") +
   scale_color_continuous(name = "# Deaths") +
   scale_fill_continuous(guide=FALSE) +
   labs(x = "DATE", y = "") +
   geom_timeline_label(aes(n_max = 5)) +
   theme(legend.position = "bottom",
         legend.key = element_blank(),
         legend.box = "horizontal",
         panel.background = element_blank(), 
         axis.line.x = element_line(size = 1, color = "black"), 
         axis.ticks.x = element_line(size = 1)) 

test_that("ggplot Returned", {
   expect_is(tl_geom,"ggplot")
   expect_is(tl_stat,"ggplot")
})

test_that("Plot Layers Match Expectations",{
   expect_identical(class(tl_geom$layers[[1]]$geom)[1], "GeomTimeline")
   expect_identical(class(tl_geom$layers[[1]]$stat)[1], "StatTimeline")
})
