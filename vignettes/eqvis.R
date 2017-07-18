## ----clean_data----------------------------------------------------------
library(eqvis)
sample3 <- head(sample_eq[,c("YEAR","MONTH","DAY","LOCATION_NAME","LATITUDE","LONGITUDE")],3)
print.data.frame(sample3,row.names=FALSE)
print.data.frame(eq_clean_data(sample3),row.names=FALSE)

## ----timeline, fig.width=7-----------------------------------------------
library(ggplot2)
library(scales)
library(eqvis)

ggplot(data=subset(eq_clean_data(sample_eq), !is.na(EQ_PRIMARY) & COUNTRY %in% c("USA")), 
       aes(x = DATE,
           y = COUNTRY,
           size = EQ_PRIMARY,
           xmin = as.Date('2012-01-01',"%Y-%m-%d"), 
           xmax = as.Date('2016-12-31',"%Y-%m-%d"),
           color = DEATHS, 
           fill = DEATHS,
           label = LOCATION_NAME)) +
    geom_timeline(alpha = .5) +
    scale_size_continuous(name = "Richter Scale Value") +
    scale_color_continuous(name = "# Deaths") +
    scale_fill_continuous(guide=FALSE) +
    labs(x = "DATE", y = "") +
    geom_timeline_label(aes(n_max = 3)) +
    theme(legend.position = "bottom",
          legend.key = element_blank(),
          legend.box = "horizontal",
          panel.background = element_blank(), 
          axis.line.x = element_line(size = 1, color = "black"), 
          axis.ticks.x = element_line(size = 1)) 

## ----map, out.width='100%', message=FALSE--------------------------------
library(eqvis)
library(leaflet)
library(magrittr)
library(dplyr)
library(lubridate)

sample_eq %>% 
eq_clean_data() %>% 
dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
dplyr::mutate(popup_text = eq_create_label(.)) %>% 
eq_map(annot_col = "popup_text")

## ----sample_eq-----------------------------------------------------------
str(sample_eq)

