#prep
motherTable <- cdm$motherTable %>% dplyr::collect()

#chose the start and end year below
# decide time range: 2006 - 2022 for SIDIAP
# same time range for2008 - 2022
motherTable <- motherTable %>%
  dplyr::mutate(
    year = format(.data$pregnancy_end_date, "%Y")
  ) %>%
  dplyr::filter(.data$year %in% 2010:2020)


## Figure 1) pregnancies over time (histogram or line chart)
# show the entire table (csv file in an appendix), can be downloaded from shiny
library(ggplot2)
ggplot(motherTable,aes(year))+
  geom_bar(aes(year), colour="black")+
  theme_bw()+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=7),
        axis.title=element_text(size=7,face="bold"),
        legend.text=element_text(size=7),
        legend.position = "top")+
  ylab("N")+
  xlab("Pregnancies")

## Figure 2) mode of delivery over time
# line chart
# unknowns, vaginal delivery, c-sections


figure2 <- motherTable %>%
  dplyr::group_by(year,pregnancy_mode_delivery)  %>%
  dplyr::summarise(
    count = dplyr::n()
  )

figure2 %>%
  ggplot(aes(x = year , y= count, fill = pregnancy_mode_delivery)) +
  geom_bar(stat = "identity")
  theme_bw()+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=7),
        axis.title=element_text(size=7,face="bold"),
        legend.text=element_text(size=7),
        legend.position = "top")



## Figure 3) outcomes over time (staples adding up to 100%)
# have to write the code to have the distinct entities over time (like for AnnualOverview)
# unknowns, livebirth, stillbirth, elective abortion, miscarriage, THERE SHOULD BE ONE MORE

figure3 <- motherTable %>%
  dplyr::group_by(year,pregnancy_outcome)  %>%
  dplyr::summarise(
    count = dplyr::n()
  )



figure3 %>%
  ggplot(aes(x = year , y= count, fill = pregnancy_outcome)) +
  geom_bar(stat = "identity")
theme_bw()+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=7),
        axis.title=element_text(size=7,face="bold"),
        legend.text=element_text(size=7),
        legend.position = "top")



## Figure 4) Distribution of gestational age

motherTable %>%
  ggplot(aes(gestational_length_in_day))+
  geom_histogram(aes(gestational_length_in_day), colour="black", binwidth = 0.5)+
  theme_bw()+
  theme(legend.position = "top")
