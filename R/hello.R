#libraries needed:
#data.table
#DBI

#function assumes datetime in logs is in UTC

usethis::use_package("ggplot2")

data(diamonds)

diamonds %>%
  ggplot(aes(cut, carat))+
  geom_point()
