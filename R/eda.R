library(ggplot2)
# Port Type 

trade_data %>%
  group_by(port_type) %>%
  summarise(value = sum(value),
            mass = sum(mass)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x = port_type, y = value))

trade_data %>%
  filter(port_type == 'Sea') %>%
  count(country) %>%
  arrange(desc(n))

trade_data %>%
  filter(port_type == 'Air') %>%
  count(country) %>%
  arrange(desc(n))


trade_data %>%
  filter(is.na(port_type)) %>%
  count(country) %>%
  arrange(desc(n))



# Conclusion -> for both mass & value, most of the food is imported by
# Sea rather than Air. Sea & Air both have similar 'top 10' countries
# we import from (by commodity code count), and some countries also 
# appear in NA's top 10. Most (> 90%) of port_types that are missing are
# labelled as 'Inland Clearance'.




# finds commodity codes that are all 'port_type' = NA
no_port_data_cn8 <-   
  trade_data %>%
  distinct(commodity_code, port_type, description) %>%
  group_by(commodity_code) %>%
  mutate(cc_n = n()) %>%
  filter(cc_n == 1 & is.na(port_type))
 
nrow(no_port_data_cn8)/length(unique(trade_data$commodity_code))



# Conclusion -> use CN8 codes rather than HS6 because HS6 codes are not granular
# enough. For example: 070999 code has fresh/chilled fennel, capers and sweetcorn
# at the CN8 level but not at HS6.
# The codes with no Air/Sea port data will be removed from the master dataset.






  