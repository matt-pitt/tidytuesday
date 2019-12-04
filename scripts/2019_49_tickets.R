library(tidyverse)
library(scales)

tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

#find out what types of fines the police are issuing
police_df <- tickets %>%
  select(violation_desc, issuing_agency, fine) %>%
  filter(issuing_agency == "POLICE") %>%
  mutate(violation_desc = as.factor(violation_desc)) %>%
  group_by(violation_desc, issuing_agency) %>%
  summarise(n = n(), dollars = sum(fine)) %>%
  ungroup() %>%
  filter(n > 2000) %>%
  arrange(desc(dollars))
  
p <- ggplot(police_df, aes(x = reorder(violation_desc, dollars), y = dollars)) + 
  geom_col(fill = "gold", colour = "black") +
  geom_text(aes(label = paste(comma(n),"Incidents")), hjust = "inward", color = "darkblue", size = 4) +
  scale_y_continuous(labels = comma) +
  coord_flip() + 
  labs(title = "Police Top Revenue Generating Offences",
       y = "Revenue Dollars",
       x = "Violation Description") +
  theme(plot.background = element_rect("lightblue"),
        panel.background = element_rect("lightblue"),
        panel.grid = element_line("lightblue"))

ggsave('plots/plot_attempt_1.png', p)
  
glimpse(police_df)
