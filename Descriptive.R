
setwd("~/Github/TickMaps")

library(ggthemr)
library(LaCroixColoR)
library(tidyverse)
library(patchwork)
library(RColorBrewer)

set.seed(5)

tk <- read_csv("TickMaps.csv")

ggthemr('dust')
x <- ggthemr('flat', set_theme = FALSE)
y <- ggthemr('light', set_theme = FALSE)
set.seed(5)
ramp1 <- colorRampPalette(unlist(x$palette$swatch))(21)
ramp2 <- colorRampPalette(unlist(y$palette$swatch))(9)

tk %>% select(Date, Pathogen_Disease) %>%
  mutate(Pathogen_Disease = strsplit(Pathogen_Disease, ",")) %>% 
  unnest(Pathogen_Disease) %>%
  group_by(Pathogen_Disease, Date) %>% 
  tally() %>% 
  ungroup() %>%
  na.omit() %>%
  complete(Pathogen_Disease, Date, fill = list(n = 0)) %>%
  group_by(Pathogen_Disease) %>% 
  mutate(Studies = cumsum(n)) %>%
  dplyr::rename(Disease = Pathogen_Disease) %>%
  ggplot(aes(x = Date, y = Studies, fill = Disease)) + 
  geom_area() +
  theme(legend.position = "bottom",
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = 'black', fill = NA),
        legend.text = element_text(size = 7.5)) +
  scale_fill_manual(values = ramp1) -> g1

tk %>% select(Date, Vector) %>%
  mutate(Vector = strsplit(Vector, ",")) %>% 
  unnest(Vector) %>%
  mutate(Vector = word(Vector, 1)) %>% 
  group_by(Vector, Date) %>% 
  tally() %>% 
  ungroup() %>%
  na.omit() %>%
  complete(Vector, Date, fill = list(n = 0)) %>%
  group_by(Vector) %>% 
  mutate(Studies = cumsum(n)) %>%
  ggplot(aes(x = Date, y = Studies, fill = Vector)) + 
  geom_area() + 
  theme(legend.position = "bottom") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = 'black', fill = NA),
        legend.text = element_text(size = 7.5)) +
  scale_fill_manual(values = ramp2) -> g2

g2 / g1

ggthemr_reset()

ggthemr('flat')

cat.rec <- c(`Grey data` = 'Point data')

tk %>% select(Date, Category) %>%
  mutate(Category = strsplit(Category, ",")) %>% 
  unnest(Category) %>%
  group_by(Category, Date) %>% 
  tally() %>% 
  ungroup() %>%
  na.omit() %>%
  complete(Category, Date, fill = list(n = 0)) %>%
  mutate(Category = recode(Category, !!!cat.rec)) %>%
  group_by(Category) %>% 
  mutate(Studies = cumsum(n)) %>%
  ggplot(aes(x = Date, y = Studies, fill = Category)) + 
  geom_area() + 
  theme(legend.position = "bottom") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = 'black', fill = NA))

################################ 2. Pie Charts


ggthemr('fresh')

rec1 <- c(checked = 'Tick data as proxy', .missing = 'Real pathogen data')

tk %>% mutate(Tick = recode(VectorNoPath, !!!rec1)) %>%
  count(Tick) %>% 
  ggplot(aes(x = "", y = n, fill = Tick)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start = 0, direction = -1) +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) -> g1

rec2 <- c(checked = 'Human case data', .missing = 'No human case data')

tk %>% mutate(Human = recode(`Human case data`, !!!rec2)) %>%
  count(Human) %>%
  ggplot(aes(x = "", y = n, fill = Human)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start = 0, direction = -1) +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) -> g2

######

wild.vec <- c('Boar', 'Deer', 'Hare', 'Shrew', 'Hedgehog', 'Moose', 'Rodent', 'White-tailed deer',
              'Wild canid', 'Wild pig', 'Wild ungulate')

tk %>% select(Title, `Host species`) %>%
  mutate(Hosts = strsplit(`Host species`, ",")) %>% 
  unnest(Hosts) %>%
  mutate(Wildlife = case_when(Hosts %in% wild.vec ~ 1,
                              !(Hosts %in% wild.vec) ~ 0)) %>% 
  group_by(Title) %>% 
  summarize(Wildlife = max(Wildlife)) %>% 
  mutate(Wildlife = case_when(Wildlife == 1 ~ 'Wildlife data',
                              Wildlife == 0 ~ 'No wildlife data')) %>%
  mutate(Wildlife = as_factor(Wildlife)) %>%
  mutate(Wildlife = fct_relevel(Wildlife, 'Wildlife data', 'No wildlife data')) %>%
  count(Wildlife) %>%
  ggplot(aes(x = "", y = n, fill = Wildlife)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) -> g3

live.vec <- c('Cattle', 'Domestic dog', 'Domestic pig', 'Goat', 'Sheep', 'Buffalo')

tk %>% select(Title, `Host species`) %>%
  mutate(Hosts = strsplit(`Host species`, ",")) %>% 
  unnest(Hosts) %>%
  mutate(Livestock = case_when(Hosts %in% live.vec ~ 1,
                              !(Hosts %in% live.vec) ~ 0)) %>% 
  group_by(Title) %>% 
  summarize(Livestock = max(Livestock)) %>% 
  mutate(Livestock = case_when(Livestock == 1 ~ 'Livestock data',
                              Livestock == 0 ~ 'No livestock data')) %>%
  mutate(Livestock = as_factor(Livestock)) %>%
  mutate(Livestock = fct_relevel(Livestock, 'Livestock data', 'No livestock data')) %>%
  count(Livestock) %>%
  ggplot(aes(x = "", y = n, fill = Livestock)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) -> g4

(g1 + g2 + g4 + g3)

###################################################################

wild.recode <- c(`Wild pig` = 'Suid',
                 Boar = 'Suid',
                 `White-tailed deer` = 'Ungulate',
                 Deer = 'Ungulate',
                 Moose = 'Ungulate',
                 Hare = 'Other small mammal',
                 Shrew = 'Other small mammal',
                 Hedgehog = 'Other small mammal',
                 `Wild canid` = 'Canid',
                 `Wild ungulate` = 'Ungulate')

x <- ggthemr('grape', set_theme = FALSE)

tk %>% select(Title, `Host species`) %>%
  mutate(Hosts = strsplit(`Host species`, ",")) %>% 
  unnest(Hosts) %>%
  filter(Hosts %in% wild.vec) %>% 
  select(Title, Hosts) %>% unique() %>% 
  mutate(Hosts = recode(Hosts, !!!wild.recode)) %>%
  mutate(Hosts = fct_relevel(Hosts, 'Canid','Suid','Ungulate','Rodent','Other small mammal')) %>%
  count(Hosts) %>%
  ggplot(aes(x = "", y = n, fill = Hosts)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) + 
  scale_fill_manual(values = unlist(x$palette$swatch)) -> p1

live.vec <- c('Cattle', 'Domestic dog', 'Domestic pig', 'Goat', 'Sheep', 'Buffalo')

live.recode <- c('Domestic dog' = 'Dog',
                 'Domestic pig' = 'Pig')

y <- ggthemr('light', set_theme = FALSE)

tk %>% select(Title, `Host species`) %>%
  mutate(Hosts = strsplit(`Host species`, ",")) %>% 
  unnest(Hosts) %>%
  filter(Hosts %in% live.vec) %>% 
  select(Title, Hosts) %>% unique() %>% 
  mutate(Hosts = recode(Hosts, !!!live.recode)) %>%
  count(Hosts) %>%
  ggplot(aes(x = "", y = n, fill = Hosts)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) + 
  scale_fill_manual(values = unlist(y$palette$swatch)) -> p2

p1 + p2
