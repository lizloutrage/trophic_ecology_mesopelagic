isotope_data <-  utils::read.csv(here::here("data","raw-data", "isotopic_data_2021.csv"), sep = ";", header = T,dec = ",")
isotope_data_fish <- isotope_data %>% filter (species != "Meganyctiphanes_norvegica")

# Summarise By Group (sbg)
essai <- isotope_data_fish %>% 
  group_by(species, station) %>% 
  summarise(count = n(),
            mC = mean(d13c), 
            sdC = sd(d13c), 
            mN = mean(d15n), 
            sdN = sd(d15n))

couleurs <- c("#E4A33A","#F67451","#D664BE","#3DA5D9","#94B3AE","#18206F","#FD151B"
                       ,"#049A8F","#072AC8", "purple","#d193f7","#d8c2ab","#678FCB","#A63A49",
                       "#00547A","#6B54A0")

# add the layers using the summary data in sbg
ggplot(data = essai, aes(x = d13c, y = d15n)) + 
  geom_errorbar(data = essai, aes(x = mC, y = mN, ymin = mN - 1.96*sdN, ymax = mN + 1.96*sdN, col= species), width = 0) +
  geom_errorbarh(data = essai, mapping = aes(x = mC, y = mN, xmin = mC - 1.96*sdC, xmax = mC + 1.96*sdC, col= species), height = 0) + 
  scale_fill_manual(values = couleurs)+
  scale_color_manual(values= couleurs)+
  facet_wrap(~species)+
  geom_point(data = essai, aes(x = mC, y = mN, fill = species, col= species), shape = 21, size = 5, alpha = 0.7)+
  theme_minimal()

###

# Summarise By Group (sbg)
d13c_data <- isotope_data_fish %>% 
  group_by(species, trawling_depth) %>% 
  summarise(count = n(),
            mC = mean(d13c), 
            sdC = sd(d13c))

ggplot(d13c_data, aes(x = trawling_depth, y = mC)) + 
  scale_fill_manual(values = couleurs)+
  scale_color_manual(values= couleurs)+
  facet_wrap(~species)+
  geom_point()+
  theme_minimal()
