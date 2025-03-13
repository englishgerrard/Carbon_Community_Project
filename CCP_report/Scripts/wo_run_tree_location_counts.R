source('./Scripts/PACKAGES.R')
df<- read.csv('./Tidy/Data_analysis_files/VI_band_spatial_buffer.csv')
vars <- unique(data.frame(Treatment = df$Treatment, Tree.Treatment =df$Tree.Treatment,
                          Plot = df$Plot,Tree.classification = df$Tree.classification))


m3m <- left_join(read.csv('./CCP_report/m3m.csv') %>% 
                   mutate(Plot =Id), vars, by = 'Plot') %>% filter(Tree.classification != 'NA') %>%
  filter(!Plot %in% c(36,46,47,54)) 
m3e <- left_join(read.csv('./CCP_report/m3e.csv') %>% 
                   mutate(Plot =Id), vars, by = 'Plot') %>% filter(Tree.classification != 'NA') %>%
  filter(!Plot %in% c(36,46,47,54))

#remove duplicates
m3m_u <- m3m %>%
  group_by(Barcode) %>%  # Group by the 'id' column
  filter(is.na(Barcode) | Barcode == "" | row_number() == 1) %>%  # Keep blanks and first occurrence of non-blanks
  ungroup() 

m3e_u <- m3e %>%
  group_by(Barcode) %>%  # Group by the 'id' column
  filter(is.na(Barcode) | Barcode == "" | row_number() == 1) %>%  # Keep blanks and first occurrence of non-blanks
  ungroup()

c <- bind_rows(count(m3m_u, Plot, Tree.classification) %>% mutate(instrument = 'M3M'),
          count(m3e_u, Plot, Tree.classification) %>% mutate(instrument = 'M3E'))

ggplot(c,aes(x = instrument, y = n-400, fill = instrument)) +
  geom_violin(alpha = 0.3)+
  geom_boxplot( width = .3)

c %>% group_by(instrument) %>%
  summarise(count = mean(n)) %>% mutate(diff = count - 400)

ggplot(c,aes(x = Tree.classification, y = n-400, fill = Tree.classification)) +
  geom_violin(alpha = 0.3)+
  geom_boxplot( width = .3) +
  facet_wrap(~instrument)

c %>% group_by(instrument, Tree.classification) %>%
  summarise(count = mean(n)) %>% mutate(diff = count - 400)

############ 100 trees

fd <- read.csv('./Tidy/Data_analysis_files/VI_band_spatial_buffer.csv') %>% filter(VI == 'NDVI') %>% filter(Buffer == 75)

m3mf <- m3m_u %>% filter(Tree.type %in% c( "Birch", "Cherry", "Alder","Oak","Aspen","Rowan","Sitka Spruce"))
m3ef <- m3e_u %>% filter(Tree.type %in% c( "Birch", "Cherry", "Alder","Oak","Aspen","Rowan","Sitka Spruce" ))
fd <- fd %>% filter(Tree.type %in% c( "Birch", "Cherry", "Alder","Oak","Aspen","Rowan","Sitka Spruce" ))

m <- m3mf %>% count(Plot, Tree.type)
e <- m3ef %>% count(Plot, Tree.type)

f <- fd %>% count(Plot, Tree.type)

d <- na.omit(bind_rows(m %>% mutate(instrument = 'M3M'),
          e %>% mutate(instrument = 'M3E'),
          f %>% mutate(instrument = 'Ground truth')))


comparison_data <- f %>%
  rename(Actual = n) %>%
  left_join(e %>% rename(Pred_e = n), by = c("Plot", "Tree.type")) %>%
  left_join(m %>% rename(Pred_m = n), by = c("Plot", "Tree.type"))

cdf <- na.omit(comparison_data) %>% group_by(Tree.type) %>%
  summarise_all(mean)
cdf[,-2]

ss <-filter(d, Tree.type == 'Sitka Spruce') %>% mutate(Tree.classification = 'Spruce')

bl <-filter(d, Tree.type != 'Sitka Spruce')
bl_all <- bl %>% group_by(instrument, Plot) %>%
  summarise(n = sum(n)) %>% mutate(Tree.classification = 'Native Broadleaf')

a <- bind_rows(ss, bl_all)

ggplot(a, aes(x = instrument, y = n, colour = Tree.classification)) +
  geom_boxplot() 

ggplot(bl, aes(x = instrument, y = n, colour = Tree.type)) +
  geom_boxplot() +
  facet_wrap(~Tree.type, scales = 'free_y')

####### old bits below #################













#################
mean(count(m3m, Plot)$n)
mean(count(m3e, Plot)$n)

mean(count(m3m_u, Plot)$n)
mean(count(m3e_u, Plot)$n)

mean(count(filter(m3m, Tree.classification == 'Spruce'),Plot)$n)
mean(count(filter(m3e, Tree.classification == 'Spruce'),Plot)$n)

mean(count(filter(m3m_u, Tree.classification != 'Spruce'),Plot)$n)
mean(count(filter(m3e_u, Tree.classification != 'Spruce'),Plot)$n)

mean(filter(count(filter(m3m, Tree.classification == 'Spruce'),Plot, Tree.type), Tree.type== 'Sitka Spruce')$n)
mean(filter(count(filter(m3e, Tree.classification == 'Spruce'),Plot, Tree.type), Tree.type== 'Sitka Spruce')$n)

mean(filter(count(filter(m3m_u, Tree.classification == 'Spruce'),Plot, Tree.type), Tree.type== 'Sitka Spruce')$n)
mean(filter(count(filter(m3e_u, Tree.classification == 'Spruce'),Plot, Tree.type), Tree.type== 'Sitka Spruce')$n)

mean(count(filter(m3m, !Tree.type %in% c('Sitka Spruce', '')),Plot)$n)
mean(count(filter(m3e, !Tree.type %in% c('Sitka Spruce', '')),Plot)$n)    

mean(count(filter(m3m_u, !Tree.type %in% c('Sitka Spruce', '')),Plot)$n)
mean(count(filter(m3e_u, !Tree.type %in% c('Sitka Spruce', '')),Plot)$n)

mean(count(filter(m3m, Barcode != ''), Barcode, Plot)$n)
mean(count(filter(m3e, Barcode != ''), Barcode, Plot)$n)


View(count(filter(m3e, Barcode != ''), Barcode, Plot))

c %>% group_by(instrument) %>%
  summarise(count = mean(n))
ggplot(c,aes(x = Plot, y = n-400, colour = instrument)) +
  geom_boxplot( position = 'dodge'  ) 
