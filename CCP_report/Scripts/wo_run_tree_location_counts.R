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

# Keep blanks and remove duplicates for non-blank values
m3m_u <- m3m %>%
  group_by(Barcode) %>%  # Group by the 'id' column
  filter(is.na(Barcode) | Barcode == "" | row_number() == 1) %>%  # Keep blanks and first occurrence of non-blanks
  ungroup() 

m3e_u <- m3e %>%
  group_by(Barcode) %>%  # Group by the 'id' column
  filter(is.na(Barcode) | Barcode == "" | row_number() == 1) %>%  # Keep blanks and first occurrence of non-blanks
  ungroup()


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


