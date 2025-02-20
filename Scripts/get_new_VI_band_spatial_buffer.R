library(tidyverse)

fl <- list.files('./Tidy/Vegetation_Index_csv/')

# get all data from QGIS output
df <- bind_rows(lapply(1:length(fl),function(x){read.csv(paste0('./Tidy/Vegetation_Index_csv/', fl[x])) %>% 
  mutate(VI = str_split(fl, '_', 2)[[x]][1]) %>%
  mutate(Buffer = str_sub(str_split(fl, '_', 2)[[x]][2], 1,3))})) %>%
  mutate(Group = Group..from.Link.Record.) %>%
  mutate(Treatment = ifelse(Basalt == 'Yes' & Soil.Innoculation == 'No', 'Basalt',
                            ifelse(Basalt == 'No' & Soil.Innoculation == 'No', 'aControl',
                                   ifelse(Basalt == 'Yes' & Soil.Innoculation == 'Yes', 'Basalt + Innoculation', 'Innoculation')))) %>%
  mutate(singleBand = ifelse(VI %in% c('green', 'nir', 'red', 'rededge'), T, F)) %>%
  mutate(Plot = Cell.No.) %>%
  mutate(Tree.Treatment = paste(Treatment, Tree.classification)) %>% 
  filter(!is.na(X_mean)) %>%
  # filters
  filter(VI != 'sEVI') 

write.csv(df, './Tidy/Data_analysis_files/VI_band_spatial_buffer.csv')

