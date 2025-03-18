df <- left_join(read.csv('./CCP_report/new_NDVI_clipped_M3M_CHM_050_M3E_buffer_075_ABCDE.csv') %>%
            mutate(Plot =Id), vars, by = 'Plot') %>% filter(Tree.classification != 'NA') %>%
  filter(!Plot %in% c(36,46,47,54))


df_sp <- df %>% filter(Tree.classification == 'Spruce')
df_bl <- df %>% filter(Tree.classification != 'Spruce')