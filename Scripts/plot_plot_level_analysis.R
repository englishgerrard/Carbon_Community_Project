## whole image analysis
df <- bind_rows(
read.csv('./Tidy/image_area_analysis/green.csv') %>%
  mutate(band = 'green'),
read.csv('./Tidy/image_area_analysis/red.csv') %>%
  mutate(band = 'red'),
read.csv('./Tidy/image_area_analysis/rededge.csv') %>%
  mutate(band = 'rededge'),
read.csv('./Tidy/image_area_analysis/nir.csv') %>%
  mutate(band = 'nir'))

ndvi <- read.csv('./Tidy/image_area_analysis/NDVI.csv') %>%
  mutate(band = 'NDVI')

ggplot(df, aes(x = factor(band, levels = c('green','red','rededge', 'nir')), y = X_mean, colour = Group, group = Group)) +
  geom_line() +
  geom_point() +
  geom_hline(data = ndvi, aes(yintercept = X_mean/10-0.05, colour = Group), size = 2, alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(
    name = "Reflectnace (?)",
    sec.axis = sec_axis(~ . * 10+0.5, name = "NDVI")
  ) +
  xlab('band')+
  theme(aspect.ratio = .75)


ggsave(filename = './Tidy/Figures/Flight_level_Band_analysis.png', dpi = 600, width = 12)

