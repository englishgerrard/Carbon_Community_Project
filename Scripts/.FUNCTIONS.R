# functions 
f_gps <- read.csv('./Tidy/Data_analysis_files/VI_band_spatial_buffer.csv')
vars <- unique(data.frame(Treatment = f_gps$Treatment, Tree.Treatment =f_gps$Tree.Treatment,
                          Plot = f_gps$Plot,Tree.classification = f_gps$Tree.classification))
rm(f_gps)
