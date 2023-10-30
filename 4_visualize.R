
source('4_visualize/src/visualize_helpers.R')

p4_visualize <- list(
  
  ##### Observed blooms figures #####
  
  tar_target(p4_obs_blooms_map, {
    ggplot() +
      geom_sf(data = st_simplify(p2_lake_superior_watershed_filt, dTolerance = 100),
              fill = '#c4dbbc', color = 'white') +
      geom_sf(data = p2_obs_blooms_sf, aes(color = Year, shape = Verified_cyanos)) +
      theme_void() + ggtitle('Observed bloom locations in our defined AOI')
  }),
  
  tar_target(p4_obs_blooms_timeseries, {
    ggplot(p2_obs_blooms_details, aes(x = `Start Date`, 
                                      y = Verified_cyanos,
                                      color = Verified_cyanos)) +
      geom_jitter(size=2) + 
      theme_bw() + 
      ggtitle('Jittered timeline of observed blooms in Lake Superior',
              subtitle = 'Separated by whether or not cyanos was verified in a microscope')
  }),
  
  ##### PRISM climate driver summary figures #####
  
  tar_target(p4_prism_summary_timeseries, {
    p2_prism_data_huc %>% 
      ggplot(aes(x = date, y = value_huc, color = huc)) + 
      geom_point(alpha = 0.25, shape=20, stroke=NA, size=2) +
      scico::scale_color_scico_d(begin = 0.15, end = 0.85,  
                                 palette = "batlow") +
      facet_grid(variable ~ ., scales = 'free_y', switch = "y",
                 labeller = as_labeller(c(tmean = "Mean temperature, deg C", 
                                          ppt = "Daily precipitation, mm"))) +
      theme_bw() + ylab("") + xlab("Date") +
      theme(strip.background = element_blank(),
            strip.placement = "outside",
            strip.text.y = element_text(size = 15))
  }),
  
  tar_target(p4_prism_summary_boxes, {
    p2_prism_data_huc %>% 
      # Log the precipitation
      mutate(value_huc = ifelse(variable == "ppt", log10(value_huc), value_huc)) %>% 
      ggplot(aes(x = decade, y = value_huc, fill = huc)) + 
      geom_boxplot() +
      scico::scale_fill_scico_d(begin = 0.15, end = 0.85, 
                                palette = "batlow") +
      facet_grid(variable ~ ., scales = 'free_y', switch = "y",
                 labeller = as_labeller(c(tmean = "Mean temperature, deg C", 
                                          ppt = "Logged daily precipitation, mm"))) +
      theme_bw() + ylab("") + xlab("Decade") +
      theme(strip.background = element_blank(),
            strip.placement = "outside",
            strip.text.y = element_text(size = 15))
  }),
  
  ##### Maps of sediment presence from classified rasters #####
  
  tar_target(p4_sediment_sentinel_heatmap_png, 
             make_sediment_heatmap(in_file = p2_sediment_heatmap_sentinel_terraqs,
                                   out_file = '4_visualize/out/sediment_heatmap_sentinel.png',
                                   mission = 'Sentinel',
                                   lake_sf = p2_lake_superior_watershed_dissolved),
             format='file'),
  
  tar_target(p4_sediment_landsat_heatmap_png, 
             make_sediment_heatmap(in_file = p2_sediment_heatmap_landsat_terraqs,
                                   out_file = '4_visualize/out/sediment_heatmap_landsat.png',
                                   mission = 'Landsat',
                                   lake_sf = p2_lake_superior_watershed_dissolved),
             format='file')
  
)
