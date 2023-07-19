
p4_visualize <- list(
  
  tar_target(p4_basic_summary_histogram, {
    
    ##### Classified GEE output figures #####
    
    # Prepare a vector to use for colors
    color_vec <- bloom_plume_class_xwalk$color
    names(color_vec) <- bloom_plume_class_xwalk$val
    
    ggplot(p2_daily_summaries_clean,
           aes(x = year, y = count, fill = as.character(class))) +
      geom_bar(position="dodge", stat="identity") +
      facet_wrap(vars(mission)) + 
      scale_fill_manual(
        name = "Classification",
        values = color_vec,
        breaks = bloom_plume_class_xwalk$val,
        labels = bloom_plume_class_xwalk$nm) +
      ylab('Pixel count') + xlab('Year')
  }),
  
  ##### Observed blooms figures #####
  
  tar_target(p4_obs_blooms_map, {
    ggplot() +
      geom_sf(data = st_simplify(p2_lake_superior_watershed_filt, dTolerance = 100),
              fill = '#c4dbbc', color = 'white') +
      geom_sf(data = p2_obs_blooms_sf, aes(color = Year, shape = Verified_cyanos)) +
      theme_void() + ggtitle('Observed bloom locations in our defined AOI')
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
  })
  
)
