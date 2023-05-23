
p4_visualize <- list(
  
  tar_target(p4_basic_summary_histogram, {
    
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
  
  tar_target(p4_prism_summary_timeseries, {
    p2_prism_data_huc %>% 
      ggplot(aes(x = date, y = value_huc, color = huc)) + 
      geom_point(alpha = 0.25, shape=20, stroke=NA, size=2) +
      scico::scale_color_scico_d(begin = 0.15, end = 0.85,  
                                 palette = "batlow") +
      facet_grid(variable ~ ., scales = 'free_y') +
      theme_bw()
  })
  
)
