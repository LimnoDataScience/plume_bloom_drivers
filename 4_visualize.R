
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
  
  tar_target(p4_prism_summary, {
    p2_prism_data %>% 
      left_join(p1_lake_superior_grid_centers) %>% 
      # These cells all have NA (assuming that these
      # are NA because the centroids are over water)
      filter(!cell_no %in% c(4, 6, 8, 11, 12)) %>% 
      ggplot(aes(x = date, y = value, 
                 color = cell_no)) + 
      geom_point() +
      scico::scale_color_scico(begin = 0.15, end = 0.85,
                               palette = "batlow") +
      facet_grid(cell_no ~ variable, scales = 'free')
  })
  
)
