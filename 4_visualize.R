
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
  })
  
)
