
make_sediment_heatmap <- function(in_file, out_file, mission, lake_sf) {
  
  common_crs <- 'epsg:4326'
  
  # Load sediment data & reproject to a common crs
  sediment_raster <- load_terraqs(in_file)
  sediment_raster_proj <- project(sediment_raster, common_crs)
  
  # Reproject lake watershed shape to a common crs
  lake_sup_ws <- st_transform(lake_sf, crs=common_crs)
  
  p<-ggplot() +
    tidyterra::geom_spatraster(data = sediment_raster_proj) +
    geom_sf(data = lake_sup_ws, fill = '#9ba68e', color = NA) +
    # Not plotting NAs of the raster
    tidyterra::scale_fill_whitebox_c(
      palette = "arid", 
      direction = -1,
      n.breaks = 5
    ) +
    labs(fill="sediment") +
    theme_void() +
    theme(plot.background = element_rect(fill='#c3d2e0', color=NA),
          panel.background = element_rect(fill='#c3d2e0', color=NA)) +
    ggtitle(sprintf('Sediment heatmap for %s data', mission))
  
  ggsave(out_file, p, dpi=300, width=2000, height=1400, units='px')
  return(out_file)
}

make_sediment_ts <- function(sediment_ts_byOutlet) {
  # ggplot(sediment_ts_byOutlet, 
  #        aes(x = as.Date(date), 
  #            y = sediment_outlet_pct,
  #            color = mission)) +
  #   geom_point() +
  #   facet_grid(river_outlet ~ .) +
  #   ylab('Percent of river outlet with sediment') +
  #   xlab('Date') +
  #   ggtitle('Time series summary of % sediment per river outlet',
  #           subtitle = 'From sediment rasters') +
  #   theme_bw() +
  #   theme(strip.text.y = element_text(angle = 0))
  
  ggplot(sediment_ts_byOutlet,
         aes(y=sediment_outlet_pct, x=year, group=year_mission, 
             fill=mission, color=mission)) +
    geom_boxplot(position = position_dodge(preserve = "single")) +
    facet_grid(river_outlet ~ .) +
    theme_bw() +
    ylab('Daily % sediment') +
    xlab('Year') +
    ggtitle('Boxplots per year per mission for daily % sediment')
}

