## all functions that created for covid_2019 project -------------------------

## Fun1: Plot cumulative/incremental count and incremental % change ------------
EDA_Plot_1 <- function(province_state, dat, dat_inc){
  
  ps <- province_state
  
  ## plot cumulative counts
  # confirm
  gg_confirm <- ggplot(dat[province_state == ps & type == "confirm"], aes(x = time, y = count)) +
    geom_point(aes(size = count), color = "blue1", show.legend = FALSE) +
    geom_line(linetype = "dashed", color = "blue1") +
    scale_x_date(date_minor_breaks = "1 day") + 
    labs(title = paste0(ps, " Cumulative Confirmed Count"))
  # plot(gg_confirm)
  # death
  gg_death <- ggplot(dat[province_state == ps & type == "death"], aes(x = time, y = count)) +
    geom_point(aes(size = count), color = "red", show.legend = FALSE) +
    geom_line(linetype = "dashed", color = "red") +
    scale_x_date(date_minor_breaks = "1 day") + 
    labs(title = paste0(ps, " Cumulative Death Count"))
  # plot(gg_death)
  # recovery
  gg_recovery <- ggplot(dat[province_state == ps & type == "recovery"], aes(x = time, y = count)) +
    geom_point(aes(size = count), color = "cyan4", show.legend = FALSE) +
    geom_line(linetype = "dashed", color = "cyan4") + 
    scale_x_date(date_minor_breaks = "1 day") + 
    labs(title = paste0(ps, " Cumulative Recovery Count"))
  # plot(gg_recovery)
  
  ## plot incremental count changes
  # confirm
  gg_inc_confirm <- ggplot(dat_inc[province_state == ps & type == "confirm"], aes(x = time, y = count_inc)) +
    geom_point(aes(size = count_inc), color = "blue1", show.legend = FALSE) +
    geom_line(linetype = "dashed", color = "blue1") +
    scale_x_date(date_minor_breaks = "1 day") + 
    labs(title = paste0(ps, " Confirmed Count Change"))
  # plot(gg_inc_confirm)
  # death
  gg_inc_death <- ggplot(dat_inc[province_state == ps & type == "death"], aes(x = time, y = count_inc)) +
    geom_point(aes(size = count_inc), color = "red", show.legend = FALSE) +
    geom_line(linetype = "dashed", color = "red") +
    scale_x_date(date_minor_breaks = "1 day") + 
    labs(title = paste0(ps, " Death Count Change"))
  # plot(gg_inc_death)
  # recovery
  gg_inc_recovery <- ggplot(dat_inc[province_state == ps & type == "recovery"], aes(x = time, y = count_inc)) +
    geom_point(aes(size = count_inc), color = "cyan4", show.legend = FALSE) +
    geom_line(linetype = "dashed", color = "cyan4") +
    scale_x_date(date_minor_breaks = "1 day") + 
    labs(title = paste0(ps, " Recovery Count Change"))
  # plot(gg_inc_recovery)
  
  ## plot incremental Percentage changes
  # confirm
  gg_per_confirm <- ggplot(dat_inc[province_state == ps & type == "confirm"], aes(x = time, y = count_inc_percent)) +
    geom_point(aes(size = count_inc_percent), color = "blue1", show.legend = FALSE) +
    geom_line(linetype = "dashed", color = "blue1") +
    scale_x_date(date_minor_breaks = "1 day") + 
    labs(title = paste0(ps, " Confirmed Percent Change"))
  # plot(gg_per_confirm)
  # death
  gg_per_death <- ggplot(dat_inc[province_state == ps & type == "death"], aes(x = time, y = count_inc_percent)) +
    geom_point(aes(size = count_inc_percent), color = "red", show.legend = FALSE) +
    geom_line(linetype = "dashed", color = "red") +
    scale_x_date(date_minor_breaks = "1 day") + 
    labs(title = paste0(ps, " Death Percent Change"))
  # plot(gg_per_death)
  # recovery
  gg_per_recovery <- ggplot(dat_inc[province_state == ps & type == "recovery"], aes(x = time, y = count_inc_percent)) +
    geom_point(aes(size = count_inc_percent), color = "cyan4", show.legend = FALSE) +
    geom_line(linetype = "dashed", color = "cyan4") +
    scale_x_date(date_minor_breaks = "1 day") + 
    labs(title = paste0(ps, " Recovery Percent Change"))
  # plot(gg_per_recovery)
  
  ## combine plots together
  gg_comb <- ggarrange(gg_confirm + labs(title = ""),
                       gg_death + labs(title = ""),
                       gg_recovery + labs(title = ""), 
                       gg_inc_confirm + labs(title = ""),
                       gg_inc_death + labs(title = ""),
                       gg_inc_recovery + labs(title = ""),
                       gg_per_confirm + labs(title = ""),
                       gg_per_death + labs(title = ""),
                       gg_per_recovery + labs(title = ""),
                       labels = c("Comfirmed Cum.", "Death Cum.", "Recovery Cum.",
                                  "Comfirmed Chg", "Death Chg", "Recovery Chg",
                                  "Comfirmed Chg %", "Death Chg %", "Recovery Chg %"),
                       ncol = 3, nrow = 3)
  
  return(gg_comb)
  
}

