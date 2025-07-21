# iSSA targets temporarily removed from master code -----
# Jack Hendrix
# 3 March 2025

# Targets: prep -----------------------------------------------------------


  
  tar_target(
    tracks_buffer,
    extract_lc_radius(
      tracks_extract,
      crs,
      lc,
      legend,
      100
    )
  ),
  
  tar_target(
    tracks_w_old,
    old_burn_dist(
      tracks_buffer,
      crs,
      old_burn
    )
  ),
  
  tar_target(
    tracks_w_both,
    new_burn_dist(
      tracks_w_old,
      crs,
      new_burn
    )
  ),
  
  tar_target(
    tracks_full,
    dist_to_roads(
      tracks_w_both,
      crs,
      roads
    )
  ),
  
  tar_target(
    avail_lc,
    calc_availability(tracks_w_both, 'lc_description', 'proportion', split_by)
  )
)



# Targets: distributions --------------------------------------------------
targets_distributions <- c(
  tar_target(
    dist_parameters,
    calc_distribution_parameters(tracks_random),
    pattern = map(tracks_random)
  ),
  tar_target(
    dist_sl_plots,
    plot_distributions(tracks_resampled, 'sl_'),
    pattern = map(tracks_resampled),
    iteration = 'list'
  ),
  tar_target(
    dist_ta_plots,
    plot_distributions(tracks_resampled, 'ta_'),
    pattern = map(tracks_resampled),
    iteration = 'list'
  )
)

# Targets: annual fire model ----------------------------------------------------------
targets_fire <- c(
  tar_target(
    model_prep,
    prepare_model(tracks_full, locs_nn)
  ),
  tar_target(
    fire_model,
    model_fire_bin(model_prep)
  ),
  tar_target(
    fire_model_check,
    model_check(fire_model)
  ),
  
  tar_target(
    fire_VIF,
    summarise_model(fire_model, model_prep)
  )
)

# Targets: seasonal fire model ----------------------------------------
targets_fire_seasonal <- c(
  
  tar_target(
    season_prep,
    as.data.table(model_prep)[, tar_group := .GRP, by = c('season')],
    iteration = 'group'
  ),
  tar_target(
    season_key,
    unique(season_prep[, .SD, .SDcols = c(seasonal_split, 'tar_group')])
  ),
  tar_target(
    s_fire_model,
    model_fire_bin(season_prep),
    map(season_prep)
  ),
  tar_target(
    s_fire_model_output,
    summarise_model(s_fire_model, season_prep),
    map(s_fire_model, season_prep)
  ),
  
  tar_target(
    s_fire_model_check,
    model_check(s_fire_model),
    map(s_fire_model)
  )
)


# Targets: fire output and effects ------------------------------------------------------------
targets_fire_effects <- c(
  tar_target(
    indiv_fire,
    indiv_estimates(fire_model, "fire")
  ),
  tar_target(
    fire_boxplot,
    plot_box_horiz(indiv_fire, plot_theme(), "fire")
  ),
  tar_target(
    sum_fire,
    sum_plot(indiv_fire, plot_theme(), "fire")
  ),
  tar_target(
    s_indiv_fire,
    indiv_seasonal(s_fire_model, season_key, "fire"),
    pattern = map(s_fire_model, season_key)
  ),
  tar_target(
    s_fire_boxplot,
    plot_boxplot_seasonal(s_indiv_fire, plot_theme(), "fire"),
    pattern = map(s_indiv_fire)
  ),
  
  tar_target(
    sum_s_fire,
    sum_seasonal(s_indiv_fire, plot_theme(), "fire"),
    pattern = map(s_indiv_fire)
  )
)
# Targets: speed from annual fire ------------------------------------------------------------
targets_speed_fire <- c(
  tar_target(
    prep_speed_fire,
    prepare_speed(
      DT = model_prep,
      summary = indiv_fire,
      model = "fire",
      params = dist_parameters
    )
  ),
  tar_target(
    calc_speed_forest_fire,
    calc_speed(prep_speed_fire, 'forest fire', seq = 0:1)
  ),
  tar_target(
    plot_speed_forest_fire,
    plot_dist(calc_speed_forest_fire, plot_theme(), 'forest') +
      labs(x = 'Proportion forested (100 m radius)', y = 'Speed (m/hr)')
  ),
  tar_target(
    calc_speed_new_burn,
    calc_speed(prep_speed_fire, 'dist_to_new_burn', seq(1, 5000, length.out = 100L))
  ),
  tar_target(
    plot_speed_new_burn,
    plot_dist(calc_speed_new_burn, plot_theme()) +
      labs(x = 'Distance to younger burns (km)', y = 'Speed (m/hr)')
  ),
  tar_target(
    calc_speed_old_burn,
    calc_speed(prep_speed_fire, 'dist_to_old_burn', seq(1, 5000, length.out = 100L))
  ),
  tar_target(
    plot_speed_old_burn,
    plot_dist(calc_speed_old_burn, plot_theme()) +
      labs(x = 'Distance to older burns (km)', y = 'Speed (m/hr)')
  ),
  tar_target(
    fire_plots,
    save_plot(plot_speed_forest_fire, "fire_model_speed_forest",
              plot_speed_new_burn, "fire_model_speed_new_burn",
              plot_speed_old_burn, "fire_model_speed_old_burn")
  )
)

# Targets: speed from seasonal fire --------------------------
targets_speed_fire_seasonal <- c(
  
  tar_target(
    prep_speed_s_fire,
    prepare_speed_seasonal(
      DT = season_prep,
      summary = s_indiv_fire,
      model = "fire",
      params = dist_parameters,
      season_key = season_key
    ),
    map(s_indiv_fire, season_key)
  ),
  tar_target(
    calc_speed_forest_s_fire,
    calc_speed_seasonal(prep_speed_s_fire, 'forest', "fire", seq = 0:1, season_key),
    map(prep_speed_s_fire, season_key)
  ),
  tar_target(
    plot_speed_forest_s_fire,
    plot_dist_seasonal(calc_speed_forest_s_fire, plot_theme(), "forest fire")
  ),
  
  tar_target(
    calc_speed_s_new_fire,
    calc_speed_seasonal(prep_speed_s_fire, 'dist_to_new_burn', "fire", seq(1, 5000, length.out = 100L), season_key),
    map(prep_speed_s_fire, season_key)
  ),
  tar_target(
    plot_speed_s_new_fire,
    plot_dist_seasonal(calc_speed_s_new_fire, plot_theme(), "younger burns")
  ),
  
  tar_target(
    calc_speed_s_old_fire,
    calc_speed_seasonal(prep_speed_s_fire, 'dist_to_old_burn', "fire", seq(1, 5000, length.out = 100L), season_key),
    map(prep_speed_s_fire, season_key)
  ),
  tar_target(
    plot_speed_s_old_fire,
    plot_dist_seasonal(calc_speed_s_old_fire, plot_theme(), "older burns")
  )
)

# Targets: RSS from annual fire model -----------------------------------------------------------
targets_rss_fire <- c(
  tar_target(
    pred_h1_new_burn,
    predict_h1_new_burn(model_prep, fire_model)
  ),
  tar_target(
    pred_h1_old_burn,
    predict_h1_old_burn(model_prep, fire_model)
  ),
  tar_target(
    pred_h1_forest_fire,
    predict_h1_forest(model_prep, fire_model, "fire")
  ),
  tar_target(
    pred_h2_fire,
    predict_h2(model_prep, fire_model, "fire")
  ),
  tar_target(
    pred_h2_fire_forest,
    predict_h2_forest(model_prep, fire_model, "fire")
  ),
  tar_target(
    rss_forest_fire,
    calc_rss(pred_h1_forest_fire, 'h1_forest', pred_h2_fire_forest, 'h2')
  ),
  tar_target(
    rss_old_burn,
    calc_rss(pred_h1_old_burn, 'h1_old_burn', pred_h2_fire, 'h2')
  ),
  tar_target(
    rss_new_burn,
    calc_rss(pred_h1_new_burn, 'h1_new_burn', pred_h2_fire, 'h2')
  ),
  tar_target(
    plot_rss_forest_fire,
    plot_rss(rss_forest_fire, plot_theme()) +
      labs(x = 'Proportion forested (100m)', y = 'logRSS',
           title = 'RSS compared to 0% forest (fire model)')
  ),
  tar_target(
    plot_rss_new_burn,
    plot_rss(rss_new_burn, plot_theme()) +
      labs(x = 'Distance to younger burns (km)', y = 'logRSS',
           title = 'RSS compared to median distance from post-1992 burns')
  ),
  tar_target(
    plot_rss_old_burn,
    plot_rss(rss_old_burn, plot_theme()) +
      labs(x = 'Distance to older burns (km)', y = 'logRSS',
           title = 'RSS compared to median distance from pre-1992 burns')
  ),
  tar_target(
    fire_rss_plots,
    save_rss_plot(plot_rss_forest_fire, "rss_forest_fire-model",
                  plot_rss_old_burn, "rss_dist_to_old_burn",
                  plot_rss_new_burn, "rss_dist_to_new_burn")
  )
)

# Targets: RSS from seasonal fire model -----------------------------------------------------------
targets_rss_fire_seasonal <- c(
  
  tar_target(
    pred_h1_forest_s_fire,
    predict_h1_forest_seasonal(season_prep, s_fire_model, "fire", season_key),
    pattern = map(s_fire_model, season_prep, season_key)
  ),
  
  tar_target(
    pred_h1_s_new_burn,
    predict_h1_new_burn_seasonal(season_prep, s_fire_model, season_key),
    pattern = map(season_prep, s_fire_model, season_key)
  ),
  tar_target(
    pred_h1_s_old_burn,
    predict_h1_old_burn_seasonal(season_prep, s_fire_model, season_key),
    pattern = map(season_prep, s_fire_model, season_key)
  ),
  tar_target(
    pred_h2_s_fire_forest,
    predict_h2_seasonal_forest(season_prep, s_fire_model, "fire", season_key),
    pattern = map(season_prep, s_fire_model, season_key)
  ),
  tar_target(
    pred_h2_s_fire,
    predict_h2_seasonal(season_prep, s_fire_model, "fire", season_key),
    pattern = map(season_prep, s_fire_model, season_key)
  ),
  
  tar_target(
    rss_forest_s_fire,
    calc_rss_seasonal(pred_h1_forest_s_fire, 'h1_forest_s_fire', pred_h2_s_fire_forest, 'h2_s_fire', season_key),
    map(pred_h1_forest_s_fire, season_key)
  ),
  
  tar_target(
    rss_s_new_burn,
    calc_rss_seasonal(pred_h1_s_new_burn, 'h1_new_burn_s', pred_h2_s_fire, 'h2_s_fire', season_key),
    pattern = map(pred_h1_s_new_burn, season_key)
  ),
  tar_target(
    rss_s_old_burn,
    calc_rss_seasonal(pred_h1_s_old_burn, 'h1_old_burn_s', pred_h2_s_fire, 'h2_s_fire', season_key),
    pattern = map(pred_h1_s_old_burn, season_key)
  ),
  
  
  tar_target(
    plot_rss_forest_s_fire,
    plot_rss_seasonal(rss_forest_s_fire, plot_theme(), "fire model")
  ),
  
  tar_target(
    plot_rss_s_new_burn,
    plot_rss_seasonal(rss_s_new_burn, plot_theme(), "younger burns")
  ),
  tar_target(
    plot_rss_s_old_burn,
    plot_rss_seasonal(rss_s_old_burn, plot_theme(), "older burns")
  )
)

# Targets: incorporating sociality into fire model ----------------------------------------------------------

# dyad formation is pretty rare, more common in winter ~16% but very rare in calving and spring migration
# indivs vary a bit too but only in winter is it above 10% for every animal, let's just focus on winter for the social model for now

targets_social_fire <- c(
  tar_target(
    social_fire_model,
    model_fire_social(model_prep)
  ),
  tar_target(
    social_fire_model_check,
    model_check(social_fire_model)
  )
)

# Targets: Social fire model output ----------------------
targets_social_fire_effects <- c(
  tar_target(
    indiv_social_fire,
    indiv_estimates(social_fire_model, "fire")
  ),
  tar_target(
    social_fire_boxplot,
    plot_box_horiz(indiv_social_fire, plot_theme(), "social fire")
  ),
  tar_target(
    social_fire_sum,
    sum_plot(indiv_social_fire, plot_theme, "social fire")
  )
)

# Targets: RSS from social fire model -----------------------------------------------------------
targets_rss_fire_social <- c(
  tar_target(
    fire_pred_h1_forest_dyad,
    predict_h1_forest_social(model_prep,
                             social_fire_model, "fire", "dyad")
  ),
  tar_target(
    fire_pred_h1_forest_alone,
    predict_h1_forest_social(model_prep,
                             social_fire_model, "fire", "alone")
  ),
  
  tar_target(
    pred_h1_new_burn_alone,
    predict_h1_new_burn_social(subset(model_prep, season == "winter"),
                               social_fire_model, "alone")
  ),
  tar_target(
    pred_h1_new_burn_dyad,
    predict_h1_new_burn_social(subset(model_prep, season == "winter"),
                               social_fire_model, "dyad")
  ),
  
  tar_target(
    pred_h1_old_burn_alone,
    predict_h1_old_burn_social(subset(model_prep, season == "winter"),
                               social_fire_model, "alone")
  ),
  tar_target(
    pred_h1_old_burn_dyad,
    predict_h1_old_burn_social(subset(model_prep, season == "winter"),
                               social_fire_model, "dyad")
  ),
  
  
  tar_target(
    fire_pred_h2_dyad,
    predict_h2(subset(model_prep, season == "winter"),
               social_fire_model, "fire dyad")
  ),
  tar_target(
    fire_pred_h2_alone,
    predict_h2(subset(model_prep, season == "winter"),
               social_fire_model, "fire alone")
  ),
  tar_target(
    fire_pred_h2_dyad_forest,
    predict_h2_forest(subset(model_prep, season == "winter"),
                      social_fire_model, "fire dyad")
  ),
  tar_target(
    fire_pred_h2_alone_forest,
    predict_h2_forest(subset(model_prep, season == "winter"),
                      social_fire_model, "fire alone")
  ),
  
  tar_target(
    fire_rss_forest_dyad,
    calc_rss(fire_pred_h1_forest_dyad, 'h1_forest', fire_pred_h2_dyad_forest, 'h2')
  ),
  tar_target(
    fire_rss_forest_alone,
    calc_rss(fire_pred_h1_forest_alone, 'h1_forest', fire_pred_h2_alone_forest, 'h2')
  ),
  tar_target(
    fire_rss_forest_social,
    join_rss(fire_rss_forest_alone, fire_rss_forest_dyad)
  ),
  
  tar_target(
    rss_new_burn_dyad,
    calc_rss(pred_h1_new_burn_dyad, 'h1_new_burn', fire_pred_h2_dyad, 'h2')
  ),
  tar_target(
    rss_new_burn_alone,
    calc_rss(pred_h1_new_burn_alone, 'h1_new_burn', fire_pred_h2_alone, 'h2')
  ),
  tar_target(
    rss_new_burn_social,
    join_rss(rss_new_burn_alone, rss_new_burn_dyad)
  ),
  
  tar_target(
    rss_old_burn_dyad,
    calc_rss(pred_h1_old_burn_dyad, 'h1_old_burn', fire_pred_h2_dyad, 'h2')
  ),
  tar_target(
    rss_old_burn_alone,
    calc_rss(pred_h1_old_burn_alone, 'h1_old_burn', fire_pred_h2_alone, 'h2')
  ),
  tar_target(
    rss_old_burn_social,
    join_rss(rss_old_burn_alone, rss_old_burn_dyad)
  ),
  
  tar_target(
    fire_plot_rss_forest_social,
    plot_rss_social(fire_rss_forest_social, plot_theme()) +
      labs(x = 'Proportion forest', y = 'logRSS',
           title = 'Social RSS compared to 0 forest (fire model)')
  ),
  tar_target(
    plot_rss_new_burn_social,
    plot_rss_social(rss_new_burn_social, plot_theme()) +
      labs(x = 'Distance to younger burns (km)', y = 'logRSS',
           title = 'Social RSS for younger burns compared to median distance')
  ),
  tar_target(
    plot_rss_old_burn_social,
    plot_rss_social(rss_old_burn_social, plot_theme()) +
      labs(x = 'Distance to older burns (km)', y = 'logRSS',
           title = 'Social RSS for older burns compared to median distance')
  ),
  
  tar_target(
    social_fire_rss_plots,
    save_social_rss_plot(fire_plot_rss_forest_social, "rss_forest_social_fire",
                         plot_rss_new_burn_social, "rss_new_burn_social",
                         plot_rss_old_burn_social, "rss_old_burn_social")
  )
  
)
