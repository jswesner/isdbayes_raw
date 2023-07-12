
post_preds = posts_group %>%
  filter(.draw <= 10) %>%
  ungroup %>%
  select(group, .draw, .epred) %>%
  right_join(isd_data, relationship = "many-to-many") %>%
  mutate(u = runif(nrow(.), 0, 1)) %>% # uniform draw
  mutate(x = (u*xmax^(.epred+1) +  (1-u) * xmin^(.epred+1) ) ^ (1/(.epred+1))) %>%
  mutate(data = "y_rep") %>%
  bind_rows(isd_data %>%
              mutate(data = "y_raw") %>%
              mutate(.draw = 0,
                     x = x))


nsamples = 1000

post_preds_sims = post_preds %>%
  group_by(.draw, group) %>%
  sample_n(nsamples, weight = counts, replace = T)

post_preds_sims %>%
  ggplot(aes(x = x, color = data, group = .draw)) +
  geom_density() +
  scale_x_log10() +
  facet_wrap(~group)
