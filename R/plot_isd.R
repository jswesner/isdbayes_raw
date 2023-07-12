
nsamples = 1000

isd_plot_data = isd_data %>%
  group_by(group) %>%
  # sample_n(nsamples, weight = counts, replace = T) %>%
  arrange(group, desc(x)) %>%
  mutate(rownumber = row_number(),
         y_order = 1:max(rownumber),
         y_order = seq(0, 1, length.out = max(rownumber)))

dat_split = posts_group %>%
  # group_by(group) %>%
  # tidybayes::median_qi(.epred) %>%
  # filter(sample_int %in% c(id)) %>%
  group_by(group, .draw, xmin, xmax) %>%
  group_split

xy.PLB = NULL
for(i in 1:length(dat_split)) {
  group = unique(dat_split[[i]]$group)
  draw = unique(dat_split[[i]]$.draw)
  # site_id = unique(dat_split[[i]]$site_id)
  # year = unique(dat_split[[i]]$year)
  xmin = unique(dat_split[[i]]$xmin)
  xmax = unique(dat_split[[i]]$xmax)

  lambda = unique(dat_split[[i]]$.epred)

  x.PLB = seq(xmin, xmax,
              length=300) # x values to plot PLB

  y.PLB = (1 - (x.PLB^(lambda + 1) - (xmin^(lambda+1)))/(xmax^(lambda + 1) - (xmin^(lambda+1))))
  # ymin.PLB = (1 - (x.PLB^(.lower + 1) - (xmin^(.lower+1)))/(xmax^(.lower + 1) - (xmin^(.lower+1))))
  # ymax.PLB = (1 - (x.PLB^(.upper + 1) - (xmin^(.upper+1)))/(xmax^(.upper + 1) - (xmin^(.upper+1))))

  xy.PLB[[i]] = tibble(x = x.PLB,
                       y_order = y.PLB,
                       # ymin = ymin.PLB,
                       # ymax = ymax.PLB,
                       xmax = xmax,
                       xmin = xmin) %>%
    mutate(group = group,
           .draw = draw,
           lambda = lambda)
}



library(tidybayes)
library(ggthemes)

post_lines = bind_rows(xy.PLB)
post_lines_summary = post_lines %>%
  group_by(x, group) %>%
  median_qi(y_order)

isd_plot_data %>%
  ggplot(aes(x = x, y = y_order)) +
  geom_point(shape = 21, aes(color = group, size = x)) +
  # scale_y_log10() +
  scale_x_log10() +
  geom_line(data = post_lines_summary,
            aes(y = y_order, group = group)) +
  geom_ribbon(data = post_lines_summary,
              aes(ymin = .lower, ymax = .upper, fill = group), alpha = 0.5) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  theme_default()

posts_group %>%
  group_by(group) %>%
  tidybayes::median_qi(.epred)


