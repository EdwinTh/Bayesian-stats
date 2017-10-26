#' Create a Marimekko/Mosaic plot with ggplot2
#' 
#' Take two categorical variable and create a Marimekko/Mosaic plot from it.
#' @param df A data frame containing `x` and `y`.
#' @param x The bare name of the variable for the x-axis.
#' @param y The bare name of the variabel for the y-axis and the fill.
#' @param alpha_condition Would highlight the condition (lower alpha for not
#' meeting the condition).
#' @return An object of class `ggplot2`.
#' @examples 
#' ggmm(mtcars, cyl, vs)

ggmm <- function(df, x, y, alpha_condition = 1 == 1) {
  library(tidyverse)
  x_q <- enquo(x)
  y_q <- enquo(y)
  a_q <- enquo(alpha_condition)
  
  plot_obj <- df %>%
    mutate(alpha_ind = !!a_q) %>% 
    mutate(x_cat = as.character(!!x_q),
           y_cat = as.character(!!y_q)) %>% 
    group_by(x_cat, y_cat) %>%
    summarise(comb_cnt  = n(),
              alpha_ind = as.numeric(sum(alpha_ind) > 0)) %>%
    mutate(freq  = comb_cnt /sum(comb_cnt),
           y_cnt = sum(comb_cnt)) %>%
    ungroup() %>% 
    ggplot(aes(x_cat, freq, width = y_cnt, fill = y_cat, alpha = alpha_ind)) +
    geom_bar(stat = "identity", position = "fill", color = "black") +
    facet_grid(~x_cat, scales = "free_x", space = "free_x") +
    theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(0, "lines")
    ) +
    guides(alpha = FALSE) +
    labs(fill = quo_name(y_q)) +
    xlab(quo_name(x_q)) 
  
  if (mutate(df, !!a_q) %>% pull() %>% 
      unique() %>% length() %>% magrittr::equals(1)) {
    plot_obj
  } else {
    plot_obj +
      scale_alpha_continuous(range = c(.4, 1))
  }
}




