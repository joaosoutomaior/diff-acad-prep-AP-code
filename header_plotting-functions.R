#==============================================================
#                  File description
#==============================================================
# contents: 
#  functions to create plots in the paper
# code author: Joao Souto-Maior
# last updated: Dec 5, 2023  

c1 = "#E7298A"
c2 = "#7570B3" 
c3 = "#66A61E" 
c4 = "#E6AB02"

plot_roc_auc <- function(ROCR_obj, AUC, title, color){
    ROCR::plot(ROCR_obj, 
       colorize = F,
       lwd = 1, 
       col = color,
       print.cutoffs.at = seq(0,1,by=0.1), 
       text.adj=c(-0.2,1.7, col = 1, lwd = 2),
       text.col = "black",
       points.col = color, 
       points.pch = 15,
       xlab = " ", 
       ylab = " ",
       xlim=c(0, 1),
       ylim=c(0, 1),
       main = title) 
  abline(a=0, b= 1, lty=2, lwd = 1.5, col="black")
  text(0.7, 0.3, col = "black", paste0("AUC = ", round(AUC, 3)))
  box(col = 'black')
}

plot_pr_auc <- function(ROCR_obj, AUC, title, color){
  ROCR::plot(ROCR_obj, 
             colorize = F,
             lwd = 1, 
             col = color,
             print.cutoffs.at = seq(0,1,by=0.1), 
             text.adj=c(-0.2,1.7, col = 1, lwd = 2),
             text.col = "black",
             points.col = color, 
             points.pch = 15,
             xlab = " ", 
             ylab = " ",
             ylim=c(0, 1),
             main = title) 
  abline(a=0.5, b= 0, lty=2, lwd = 1.5, col="black")
  text(0.7, 0.3, col = "black", paste0("AUC = ", round(AUC, 3)))
  box(col = 'black')
}

plot_benchmark_test <- function(dat, xvar, yvar, lim, size){ 
  ggplot(dat) + 
    geom_point(aes(x = xvar, 
                   y = yvar), 
               alpha = 0.5,
               colour = "black",
               stroke = 1,
               shape = 1,
               size = size) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', colour = "black", lwd = 0.8) +
    scale_y_continuous('Pct. enrolled in at least one AP math course\n(Black students)', limits=c(0,lim), 
                       labels = function(x) paste0(x, "%"),
                       expand = c(0.1,0.1)) +
    scale_x_continuous('Pct. enrolled in at least one AP math course \n(White students)', limits=c(0,lim), 
                       labels = function(x) paste0(x, "%"),
                       expand = c(0.1,0.1)) +
    ggtheme
}

plot_outcome_test <- function(dat, xvar, yvar, lim, size){ 
  ggplot(dat) + 
    geom_point(aes(x = xvar, 
                   y = yvar), 
               alpha = 0.5,
               colour = "black",
               stroke = 1,
               shape = 1,
               size = size) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', colour = "black", lwd = 0.8) +
    scale_y_continuous('Pct. passed at least one AP math exam \n(Black students)', limits=c(0,lim), 
                       labels = function(x) paste0(x, "%"),
                       expand = c(0.05,0.05)) +
    scale_x_continuous('Pct. passed at least one AP math exam \n(White students)', limits=c(0,lim), 
                       labels = function(x) paste0(x, "%"),
                       expand = c(0.05,0.05)) +
    ggtheme
}

plot_enroll_success_by_context <- function(data, var){
  p <- data %>%
    ggplot() +
    geom_smooth(data = dat_white,
                aes(prob_passage_pct, AP_math_enroll, color = "White"),
                method = "loess", 
                formula = y ~ x, 
                se = F,
                linetype = "solid") +
    geom_smooth(data = dat_black,
                aes(prob_passage_pct, AP_math_enroll, color = "Black"),
                method = "loess", 
                formula = y ~ x, 
                se = F,
                linetype = "solid") +
    geom_smooth(data = dat_hispanic,
                aes(prob_passage_pct, AP_math_enroll, color = "Hispanic"),
                method = "loess", 
                formula = y ~ x, 
                se = F,
                linetype = "solid") +
    geom_smooth(data = dat_asian,
                aes(prob_passage_pct, AP_math_enroll, color = "Asian"),
                method = "loess", 
                formula = y ~ x, 
                se = F,
                linetype = "solid") +
    scale_y_continuous("Probability of AP math enrollment", limits=c(0,1), 
                       labels = function(x) paste0(100 * x, "%"),
                       expand = c(0,0)) +
    scale_x_continuous('Ex ante probability of AP math success', limits=c(0.1,99.9), 
                       labels = function(x) paste0(x, "%"),
                       expand = c(0,0)) +
    scale_color_manual(values = c("White" = c1, "Black" = c3, "Hispanic" = c4, "Asian" = c2)) +
    ggtheme_facet + 
    theme(legend.position="top",
          legend.direction = "horizontal",
          legend.title = element_blank()) +
    facet_wrap(facets = var, ncol = 2)
  p
}
