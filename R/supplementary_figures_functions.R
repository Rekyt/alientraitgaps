# Functions to save files of supplementary figures
# Saving Figure S1 -------------------------------------------------------------
save_supp_fig1_pdf = function(given_diagram, given_file) {

  pdf(file = given_file, width = 7, height = 7)

  eulerr:::plot.eulergram(given_diagram)

  dev.off()

  return(given_file)

}

save_supp_fig1_png = function(given_diagram, given_file) {

  png(file = given_file, width = 1000, height = 1000, res = 200)

  eulerr:::plot.eulergram(given_diagram)

  dev.off()

  return(given_file)

}


# Saving Figure S2 -------------------------------------------------------------

save_supp_fig2_png = function(given_diagram, given_file) {

  png(file = given_file, width = 1200, height = 1200, res = 150)

  eulerr:::plot.eulergram(given_diagram)

  dev.off()

  return(given_file)

}


# Saving Figure S3 -------------------------------------------------------------

custom_ggsave = function(given_target, given_file, ...) {

  ggsave(
    given_file,
    given_target,
    height = 7, width = 7, units = "in", ...
  )

}
