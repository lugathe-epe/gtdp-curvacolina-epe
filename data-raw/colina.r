# CURVA COLINA -------------------------------------------------------------------------------------

devtools::load_all()

colinadummy <- learqcolina("inst/extdata/colina.xlsx")

usethis::use_data(colinadummy, overwrite = TRUE)
