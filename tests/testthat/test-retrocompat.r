test_that("Reta Minima Distancia", {
    arq <- system.file("extratestdata/retamindist/colina_Processada/preliminar/retamindistteste.csv",
        package = "curvacolina")
    expect_warning(interp <- retamindist(arq))

    arq <- system.file("extratestdata/retamindist/colina_Processada/preliminar/retamindistteste_1.csv",
        package = "curvacolina")
    interp <- retamindist(arq)

    expect_equal(class(interp), c("retamindist", "interpolador"))
    expect_equal(length(interp), 2)
    expect_equal(names(interp), c("superficie", "colina"))

    gg <- coordgrade(getcolina.retamindist(interp), 20, 20)

    pred_vec <- predict(interp, gg)
    expect_snapshot_value(summary(pred_vec), style = "json2")

    pred_full <- predict(interp, gg, TRUE)
    expect_equal(class(pred_full), "gradecolina")
    expect_equal(class(pred_full[[2]]), "curvacolina")
    expect_equal(colnames(pred_full[[1]]), c("hl", "pot", "rend", "inhull"))
    expect_equal(pred_full[[1]]$rend, pred_vec)

    p3d <- plot(interp, print = FALSE, dhl = 10, dpot = 10)
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    p2d <- plot(interp, tipo = "2d", print = FALSE, dhl = 10, dpot = 10)
    expect_equal(class(p2d), c("gg", "ggplot"))
})