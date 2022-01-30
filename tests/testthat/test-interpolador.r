test_that("Modelagem por Triangulacao", {
    interp <- interpolador(colinadummy, "triangulacao")

    expect_equal(class(interp), c("triangulacao", "interpolador"))
    expect_equal(length(interp), 2)
    expect_equal(names(interp), c("triangulos", "colina"))

    expect_equal(getcolina.triangulacao(interp), colinadummy)

    gg <- coordgrade(colinadummy, 20, 20)

    pred_vec <- predict(interp, gg)
    expect_snapshot_value(pred_vec, style = "json2")

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

test_that("Modelagem por Tensor Product", {
    interp <- interpolador(colinadummy, "tensorprod")

    expect_equal(class(interp), c("tensorprod", "interpolador"))
    expect_equal(length(interp), 2)
    expect_equal(names(interp), c("superficie", "colina"))

    expect_equal(getcolina.tensorprod(interp), colinadummy)

    gg <- coordgrade(colinadummy, 20, 20)

    pred_vec <- predict(interp, gg)
    expect_snapshot_value(pred_vec, style = "json2")

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

test_that("Modelagem por Thin Plate", {
    interp <- interpolador(colinadummy, "thinplate", taxa_reducao = 5)

    expect_equal(class(interp), c("thinplate", "interpolador"))
    expect_equal(length(interp), 2)
    expect_equal(names(interp), c("superficie", "colina"))

    expect_equal(getcolina.thinplate(interp), colinadummy)

    gg <- coordgrade(colinadummy, 20, 20)

    pred_vec <- predict(interp, gg)
    expect_snapshot_value(pred_vec, style = "json2")

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