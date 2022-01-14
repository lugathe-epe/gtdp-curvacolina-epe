test_that("Modelagem por Triangulacao", {
    interp <- interpolador(colinadummy, "triangulacao")

    expect_equal(class(interp), c("triangulacao", "interpolador"))
    expect_equal(length(interp), 2)
    expect_equal(names(interp), c("triangulos", "colina"))

    expect_equal(getcolina.triangulacao(interp), colinadummy)

    gg <- geragrade(colinadummy, 20, 20)

    pred_vec <- predict(interp, gg)
    expect_snapshot_value(pred_vec, style = "json2")

    pred_full <- predict(interp, gg, TRUE)
    expect_equal(class(pred_full), c("data.table", "data.frame"))
    expect_equal(colnames(pred_full), c("hl", "pot", "rend", "inhull"))
    expect_equal(pred_full$rend, pred_vec)

    p3d <- plot(interp, hl = 10, pot = 10, print = FALSE)
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    p2d <- plot(interp, hl = 10, pot = 10, tipo = "2d", print = FALSE)
    expect_equal(class(p2d), c("gg", "ggplot"))
})

test_that("Modelagem por Tensor Product", {
    interp <- interpolador(colinadummy, "tensorprod")

    expect_equal(class(interp), c("tensorprod", "interpolador"))
    expect_equal(length(interp), 2)
    expect_equal(names(interp), c("superficie", "colina"))

    expect_equal(getcolina.tensorprod(interp), colinadummy)

    gg <- geragrade(colinadummy, 20, 20)

    pred_vec <- predict(interp, gg)
    expect_snapshot_value(pred_vec, style = "json2")

    pred_full <- predict(interp, gg, TRUE)
    expect_equal(class(pred_full), c("data.table", "data.frame"))
    expect_equal(colnames(pred_full), c("hl", "pot", "rend", "inhull"))
    expect_equal(pred_full$rend, pred_vec)

    p3d <- plot(interp, hl = 10, pot = 10, print = FALSE)
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    p2d <- plot(interp, hl = 10, pot = 10, tipo = "2d", print = FALSE)
    expect_equal(class(p2d), c("gg", "ggplot"))
})

test_that("Modelagem por Thin Plate", {
    interp <- interpolador(colinadummy, "thinplate", taxa_reducao = 2)

    expect_equal(class(interp), c("thinplate", "interpolador"))
    expect_equal(length(interp), 2)
    expect_equal(names(interp), c("superficie", "colina"))

    expect_equal(getcolina.thinplate(interp), colinadummy)

    gg <- geragrade(colinadummy, 20, 20)

    pred_vec <- predict(interp, gg)
    expect_snapshot_value(pred_vec, style = "json2")

    pred_full <- predict(interp, gg, TRUE)
    expect_equal(class(pred_full), c("data.table", "data.frame"))
    expect_equal(colnames(pred_full), c("hl", "pot", "rend", "inhull"))
    expect_equal(pred_full$rend, pred_vec)

    p3d <- plot(interp, hl = 10, pot = 10, print = FALSE)
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    p2d <- plot(interp, hl = 10, pot = 10, tipo = "2d", print = FALSE)
    expect_equal(class(p2d), c("gg", "ggplot"))
})