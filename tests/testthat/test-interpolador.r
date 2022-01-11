test_that("Modelagem por Triangulacao", {
    interp <- interpolador(colinadummy, "triangulacao")

    expect_equal(class(interp), c("triangulacao", "interpolador"))
    expect_equal(length(interp), 2)
    expect_equal(names(interp), c("triangulos", "colina"))

    expect_equal(getcolina.triangulacao(interp), colinadummy)

    expect_snapshot_value(predict(interp, geragrade(colinadummy, 20, 20)), style = "json2")
})

test_that("Modelagem por Tensor Product", {
    interp <- interpolador(colinadummy, "tensorprod")

    expect_equal(class(interp), c("tensorprod", "interpolador"))
    expect_equal(length(interp), 2)
    expect_equal(names(interp), c("superficie", "colina"))

    expect_equal(getcolina.tensorprod(interp), colinadummy)

    expect_snapshot_value(predict(interp, geragrade(colinadummy, 20, 20)), style = "json2")
})