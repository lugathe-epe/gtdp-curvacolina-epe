test_that("Interpolacao Bilinear", {
    tri <- interpolador(colinadummy, "triangulacao")
    gradecolina <- predict(tri, coordgrade(colinadummy, 100, 100), as.gradecolina = TRUE)

    # METODO DE DATA.FRAME

    pontos1 <- expand.grid(hl = 55:60, pot = 300:305)

    rr1 <- predict(gradecolina, pontos1)
    expect_equal(class(rr1), "numeric")
    expect_equal(length(rr1), nrow(pontos1))

    rr2 <- predict(gradecolina, pontos1, full.output = TRUE)
    expect_equal(class(rr2), c("data.table", "data.frame"))
    expect_equal(dim(rr2), dim(pontos1) + c(0, 2))
    expect_equal(sort(rr2$hl), sort(pontos1$hl))
    expect_equal(sort(rr2$pot), sort(pontos1$pot))
    expect_equal(rr2$rend, rr1)

    # METODO FITTED

    rr3 <- fitted(gradecolina)
    expect_equal(class(rr3), "numeric")
    expect_equal(length(rr3), nrow(colinadummy$CC))

    rr4 <- fitted(gradecolina, full.output = TRUE)
    expect_equal(class(rr4), c("data.table", "data.frame"))
    expect_equal(dim(rr4), c(nrow(colinadummy$CC), 4))
    expect_equal(sort(rr4$hl), sort(colinadummy$CC$hl))
    expect_equal(sort(rr4$pot), sort(colinadummy$CC$pot))
    expect_equal(rr4$rend, rr3)

    # METODO RESIDUALS

    rr5 <- residuals(gradecolina)
    expect_equal(class(rr5), "numeric")
    expect_equal(length(rr5), nrow(colinadummy$CC))

    # TESTE COM INTERPOLADOR POR VAZAO -----------------------------------------

    colina2 <- learqprocit(system.file("extdata/procit_cc_alterada.xlsx", package = "colinapython"))
    tri <- interpolador(colina2[[1]], "triangulacao", modo = "vaz")
    gradecolina <- predict(tri, coordgrade(colina2[[1]], 100, dvaz = 100), as.gradecolina = TRUE)

    # METODO DE DATA.FRAME

    pontos1 <- expand.grid(hl = 55:60, vaz = 300:305)

    rr1 <- predict(gradecolina, pontos1)
    expect_equal(class(rr1), "numeric")
    expect_equal(length(rr1), nrow(pontos1))

    rr2 <- predict(gradecolina, pontos1, full.output = TRUE)
    expect_equal(class(rr2), c("data.table", "data.frame"))
    expect_equal(dim(rr2), dim(pontos1) + c(0, 2))
    expect_equal(sort(rr2$hl), sort(pontos1$hl))
    expect_equal(sort(rr2$vaz), sort(pontos1$vaz))
    expect_equal(rr2$rend, rr1)

    # METODO FITTED

    rr3 <- fitted(gradecolina)
    expect_equal(class(rr3), "numeric")
    expect_equal(length(rr3), nrow(colina2[[1]]$CC))

    rr4 <- fitted(gradecolina, full.output = TRUE)
    expect_equal(class(rr4), c("data.table", "data.frame"))
    expect_equal(dim(rr4), c(nrow(colina2[[1]]$CC), 4))
    expect_equal(sort(rr4$hl), sort(colina2[[1]]$CC$hl))
    expect_equal(sort(rr4$vaz), sort(colina2[[1]]$CC$vaz))
    expect_equal(rr4$rend, rr3)

    # METODO RESIDUALS

    rr5 <- residuals(gradecolina)
    expect_equal(class(rr5), "numeric")
    expect_equal(length(rr5), nrow(colina2[[1]]$CC))
})
