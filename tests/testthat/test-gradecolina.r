test_that("Interpolacao Bilinear", {
    tri <- interpolador(colinadummy, "tri")
    gradecolina <- predict(tri, geragrade(colinadummy, 100, 100), full.output = TRUE)

    # METODO DE DATA.FRAME

    pontos1 <- expand.grid(hl = 55:60, pot = 300:305)

    rr1 <- interpolagrade(pontos1, gradecolina)
    expect_equal(class(rr1), "numeric")
    expect_equal(length(rr1), nrow(pontos1))

    rr2 <- interpolagrade(pontos1, gradecolina, full.output = TRUE)
    expect_equal(class(rr2), c("data.table", "data.frame"))
    expect_equal(dim(rr2), dim(pontos1) + c(0, 1))
    expect_equal(sort(rr2$hl), sort(pontos1$hl))
    expect_equal(sort(rr2$pot), sort(pontos1$pot))
    expect_equal(rr2$rend, rr1)

    # METODO CURVACOLINA

    rr3 <- interpolagrade(colinadummy, gradecolina)
    expect_equal(class(rr3), "numeric")
    expect_equal(length(rr3), nrow(colinadummy$CC))

    rr4 <- interpolagrade(colinadummy, gradecolina, full.output = TRUE)
    expect_equal(class(rr4), c("data.table", "data.frame"))
    expect_equal(dim(rr4), c(nrow(colinadummy$CC), 3))
    expect_equal(sort(rr4$hl), sort(colinadummy$CC$hl))
    expect_equal(sort(rr4$pot), sort(colinadummy$CC$pot))
    expect_equal(rr4$rend, rr3)
})