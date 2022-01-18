test_that("Gerador de grades", {

    # metodos entregam mesmo resultado
    grade_1 <- geragrade(colinadummy, 20, 20)
    grade_2 <- geragrade(colinadummy$CC, 20, 20)
    grade_3 <- geragrade(as.data.frame(colinadummy$CC), 20, 20)
    expect_true(all(grade_1 == grade_2))
    expect_true(all(grade_2 == grade_3))

    # grade gerada com dhl e dpot inteiros
    expect_equal(length(unique(grade_1$hl)), 20)
    expect_equal(min(grade_1$hl), min(colinadummy$CC$hl))
    expect_equal(max(grade_1$hl), max(colinadummy$CC$hl))

    expect_equal(length(unique(grade_1$pot)), 20)
    expect_equal(min(grade_1$pot), min(colinadummy$CC$pot))
    expect_equal(max(grade_1$pot), max(colinadummy$CC$pot))

    grade_2 <- geragrade(colinadummy, 30, 30, expande = c(.1, .1))

    deltahl  <- .1 * diff(range(colinadummy$CC$hl))
    deltapot <- .1 * diff(range(colinadummy$CC$pot))

    expect_equal(length(unique(grade_2$hl)), 30)
    expect_equal(min(grade_2$hl), min(colinadummy$CC$hl) - deltahl)
    expect_equal(max(grade_2$hl), max(colinadummy$CC$hl) + deltahl)

    expect_equal(length(unique(grade_2$pot)), 30)
    expect_equal(min(grade_2$pot), min(colinadummy$CC$pot) - deltapot)
    expect_equal(max(grade_2$pot), max(colinadummy$CC$pot) + deltapot)

    # grade gerada com dhl e dpot vetores

    grade_5 <- geragrade(colinadummy, 20:40, 200:300)

    expect_equal(unique(grade_5$hl), 20:40)
    expect_equal(unique(grade_5$pot), 200:300)

    grade_5 <- geragrade(colinadummy, 20:40, 200:300, expand = c(.1, .1))

    expect_equal(unique(grade_5$hl), 20:40)
    expect_equal(unique(grade_5$pot), 200:300)

    # grade gerada com byhl e bypot
    # os limites dos expect_equal aqui e no proximo teste foram pre calculados

    grade_3 <- geragrade(colinadummy, byhl = .5, bypot = 5)

    expect_equal(min(grade_3$hl), 34)
    expect_equal(max(grade_3$hl), 61.5)
    expect_equal(seq(34, 61.5, by = .5), unique(grade_3$hl))

    expect_equal(min(grade_3$pot), 115)
    expect_equal(max(grade_3$pot), 455)
    expect_equal(seq(115, 455, by = 5), unique(grade_3$pot))

    # grade gerada com byhl e bypot e expand

    grade_4 <- geragrade(colinadummy, byhl = .5, bypot = 5, expand = c(.1, .1))

    expect_equal(min(grade_4$hl), 31)
    expect_equal(max(grade_4$hl), 64)
    expect_equal(seq(31, 64, by = .5), unique(grade_4$hl))

    expect_equal(min(grade_4$pot), 85)
    expect_equal(max(grade_4$pot), 485)
    expect_equal(seq(85, 485, by = 5), unique(grade_4$pot))
})