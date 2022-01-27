test_that("Leitura de colinas pura", {
    arq <- system.file("extdata/colina.xlsx", package = "curvacolina")
    colina <- learqcolina(arq)

    expect_equal(class(colina), "curvacolina")
    expect_equal(colnames(colina$CC), c("hl", "pot", "vaz", "rend"))

    # a ideia desse teste e checar se a leitura da colina continua correta. Originalmente era 
    # comparado um snapshot de colina$CC inteira, mas isso fica pesado demais no diretorio
    summ <- sapply(colina$CC, summary)
    expect_snapshot_value(summ, style = "json2")
    expect_true(all(is.na(colina$CC$vaz)))

    expect_equal(attr(colina, "ncurvas"), 21)
    expect_equal(attr(colina, "max"), 95.78)
    expect_true(is.na(attr(colina, "rho")))
    expect_true(is.na(attr(colina, "g")))

    expect_snapshot_value(attr(colina, "rends"), style = "json2")
})

test_that("Leitura de colinas fornecendo rho e g", {
    arq <- system.file("extdata/colina.xlsx", package = "curvacolina")
    colina <- learqcolina(arq, rho = 1000, g = 9.81)

    expect_equal(class(colina), "curvacolina")
    expect_equal(colnames(colina$CC), c("hl", "pot", "vaz", "rend"))

    # a ideia desse teste e checar se a leitura da colina continua correta. Originalmente era 
    # comparado um snapshot de colina$CC inteira, mas isso fica pesado demais no diretorio
    summ <- sapply(colina$CC, summary)
    expect_snapshot_value(summ, style = "json2")
    expect_true(all(!is.na(colina$CC$vaz)))

    expect_equal(attr(colina, "ncurvas"), 21)
    expect_equal(attr(colina, "max"), 95.78)
    expect_equal(attr(colina, "rho"), 1000)
    expect_equal(attr(colina, "g"), 9.81)

    expect_snapshot_value(attr(colina, "rends"), style = "json2")
})

test_that("Colina dummy", {
    colina <- colinadummy

    expect_equal(class(colina), "curvacolina")
    expect_equal(colnames(colina$CC), c("hl", "pot", "vaz", "rend"))

    # a ideia desse teste e checar se a leitura da colina continua correta. Originalmente era 
    # comparado um snapshot de colina$CC inteira, mas isso fica pesado demais no diretorio
    summ <- sapply(colina$CC, summary)
    expect_snapshot_value(summ, style = "json2")
    expect_true(all(is.na(colina$CC$vaz)))

    expect_equal(attr(colina, "ncurvas"), 21)
    expect_equal(attr(colina, "max"), 95.78)
    expect_true(is.na(attr(colina, "rho")))
    expect_true(is.na(attr(colina, "g")))

    expect_snapshot_value(attr(colina, "rends"), style = "json2")
})

test_that("Leitura de processo iterativo (CC Original)", {
    arq <- system.file("extdata/procit_cc_original.xlsx", package = "curvacolina")
    colina <- learqprocit(arq)

    expect_true(is.list(colina))
    expect_equal(length(colina), 2)
    expect_equal(names(colina), paste0("colina_", 1:2))

    expect_equal(unname(sapply(colina, class)), rep("curvacolina", 2))

    v_ncurvas <- c(10, 14)
    v_max <- c(NA, NA)
    v_rho <- rep(997.3128, 2)
    v_g   <- rep(9.78473833060729, 2)

    for(i in seq(2)) {
        expect_snapshot_value(attr(colina[[i]], "rends"), style = "json2")

        expect_equal(attr(colina[[i]], "ncurvas"), v_ncurvas[i])
        expect_equal(attr(colina[[i]], "max"), v_max[i])
        expect_equal(attr(colina[[i]], "rho"), v_rho[i])
        expect_equal(attr(colina[[i]], "g"), v_g[i])

        expect_true(!is.na(attr(colina[[i]], "rho")))
        expect_true(!is.na(attr(colina[[i]], "g")))

        summ <- sapply(colina[[i]]$CC, summary)
        expect_snapshot_value(summ, style = "json2")
    }
})

test_that("Leitura de processo iterativo (CC Alterada)", {
    arq <- system.file("extdata/procit_cc_alterada.xlsx", package = "curvacolina")
    colina <- learqprocit(arq)

    expect_true(is.list(colina))
    expect_equal(length(colina), 2)
    expect_equal(names(colina), paste0("colina_", 1:2))

    expect_equal(unname(sapply(colina, class)), rep("curvacolina", 2))

    v_ncurvas <- c(21, 22)
    v_max <- list(95.78, NA)
    v_rho <- rep(997.026, 2)
    v_g   <- rep(9.7877, 2)

    for(i in seq(2)) {
        expect_snapshot_value(attr(colina[[i]], "rends"), style = "json2")

        expect_equal(attr(colina[[i]], "ncurvas"), v_ncurvas[i])
        expect_equal(attr(colina[[i]], "max"), v_max[[i]])
        expect_equal(attr(colina[[i]], "rho"), v_rho[i])
        expect_equal(attr(colina[[i]], "g"), v_g[i])

        expect_true(!is.na(attr(colina[[i]], "rho")))
        expect_true(!is.na(attr(colina[[i]], "g")))

        summ <- sapply(colina[[i]]$CC, summary)
        expect_snapshot_value(summ, style = "json2")
    }
})

test_that("as.curvacolina", {
    cc <- colinadummy$CC

    cc.1 <- copy(cc)
    cc.1[, rend := rend / 100]

    cc.2 <- copy(cc)
    cc.2[, rend := paste0(rend, "%")]

    cc.4 <- colinadummy$CC[, list(hl, pot, rend, vaz)]
    colnames(cc.4) <- paste0("V", 1:4)

    cc.5 <- cc.4[, 1:2]

    cc.6 <- copy(cc)
    cc.6[, rend := rep("a", .N)]

    # SEM FORCE // SEM g E rho

    xx1 <- as.curvacolina(cc)

    expect_error(as.curvacolina(cc.1)) # rendimentos decimais
    expect_error(as.curvacolina(cc.2)) # coluna nao numerica
    expect_error(as.curvacolina(cc.2)) # faltando nomes de colunas

    expect_equal(class(xx1), "curvacolina")
    expect_equal(colnames(xx1$CC), c("hl", "pot", "vaz", "rend"))
    expect_true(all(xx1$CC$hl == cc$CC$hl))
    expect_true(all(xx1$CC$pot == cc$CC$pot))
    expect_true(all(xx1$CC$rend == cc$CC$rend))
    expect_true(all(is.na(xx1$CC$vaz) & is.na(cc$CC$vaz)))

    expect_true(is.na(attr(xx1, "g")))
    expect_true(is.na(attr(xx1, "rho")))

    # SEM FORCE // COM g E rho

    xx2 <- as.curvacolina(cc, 9.81, 1000)

    expect_equal(class(xx2), "curvacolina")
    expect_equal(colnames(xx2$CC), c("hl", "pot", "vaz", "rend"))
    expect_true(all(xx2$CC$hl == cc$CC$hl))
    expect_true(all(xx2$CC$pot == cc$CC$pot))
    expect_true(all(xx2$CC$rend == cc$CC$rend))
    expect_true(all(!is.na(xx2$CC$vaz)))

    expect_equal(attr(xx2, "g"), 9.81)
    expect_equal(attr(xx2, "rho"), 1000)

    # COM FORCE // SEM g E rho

    expect_warning(xx3 <- as.curvacolina(cc.1, force = TRUE)) # rendimentos decimais
    expect_warning(xx3 <- as.curvacolina(cc.2, force = TRUE)) # coluna nao numerica
    expect_warning(xx3 <- as.curvacolina(cc.4, force = TRUE)) # colunas sem nome certo (ncol > 3)

    expect_error(xx3 <- as.curvacolina(cc.5, force = TRUE)) # colunas sem nome certo (ncol < 3)
    expect_error(xx3 <- as.curvacolina(cc.6, force = TRUE)) # rend nao pode ser convertido p num

    expect_equal(class(xx3), "curvacolina")
    expect_equal(colnames(xx3$CC), c("hl", "pot", "vaz", "rend"))
    expect_true(all(xx3$CC$hl == cc$CC$hl))
    expect_true(all(xx3$CC$pot == cc$CC$pot))
    expect_true(all(xx3$CC$rend == cc$CC$rend))
    expect_true(all(is.na(xx3$CC$vaz) & is.na(cc$CC$vaz)))

    expect_true(is.na(attr(xx3, "g")))
    expect_true(is.na(attr(xx3, "rho")))

    # COM FORCE // COM g E rho

    expect_warning(xx4 <- as.curvacolina(cc.4, 9.81, 1000, force = TRUE))

    expect_equal(class(xx4), "curvacolina")
    expect_equal(colnames(xx4$CC), c("hl", "pot", "vaz", "rend"))
    expect_true(all(xx4$CC$hl == cc$CC$hl))
    expect_true(all(xx4$CC$pot == cc$CC$pot))
    expect_true(all(xx4$CC$rend == cc$CC$rend))
    expect_true(all(!is.na(xx4$CC$vaz)))

    expect_equal(attr(xx4, "g"), 9.81)
    expect_equal(attr(xx4, "rho"), 1000)
})

test_that("set_grho", {
    cc <- set_grho(colinadummy, 9.81, 1000)

    expect_equal(class(cc), "curvacolina")
    expect_equal(colnames(cc$CC), c("hl", "pot", "vaz", "rend"))

    expect_true(all(!is.na(cc$CC$vaz)))

    expect_equal(attr(cc, "g"), 9.81)
    expect_equal(attr(cc, "rho"), 1000)
})