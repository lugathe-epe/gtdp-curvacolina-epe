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