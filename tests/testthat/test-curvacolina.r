test_that("Leitura de colinas", {
    arq <- system.file("extdata/colina.xlsx", package = "curvacolina")
    colina <- learqcolina(arq)

    expect_equal(class(colina), "curvacolina")
    expect_equal(colnames(colina$CC), c("hl", "pot", "vaz", "rend"))

    expect_true(all(colina$CC$hl - colinadummy$CC$hl == 0))
    expect_true(all(colina$CC$pot - colinadummy$CC$pot == 0))
    expect_true(all(colina$CC$rend - colinadummy$CC$rend == 0))
    expect_true(all(is.na(colina$CC$vaz) & is.na(colinadummy$CC$vaz)))

    expect_snapshot_value(attr(colina, "ncurvas"))
    expect_snapshot_value(attr(colina, "rends"), style = "json2")

    # a ideia desse teste e checar se a leitura da colina continua correta. Originalmente era 
    # comparado um snapshot de colina$CC inteira, mas isso fica pesado demais no diretorio
    summ <- sapply(colina$CC, summary)
    expect_snapshot_value(summ, style = "json2")
})

test_that("Leitura de processo iterativo (CC Original)", {
    arq <- system.file("extdata/procit_cc_original.xlsx", package = "curvacolina")
    colina <- learqprocit(arq)

    expect_true(is.list(colina))
    expect_equal(length(colina), 2)
    expect_equal(names(colina), paste0("colina_", 1:2))

    expect_equal(unname(sapply(colina, class)), rep("curvacolina", 2))

    for(i in seq(2)) {
        expect_snapshot_value(attr(colina[[i]], "ncurvas"))
        expect_snapshot_value(attr(colina[[i]], "rends"), style = "json2")

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

    for(i in seq(2)) {
        expect_snapshot_value(attr(colina[[i]], "ncurvas"))
        expect_snapshot_value(attr(colina[[i]], "rends"), style = "json2")

        summ <- sapply(colina[[i]]$CC, summary)
        expect_snapshot_value(summ, style = "json2")
    }
})