test_that("Leitura de colinas", {
    arq <- system.file("extdata/colina.xlsx", package = "curvacolina")
    colina <- learqcolina(arq)

    expect_equal(class(colina), "curvacolina")
    expect_snapshot_value(attr(colina, "ncurvas"))
    expect_snapshot_value(attr(colina, "rends"), style = "json2")
    expect_snapshot_value(data.matrix(colina$CC), style = "json2")
})

test_that("Leitura de processo iterativo (CC Original)", {
    arq <- system.file("extdata/procit_cc_original.xlsx", package = "curvacolina")
    colina <- learqprocit(arq)

    expect_true(is.list(colina))
    expect_equal(length(colina), 2)
    expect_equal(names(colina), paste0("colina_", 1:2))
    for(i in seq(2)) {
        expect_snapshot_value(attr(colina[[i]], "ncurvas"))
        expect_snapshot_value(attr(colina[[i]], "rends"), style = "json2")
        expect_snapshot_value(data.matrix(colina[[i]]$CC), style = "json2")
    }
})

test_that("Leitura de processo iterativo (CC Alterada)", {
    arq <- system.file("extdata/procit_cc_alterada.xlsx", package = "curvacolina")
    colina <- learqprocit(arq)

    expect_true(is.list(colina))
    expect_equal(length(colina), 2)
    expect_equal(names(colina), paste0("colina_", 1:2))
    for(i in seq(2)) {
        expect_snapshot_value(attr(colina[[i]], "ncurvas"))
        expect_snapshot_value(attr(colina[[i]], "rends"), style = "json2")
        expect_snapshot_value(data.matrix(colina[[i]]$CC), style = "json2")
    }
})