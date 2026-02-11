# Tests for helper functions
# These tests do not require the Sirius API

test_that(".flatten_list flattens nested lists correctly", {
    lst <- list(a = 1, b = list(c = 2, d = 3))
    flat <- RuSirius:::.flatten_list(lst)

    expect_type(flat, "list")
    expect_equal(flat$a, 1)
    expect_equal(flat$c, 2)
    expect_equal(flat$d, 3)
})

test_that(".flatten_list handles empty list", {
    flat <- RuSirius:::.flatten_list(list())
    expect_length(flat, 0)
})

test_that(".flatten_list handles deeply nested lists", {
    lst <- list(a = 1, b = list(c = list(d = list(e = 5))))
    flat <- RuSirius:::.flatten_list(lst)

    expect_equal(flat$a, 1)
    expect_equal(flat$e, 5)
})

test_that(".format_command removes 'config' and formats correctly", {
    cmd <- "sirius config --param1 value1 --param2 value2"
    formatted <- RuSirius:::.format_command(cmd)

    expect_false(grepl("config", formatted))
    expect_true(grepl("sirius", formatted))
    expect_true(grepl("--param1", formatted))
})

test_that(".standardize_columns converts column types correctly", {
    df <- data.frame(
        inchiKey = 123,
        smiles = 456,
        xlogP = "1.5",
        other = "unchanged",
        stringsAsFactors = FALSE
    )

    result <- RuSirius:::.standardize_columns(df)

    expect_type(result$inchiKey, "character")
    expect_type(result$smiles, "character")
    expect_type(result$xlogP, "double")
    expect_equal(result$other, "unchanged")
})

test_that(".convert_to_dataframe handles NULL input", {
    result <- RuSirius:::.convert_to_dataframe(NULL, "test_section")
    expect_null(result)
})

test_that(".convert_to_dataframe adds section column", {
    input <- list(
        list(name = "a", value = 1),
        list(name = "b", value = 2)
    )
    result <- RuSirius:::.convert_to_dataframe(input, "my_section")

    expect_s3_class(result, "data.frame")
    expect_true("section" %in% colnames(result))
    expect_true(all(result$section == "my_section"))
})
