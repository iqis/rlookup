context("build")

data("mpg", package = "ggplot2")
mpg_lookup <- mpg %>% build_lookup(cyl, drv, fl, class)


test_that("lookup table has class 'lookup' and 'tbl_df'",{
        expect_is(mpg_lookup, "lookup")
        expect_is(mpg_lookup, "tbl_df")
})

test_that("lookup table has expected dimensions", {
        expect_equal(dim(mpg_lookup), c(19, 3))
})


test_that("lookup table has expected column names, old/new values from data", {
        expect_setequal(unique(mpg_lookup$col_name), c("cyl", "drv", "fl", "class"))
        expect_setequal(unique(mpg_lookup$old_value), c("4","6", "8", "5", "f", "r", "p", "e", "d", "c",
                                                         "compact", "midsize", "suv",  "2seater", "minivan", "pickup", "subcompact"))
        expect_true(all(is.na(mpg_lookup$new_value)))
})


context("write")

tmp_path <- tempfile()
mpg_lookup %>% write_lookup(tmp_path)

test_that("refuses non-`lookup` object to be passed", {
        expect_error({write_lookup(mpg, tmp_path)})
})

test_that("written file has expected size", {
        expect_equal(file.size(tmp_path), 211)
})


context("read")
mpg_lookup_read <- read_lookup(tmp_path)

# test_that("refuses file w/o correct colnames", {
#
# })


test_that("read file equals to original object", {
        expect_equal(mpg_lookup, mpg_lookup_read)
})




context("modify & use")

mpg_lookup2 <- mpg_lookup
# mpg_lookup2$new_value <- c(NA, NA, NA, NA, NA, NA, NA,
#                            "premium", "regular", "other", "diesel", "other", "car", "car", "truck", "car", "car", "truck", "car")
#
# mpg_lookup2 %>% write_lookup(tmp_path)

mpg_lookup2 <- mpg_lookup2 %>% modify_lookup(~(paste0(toupper(.), "WD")), drv)
mpg2 <- mpg %>% use_lookup(mpg_lookup2)

test_that("correct function application & lookup use", {
        expect_setequal(unique(mpg2$drv), c("4WD", "FWD", "RWD"))
})


