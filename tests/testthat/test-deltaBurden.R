context("Testing deltaBurden method...")

test_that("Checking that the default ITHIM object can be created", {

    expect_silent(createITHIM())

})


test_that("Checking that deltaBurden is within 1% of expected result for Depression", {

    ITHIM.baseline <- createITHIM()
    ITHIM.scenario <- update(ITHIM.baseline, list(muwt = 120, muct = 60))
    deltaDALY <- deltaBurden(ITHIM.baseline, ITHIM.scenario, bur = "daly", dis = "Depression")
    expected <- -44249.44
    expect_equal(deltaDALY, expected = expected, tolerance = 0.01, scale = expected)

})
