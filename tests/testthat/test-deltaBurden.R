context("Testing deltaBurden method...")

test_that("Checking that the default ITHIM object can be created", {

    expect_silent(createITHIM())

})


test_that("Checking that deltaBurden is within 1% of expected result", {

    ITHIM.baseline <- createITHIM()
    ITHIM.scenario <- update(ITHIM.baseline, list(muwt = 120, muct = 60))
    deltaDALY <- deltaBurden(ITHIM.baseline, ITHIM.scenario, bur = "daly", dis = "CVD")
    expect_equal(deltaDALY, expected = -128174, tolerance = 0.01, scale = 128174)

})
