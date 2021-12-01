


test_that("arc_to_df works", {

  params <- list(
    x0        =  2,
    y0        = 12.5,
    rx        =  2,
    ry        =  2,
    rot       =  0,
    arcflag   =  0,
    sweepflag =  1,
    x         =  2,
    y         = 8.5
  )

  df <- arc_to_df(params, 10)

  expect_equal(
    df$x,
    c(2, 1.31595971334866, 0.714424780626921, 0.267949192431123,
      0.0303844939755837, 0.0303844939755837, 0.267949192431122, 0.714424780626921,
      1.31595971334866, 2)
  )

  expect_equal(
    df$y,
    c(12.5, 12.3793852415718, 12.032088886238, 11.5, 10.8472963553339,
      10.1527036446661, 9.5, 8.96791111376204, 8.62061475842818, 8.5
    )
  )

})




test_that("arc_to_df works", {

  params <- list(
    x0        =  2,
    y0        = 12.5,
    rx        =  2,
    ry        =  2,
    rot       =  0,
    arcflag   =  1,
    sweepflag =  1,
    x         =  2,
    y         = 8.5
  )

  df <- arc_to_df(params, 2)
  expect_equal(nrow(df), 4)



  params <- list(
    x0        =  2,
    y0        = 12.5,
    rx        =  2,
    ry        =  2,
    rot       =  0,
    arcflag   =  1,
    sweepflag =  0,
    x         =  2,
    y         = 8.5
  )

  df <- arc_to_df(params, 2)
  expect_equal(nrow(df), 4)

})
