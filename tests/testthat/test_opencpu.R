testthat::context("opencpu")

test_that(
  "opencpu server - subsetting",
  {
    skip_on_cran()
    skip_on_ci()
      
    gparl <- corpus("GERMAPARLMINI", server = "https://cloud.opencpu.org", restricted = FALSE)

    gparl_sub1 <- subset(gparl, date == "2009-11-11")
    gparl_sub2 <- subset(gparl, substitute(date == "2009-11-11"))
    day <- "2009-11-11"
    gparl_sub3 <- subset(gparl, substitute(date == day, list(day = day)))

    expect_identical(gparl_sub1, gparl_sub2)
    expect_identical(gparl_sub1, gparl_sub3)


    days <- c("FAS_200109_11098", "2009-11-12")
    gparl_sub4 <- subset(gparl, substitute(date %in% days, list(days = days)))

    gparl_sub_local <- corpus("GERMAPARLMINI") %>%
      subset(substitute(date %in% days, list(days = days)))

    expect_identical(size(gparl_sub4), size(gparl_sub_local))

  }
)


test_that(
  "opencpu server - s_attributes",
  {
    skip_on_cran()
    skip_on_ci()
    
    gparl <- corpus("GERMAPARLMINI", server = "https://cloud.opencpu.org", restricted = FALSE)
    gparl_s_attr <- s_attributes(gparl)
    gparl_s_attr_local <- s_attributes("GERMAPARLMINI")
    expect_identical(gparl_s_attr, gparl_s_attr_local)

  }
)
