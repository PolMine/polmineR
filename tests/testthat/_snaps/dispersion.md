# check that warnings are issued if argument sAttribute is used

    Code
      expect_warning(y <- dispersion("GERMAPARLMINI", query = "Integration",
        sAttribute = "date"))
    Message
      i aggregate results by: "query"

---

    Code
      expect_warning(y <- corpus("GERMAPARLMINI") %>% subset(speaker = "Angela Dorothea Merkel") %>%
        dispersion(query = "Integration", sAttribute = "date"))

---

    Code
      expect_warning(y <- corpus("GERMAPARLMINI") %>% hits(query = "Integration",
        s_attribute = "date") %>% dispersion(sAttribute = "date", source = "GERMAPARLMINI"))
    Message
      i aggregate results by: "query"

