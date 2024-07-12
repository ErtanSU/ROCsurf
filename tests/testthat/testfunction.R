library(testthat)        # load testthat package
library(ROCsurf)            # load our package


test_that("Type, length and error checking of dG, dW and dL functions", {
  # dG
  resultG <- dG(1:10, 2, 2)
  expect_type(resultG, "double")
  expect_length(resultG, 10)
  expect_vector(resultG, ptype = double(), size = 10)
  expect_error(dG(1:10, -1, 2), "alpha value must be greater than 0")
  expect_error(dG(1:10, 2, -1), "beta value must be greater than 0")

  # dW
  resultW <- dW(1:10, 2, 2)
  expect_type(resultW, "double")
  expect_length(resultW, 10)
  expect_vector(resultW, ptype = double(), size = 10)
  expect_error(dW(1:10, -1, 2), "alpha value must be greater than 0")
  expect_error(dW(1:10, 2, -1), "beta value must be greater than 0")




  # dL
  resultL <- dL(1:10, 2, 2)
  expect_type(resultL, "double")
  expect_length(resultL, 10)
  expect_vector(resultL, ptype = double(), size = 10)
  expect_error(dL(1:10, -1, 2), "alpha value must be greater than 0")
  expect_error(dL(1:10, 2, -1), "beta value must be greater than 0")
})


test_that("Type, length and error checking of pG, pW and pL functions", {
  # pG
  resultG <- pG(1:10, 2, 2)
  expect_type(resultG, "double")
  expect_length(resultG, 10)
  expect_vector(resultG, ptype = double(), size = 10)
  expect_error(pG(1:10, -1, 2), "alpha value must be greater than 0")
  expect_error(pG(1:10, 2, -1), "beta value must be greater than 0")

  # pW
  resultW <- pW(1:10, 2, 2)
  expect_type(resultW, "double")
  expect_length(resultW, 10)
  expect_vector(resultW, ptype = double(), size = 10)
  expect_error(pW(1:10, -1, 2), "alpha value must be greater than 0")
  expect_error(pW(1:10, 2, -1), "beta value must be greater than 0")

  # pL
  resultL <- pL(1:10, 2, 2)
  expect_type(resultL, "double")
  expect_length(resultL, 10)
  expect_vector(resultL, ptype = double(), size = 10)
  expect_error(pL(1:10, -1, 2), "alpha value must be greater than 0")
  expect_error(pL(1:10, 2, -1), "beta value must be greater than 0")
})


test_that("Type, length and error checking of qG, qW and qL functions", {
  # qG
  resultG <- qG(seq(0, 1, length.out = 10), 2, 2)
  expect_type(resultG, "double")
  expect_length(resultG, 10)
  expect_vector(resultG, ptype = double(), size = 10)
  expect_error(qG(seq(0, 1, length.out = 10), -1, 2), "alpha value must be greater than 0")
  expect_error(qG(seq(0, 1, length.out = 10), 2, -1), "beta value must be greater than 0")
  expect_error(qG(c(-0.1, 0.5, 1.1), 2, 2), "p must be between 0 and 1")

  # qW
  resultW <- qW(seq(0, 1, length.out = 10), 2, 2)
  expect_type(resultW, "double")
  expect_length(resultW, 10)
  expect_vector(resultW, ptype = double(), size = 10)
  expect_error(qW(seq(0, 1, length.out = 10), -1, 2), "alpha value must be greater than 0")
  expect_error(qW(seq(0, 1, length.out = 10), 2, -1), "beta value must be greater than 0")
  expect_error(qW(c(-0.1, 0.5, 1.1), 2, 2), "p must be between 0 and 1")

  # qL
  resultL <- qL(seq(0, 1, length.out = 10), 2, 2)
  expect_type(resultL, "double")
  expect_length(resultL, 10)
  expect_vector(resultL, ptype = double(), size = 10)
  expect_error(qL(seq(0, 1, length.out = 10), -1, 2), "alpha value must be greater than 0")
  expect_error(qL(seq(0, 1, length.out = 10), 2, -1), "beta value must be greater than 0")
  expect_error(qL(c(-0.1, 0.5, 1.1), 2, 2), "p must be between 0 and 1")
})

test_that("Type, length and error checking of rG, rW and rL functions", {
  # rG
  resultG <- rG(10, 2, 2)
  expect_type(resultG, "double")
  expect_length(resultG, 10)
  expect_vector(resultG, ptype = double(), size = 10)
  expect_error(rG(0, 2, 2), "n value must be >=1")
  expect_error(rG(10, -1, 2), "alpha value must be greater than 0")
  expect_error(rG(10, 2, -1), "beta value must be greater than 0")

  # rW
  resultW <- rW(10, 2, 2)
  expect_type(resultW, "double")
  expect_length(resultW, 10)
  expect_vector(resultW, ptype = double(), size = 10)
  expect_error(rW(0, 2, 2), "n value must be >=1")
  expect_error(rW(10, -1, 2), "alpha value must be greater than 0")
  expect_error(rW(10, 2, -1), "beta value must be greater than 0")

  # rL
  resultL <- rL(10, 2, 2)
  expect_type(resultL, "double")
  expect_length(resultL, 10)
  expect_vector(resultL, ptype = double(), size = 10)
  expect_error(rL(0, 2, 2), "n value must be >=1")
  expect_error(rL(10, -1, 2), "alpha value must be greater than 0")
  expect_error(rL(10, 2, -1), "beta value must be greater than 0")
})


test_that("function return a double or list",
          {
            expect_type(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                                 model=c("GWL"), method=c("TRUE")), "double")
            expect_type(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                                 model=c("GGW"), method=c("TRUE")), "double")
            expect_type(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                                 model=c("GLL"), method=c("TRUE")), "double")
            expect_type(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=3,beta2=4,alpha3=1,beta3=1),
                                 model=c("WGW"), method=c("TRUE")), "double")
            expect_type(r.tc_vus(true_param=c(alpha1=7,beta1=2,alpha2=7,beta2=3,alpha3=7,beta3=4),
                                 model=c("WWW"), method=c("TRUE")), "double")
            expect_type(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                                 model=c("GGG"), method=c("TRUE")), "double")
            expect_type(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                                 model=c("LLL"), method=c("TRUE")), "double")

            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                                   true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                                   model=c("GWL"),
                                   method=c("TRUE")), "double")
            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                                   true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                                   model=c("GGW"),
                                   method=c("TRUE")), "double")
            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                                   true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                                   model=c("GLL"),
                                   method=c("TRUE")), "double")
            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                                   true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                                   model=c("WGW"),
                                   method=c("TRUE")), "double")
            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                                   true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                                   model=c("WWW"),
                                   method=c("TRUE")), "double")
            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                                   true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                                   model=c("GGG"),
                                   method=c("TRUE")), "double")
            expect_type(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                                   y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                                   z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                                   true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                                   model=c("LLL"),
                                   method=c("TRUE")), "double")

            expect_type(r.tc_graph(true_param = list(alpha1 = 1, beta1 = 1, alpha2 = 1, beta2 = 1, alpha3 = 1, beta3 = 1),
                                   model = "GWL",  method = "TRUE"),"list")
            expect_type(r.tc_graph(true_param = list(alpha1 = 1, beta1 = 1, alpha2 = 1, beta2 = 1, alpha3 = 1, beta3 = 1),
                                   model = "GGW",  method = "TRUE"),"list")
            expect_type(r.tc_graph(true_param = list(alpha1 = 1, beta1 = 1, alpha2 = 1, beta2 = 1, alpha3 = 1, beta3 = 1),
                                   model = "GLL",  method = "TRUE"),"list")
            expect_type(r.tc_graph(true_param = list(alpha1 = 1, beta1 = 1, alpha2 = 1, beta2 = 1, alpha3 = 1, beta3 = 1),
                                   model = "WGW",  method = "TRUE"),"list")
            expect_type(r.tc_graph(true_param = list(alpha1 = 1, beta1 = 1, alpha2 = 1, beta2 = 1, alpha3 = 1, beta3 = 1),
                                   model = "WWW",  method = "TRUE"),"list")
            expect_type(r.tc_graph(true_param = list(alpha1 = 1, beta1 = 1, alpha2 = 1, beta2 = 1, alpha3 = 1, beta3 = 1),
                                   model = "GGG",  method = "TRUE"),"list")
            expect_type(r.tc_graph(true_param = list(alpha1 = 1, beta1 = 1, alpha2 = 1, beta2 = 1, alpha3 = 1, beta3 = 1),
                                   model = "LLL",  method = "TRUE"),"list")

          })


# Test whether the output contains the right number
test_that("functions returns a list with the specified length", {
  expect_length(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                         model=c("GWL"), method=c("TRUE")), 1)
  expect_length(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                         model=c("GGW"), method=c("TRUE")), 1)
  expect_length(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                         model=c("GLL"), method=c("TRUE")), 1)
  expect_length(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=3,beta2=4,alpha3=1,beta3=1),
                         model=c("WGW"), method=c("TRUE")), 1)
  expect_length(r.tc_vus(true_param=c(alpha1=7,beta1=2,alpha2=7,beta2=3,alpha3=7,beta3=4),
                         model=c("WWW"), method=c("TRUE")), 1)
  expect_length(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                         model=c("GGG"), method=c("TRUE")), 1)
  expect_length(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                         model=c("LLL"), method=c("TRUE")), 1)

  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("GWL"),method=c("TRUE")), 25)
  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("GGW"),method=c("TRUE")), 25)
  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("GLL"),
                           method=c("TRUE")), 25)
  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("WGW"),
                           method=c("TRUE")), 25)
  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("WWW"),
                           method=c("TRUE")), 25)
  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("GGG"),
                           method=c("TRUE")), 25)
  expect_length(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("LLL"),
                           method=c("TRUE")), 25)
})

# Test whether the output is a vector with the expected size
test_that("functions returns a  vector with the expected size", {

  expect_vector(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                         model=c("GWL"), method=c("TRUE")), ptype=double(),size=1)
  expect_vector(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                         model=c("GGW"), method=c("TRUE")), ptype=double(),size=1)
  expect_vector(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                         model=c("GLL"), method=c("TRUE")), ptype=double(),size=1)
  expect_vector(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=3,beta2=4,alpha3=1,beta3=1),
                         model=c("WGW"), method=c("TRUE")), ptype=double(),size=1)
  expect_vector(r.tc_vus(true_param=c(alpha1=7,beta1=2,alpha2=7,beta2=3,alpha3=7,beta3=4),
                         model=c("WWW"), method=c("TRUE")), ptype=double(),size=1)
  expect_vector(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                         model=c("GGG"), method=c("TRUE")), ptype=double(),size=1)
  expect_vector(r.tc_vus(true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),
                         model=c("LLL"), method=c("TRUE")), ptype=double(),size=1)

  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("GWL"),method=c("TRUE")), ptype=rbind(),size=5)
  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("GGW"),method=c("TRUE")), ptype=rbind(),size=5)
  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("GLL"),
                           method=c("TRUE")), ptype=rbind(),size=5)
  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("WGW"),
                           method=c("TRUE")), ptype=rbind(),size=5)
  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("WWW"),
                           method=c("TRUE")), ptype=rbind(),size=5)
  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("GGG"),
                           method=c("TRUE")), ptype=rbind(),size=5)
  expect_vector(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),
                           true_param=c(alpha1=1,beta1=1,alpha2=1,beta2=1,alpha3=1,beta3=1),init_index=c(median(x),median(y)),
                           model=c("LLL"),
                           method=c("TRUE")), ptype=rbind(),size=5)
})



# Test whether the output is an error
test_that("functions returns error", {

  expect_error(r.tc_vus( init_param = c(-1, 1, 1, 1, 1, 1),model="GWL",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),model="GWL",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, -1, 1, 1, 1),model="GWL",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, -1, 1, 1),model="GWL",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, -1, 1),model="GWL",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, 1, -1),model="GWL",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),model="GWL",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),model="GWL",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),model="GWL",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),model="GWL",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),model="GWL",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),model="GWL",method="TRUE"), "beta3 value must be greater than 0")



  expect_error(r.tc_vus( init_param = c(-1, 1, 1, 1, 1, 1),model="GGW",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),model="GGW",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, -1, 1, 1, 1),model="GGW",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, -1, 1, 1),model="GGW",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, -1, 1),model="GGW",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, 1, -1),model="GGW",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),model="GGW",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),model="GGW",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),model="GGW",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),model="GGW",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),model="GGW",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),model="GGW",method="TRUE"), "beta3 value must be greater than 0")

  expect_error(r.tc_vus( init_param = c(-1, 1, 1, 1, 1, 1),model="GLL",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),model="GLL",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, -1, 1, 1, 1),model="GLL",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, -1, 1, 1),model="GLL",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, -1, 1),model="GLL",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, 1, -1),model="GLL",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),model="GLL",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),model="GLL",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),model="GLL",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),model="GLL",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),model="GLL",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),model="GLL",method="TRUE"), "beta3 value must be greater than 0")

  expect_error(r.tc_vus( init_param = c(-1, 1, 1, 1, 1, 1),model="WGW",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),model="WGW",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, -1, 1, 1, 1),model="WGW",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, -1, 1, 1),model="WGW",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, -1, 1),model="WGW",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, 1, -1),model="WGW",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),model="WGW",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),model="WGW",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),model="WGW",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),model="WGW",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),model="WGW",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),model="WGW",method="TRUE"), "beta3 value must be greater than 0")

  expect_error(r.tc_vus( init_param = c(-1, 1, 1, 1, 1, 1),model="WWW",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),model="WWW",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, -1, 1, 1, 1),model="WWW",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, -1, 1, 1),model="WWW",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, -1, 1),model="WWW",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, 1, -1),model="WWW",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),model="WWW",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),model="WWW",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),model="WWW",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),model="WWW",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),model="WWW",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),model="WWW",method="TRUE"), "beta3 value must be greater than 0")


  expect_error(r.tc_vus( init_param = c(-1, 1, 1, 1, 1, 1),model="GGG",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),model="GGG",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, -1, 1, 1, 1),model="GGG",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, -1, 1, 1),model="GGG",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, -1, 1),model="GGG",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, 1, -1),model="GGG",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),model="GGG",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),model="GGG",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),model="GGG",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),model="GGG",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),model="GGG",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),model="GGG",method="TRUE"), "beta3 value must be greater than 0")


  expect_error(r.tc_vus( init_param = c(-1, 1, 1, 1, 1, 1),model="LLL",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, -1, 1, 1, 1, 1),model="LLL",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, -1, 1, 1, 1),model="LLL",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, -1, 1, 1),model="LLL",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, -1, 1),model="LLL",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( init_param = c(1, 1, 1, 1, 1, -1),model="LLL",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_vus( true_param  = c(-1, 1, 1, 1, 1, 1),model="LLL",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, -1, 1, 1, 1, 1),model="LLL",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, -1, 1, 1, 1),model="LLL",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, -1, 1, 1),model="LLL",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, -1, 1),model="LLL",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_vus( true_param  = c(1, 1, 1, 1, 1, -1),model="LLL",method="TRUE"), "beta3 value must be greater than 0")



  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="GWL",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="GWL",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="GWL",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="GWL",method="TRUE"), "beta3 value must be greater than 0")




  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="GGW",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="GGW",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="GGW",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="GGW",method="TRUE"), "beta3 value must be greater than 0")





  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="GLL",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="GLL",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="GLL",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="GLL",method="TRUE"), "beta3 value must be greater than 0")





  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="WGW",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="WGW",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="WGW",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="WGW",method="TRUE"), "beta3 value must be greater than 0")






  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="WWW",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="WWW",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="WWW",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="WWW",method="TRUE"), "beta3 value must be greater than 0")





  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="GGG",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="GGG",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="GGG",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="GGG",method="TRUE"), "beta3 value must be greater than 0")





  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="LLL",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),init_param = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="LLL",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(-1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, -1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, -1, 1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), true_param  = c(1, 1, 1, -1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, -1, 1),init_index = c(median(x),median(y)),model="LLL",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_index( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                           y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                           z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),true_param  = c(1, 1, 1, 1, 1, -1),init_index = c(median(x),median(y)),model="LLL",method="TRUE"), "beta3 value must be greater than 0")










  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(-1, 1, 1, 1, 1, 1),model="GWL",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, -1, 1, 1, 1, 1),model="GWL",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, -1, 1, 1, 1),model="GWL",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, -1, 1, 1),model="GWL",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, -1, 1),model="GWL",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, 1, -1),model="GWL",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_graph( true_param  = c(-1, 1, 1, 1, 1, 1),model="GWL",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, -1, 1, 1, 1, 1),model="GWL",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, -1, 1, 1, 1),model="GWL",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, -1, 1, 1),model="GWL",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, -1, 1),model="GWL",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, 1, -1),model="GWL",method="TRUE"), "beta3 value must be greater than 0")



  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(-1, 1, 1, 1, 1, 1),model="GGW",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, -1, 1, 1, 1, 1),model="GGW",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, -1, 1, 1, 1),model="GGW",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, -1, 1, 1),model="GGW",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, -1, 1),model="GGW",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, 1, -1),model="GGW",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_graph( true_param  = c(-1, 1, 1, 1, 1, 1),model="GGW",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, -1, 1, 1, 1, 1),model="GGW",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, -1, 1, 1, 1),model="GGW",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, -1, 1, 1),model="GGW",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, -1, 1),model="GGW",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, 1, -1),model="GGW",method="TRUE"), "beta3 value must be greater than 0")

  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(-1, 1, 1, 1, 1, 1),model="GLL",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, -1, 1, 1, 1, 1),model="GLL",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, -1, 1, 1, 1),model="GLL",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, -1, 1, 1),model="GLL",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, -1, 1),model="GLL",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, 1, -1),model="GLL",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_graph( true_param  = c(-1, 1, 1, 1, 1, 1),model="GLL",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, -1, 1, 1, 1, 1),model="GLL",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, -1, 1, 1, 1),model="GLL",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, -1, 1, 1),model="GLL",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, -1, 1),model="GLL",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, 1, -1),model="GLL",method="TRUE"), "beta3 value must be greater than 0")


  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(-1, 1, 1, 1, 1, 1),model="WGW",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, -1, 1, 1, 1, 1),model="WGW",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, -1, 1, 1, 1),model="WGW",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, -1, 1, 1),model="WGW",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, -1, 1),model="WGW",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, 1, -1),model="WGW",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_graph( true_param  = c(-1, 1, 1, 1, 1, 1),model="WGW",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, -1, 1, 1, 1, 1),model="WGW",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, -1, 1, 1, 1),model="WGW",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, -1, 1, 1),model="WGW",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, -1, 1),model="WGW",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, 1, -1),model="WGW",method="TRUE"), "beta3 value must be greater than 0")

  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(-1, 1, 1, 1, 1, 1),model="WWW",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, -1, 1, 1, 1, 1),model="WWW",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, -1, 1, 1, 1),model="WWW",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, -1, 1, 1),model="WWW",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, -1, 1),model="WWW",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, 1, -1),model="WWW",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_graph( true_param  = c(-1, 1, 1, 1, 1, 1),model="WWW",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, -1, 1, 1, 1, 1),model="WWW",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, -1, 1, 1, 1),model="WWW",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, -1, 1, 1),model="WWW",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, -1, 1),model="WWW",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, 1, -1),model="WWW",method="TRUE"), "beta3 value must be greater than 0")



  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(-1, 1, 1, 1, 1, 1),model="GGG",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, -1, 1, 1, 1, 1),model="GGG",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, -1, 1, 1, 1),model="GGG",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, -1, 1, 1),model="GGG",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, -1, 1),model="GGG",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, 1, -1),model="GGG",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_graph( true_param  = c(-1, 1, 1, 1, 1, 1),model="GGG",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, -1, 1, 1, 1, 1),model="GGG",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, -1, 1, 1, 1),model="GGG",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, -1, 1, 1),model="GGG",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, -1, 1),model="GGG",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, 1, -1),model="GGG",method="TRUE"), "beta3 value must be greater than 0")


  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(-1, 1, 1, 1, 1, 1),model="LLL",method="MLE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, -1, 1, 1, 1, 1),model="LLL",method="MLE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, -1, 1, 1, 1),model="LLL",method="MLE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, -1, 1, 1),model="LLL",method="MLE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, -1, 1),model="LLL",method="MLE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617),empirical = FALSE, init_param = c(1, 1, 1, 1, 1, -1),model="LLL",method="MLE"), "beta3 value must be greater than 0")

  expect_error(r.tc_graph( true_param  = c(-1, 1, 1, 1, 1, 1),model="LLL",method="TRUE"), "alpha1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, -1, 1, 1, 1, 1),model="LLL",method="TRUE"), "beta1 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, -1, 1, 1, 1),model="LLL",method="TRUE"), "alpha2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, -1, 1, 1),model="LLL",method="TRUE"), "beta2 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, -1, 1),model="LLL",method="TRUE"), "alpha3 value must be greater than 0")
  expect_error(r.tc_graph( true_param  = c(1, 1, 1, 1, 1, -1),model="LLL",method="TRUE"), "beta3 value must be greater than 0")



  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GWL",method="MLE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="MLE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GWL",method="MLE"), "Optimization did not converge.Please check your initial parameters.")



  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GWL",method="AD"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="AD"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GWL",method="AD"), "Optimization did not converge.Please check your initial parameters.")






  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GWL",method="CvM"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="CvM"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GWL",method="CvM"), "Optimization did not converge.Please check your initial parameters.")






  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GWL",method="LSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="LSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GWL",method="LSE"), "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GWL",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GWL",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GWL",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")












  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GGW",method="MLE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 0, 0, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="MLE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGW",method="MLE"), "Optimization did not converge.Please check your initial parameters.")



  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GGW",method="AD"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="AD"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGW",method="AD"), "Optimization did not converge.Please check your initial parameters.")





  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GGW",method="CvM"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="CvM"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGW",method="CvM"), "Optimization did not converge.Please check your initial parameters.")







  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GGW",method="LSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="LSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGW",method="LSE"), "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GGW",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGW",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GGW",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")




  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GLL",method="MLE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="MLE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GLL",method="MLE"), "Optimization did not converge.Please check your initial parameters.")



  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GLL",method="AD"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="AD"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GLL",method="AD"), "Optimization did not converge.Please check your initial parameters.")






  expect_error(r.tc_vus( x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                         y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                         z=c(10000, 1, 2, 4, 2.893617),init_param = c(1, 1, 1, 1, 1, 1),model="GLL",method="CvM"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="CvM"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GLL",method="CvM"), "Optimization did not converge.Please check your initial parameters.")







  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GLL",method="LSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="LSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GLL",method="LSE"), "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GLL",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GLL",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_graph( init_param = c(1, 1, 1, 1, 1, 1),model="GLL",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")




  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="WGW",method="MLE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 0, 0, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="MLE"), "Optimization did not converge.Please check your initial parameters.")




  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="WGW",method="AD"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="AD"), "Optimization did not converge.Please check your initial parameters.")





  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="WGW",method="CvM"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="CvM"), "Optimization did not converge.Please check your initial parameters.")








  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="WGW",method="LSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="LSE"), "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="WGW",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WGW",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617),  init_param = c(1, 1, 1, 1, 1, 1),model="WWW",method="MLE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 0, 0, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="MLE"), "Optimization did not converge.Please check your initial parameters.")




  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="WWW",method="AD"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="AD"), "Optimization did not converge.Please check your initial parameters.")





  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="WWW",method="CvM"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="CvM"), "Optimization did not converge.Please check your initial parameters.")








  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="WWW",method="LSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="LSE"), "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="WWW",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 4.939489, 6.072339, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="WWW",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GGG",method="MLE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 0, 0, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="MLE"), "Optimization did not converge.Please check your initial parameters.")



  expect_error(r.tc_vus(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                        y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                        z=c(10000, 1, 2, 4, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),model="GGG",method="AD"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(0.7736414, 1.0131692, 0.8667612, 1.0066519, 1.0372385),
                          y=c(1.5200108, 0.0617668, 6.0647578, 0.7594201, 0.3714640),
                          z=c(3.485613, 0, 0, 3.995163, 2.893617), init_param = c(1, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="AD"), "Optimization did not converge.Please check your initial parameters.")





  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000), init_param = c(1, 1, 1, 1, 1, 1),model="GGG",method="CvM"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="CvM"), "Optimization did not converge.Please check your initial parameters.")







  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000), init_param = c(1, 1, 1, 1, 1, 1),model="GGG",method="LSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="LSE"), "Optimization did not converge.Please check your initial parameters.")



  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000),  init_param = c(1, 1, 1, 1, 1, 1),model="GGG",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="GGG",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000), init_param = c(1, 1, 1, 1, 1, 1),model="LLL",method="MLE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="MLE"), "Optimization did not converge.Please check your initial parameters.")


  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000), init_param = c(1, 1, 1, 1, 1, 1),model="LLL",method="AD"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="AD"), "Optimization did not converge.Please check your initial parameters.")




  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000), init_param = c(1, 1, 1, 1, 1, 1),model="LLL",method="CvM"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="CvM"), "Optimization did not converge.Please check your initial parameters.")





  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000), init_param = c(1, 1, 1, 1, 1, 1),model="LLL",method="LSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="LSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_vus(x=c(0, 0, 0, 1.0066519, 1.0372385),
                        y=c(0, 0, 0, 0, 0.3714640),
                        z=c(10000, 1, 2, 4, 10000), init_param = c(1, 1, 1, 1, 1, 1),model="LLL",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")

  expect_error(r.tc_index(x=c(10, 110,1110, 11110, 111110),
                          y=c(10000000, 0, 4, 4, 4),
                          z=c(1,2), init_param = c(1000, 1, 1, 1, 1, 1),init_index = c(median(x),median(y)),model="LLL",method="WLSE"), "Optimization did not converge.Please check your initial parameters.")


})


