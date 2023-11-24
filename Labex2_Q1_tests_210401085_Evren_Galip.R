install.packages("testthat")
library(testthat)

current_dir <- getwd()
relative_path <- paste(current_dir, "/Labex2_Q1_210401085_Evren_Galip.R", sep = "")
source(relative_path)

test_that("spotify_token creates a global variable", {
  # Fonksiyonu çağır
  spotify_token()
  
  # Global ortamda spotify_token adlı bir değişkenin olup olmadığını kontrol et
  expect_true(exists("spotify_token", envir = .GlobalEnv), 
              info = "spotify_token variable should be created in the global environment.")
})


test_that("spotify_token is a function", {
  # spotify_token adlı değişkenin bir fonksiyon olup olmadığını kontrol et
  expect_true(is.function(spotify_token), 
              info = "spotify_token should be a function.")
})

test_that("spotify_token returns a list", {
  # spotify_token fonksiyonunu çağır
  result <- spotify_token()
  
  # Fonksiyonun döndürdüğü değerin bir liste olup olmadığını kontrol et
  expect_true(is.list(result), 
              info = "spotify_token should return a list.")
})

test_that("spotify_token returns a list with two elements", {
  # spotify_token fonksiyonunu çağır
  result <- spotify_token()
  
  # Fonksiyonun döndürdüğü listenin iki element içerip içermediğini kontrol et
  expect_equal(length(result), 2,
               info = "spotify_token should return a list with two elements.")
})


test_that("spotify_token returns a list with 'status_code' as the first element", {
  # spotify_token fonksiyonunu çağır
  result <- spotify_token()
  
  # Fonksiyonun döndürdüğü listenin ilk elementinin ismini kontrol et
  expect_identical(names(result)[1], "status_code",
                   info = "The first element of the list should be named 'status_code'.")
})

test_that("spotify_token returns a list with 'status_code' as a numeric element", {
  # spotify_token fonksiyonunu çağır
  result <- spotify_token()
  
  # Fonksiyonun döndürdüğü listenin ilk elementinin class'ını kontrol et
  expect_true(is.numeric(result$status_code),
              info = "The 'status_code' element should be of class 'numeric'.")
})

test_that("spotify_token returns a list with 'status_code' equal to 200", {
  # spotify_token fonksiyonunu çağır
  result <- spotify_token()
  
  # Fonksiyonun döndürdüğü listenin 'status_code' adlı elementinin değerini kontrol et
  expect_equal(result$status_code, 200,
               info = "The 'status_code' element should be equal to 200.")
})

test_that("spotify_token returns a list with 'token' as the second element", {
  # spotify_token fonksiyonunu çağır
  result <- spotify_token()
  
  # Fonksiyonun döndürdüğü listenin ikinci elementinin ismini kontrol et
  expect_identical(names(result)[2], "token",
                   info = "The second element of the list should be named 'token'.")
})

test_that("spotify_token returns a list with 'token' as a character element", {
  # spotify_token fonksiyonunu çağır
  result <- spotify_token()
  
  # Fonksiyonun döndürdüğü listenin ikinci elementinin class'ını kontrol et
  expect_true(is.character(result$token),
              info = "The 'token' element should be of class 'character'.")
})

test_that("spotify_token returns a list with 'token' starting with 'Bearer '", {
  # spotify_token fonksiyonunu çağır
  result <- spotify_token()
  
  # Fonksiyonun döndürdüğü listenin ikinci elementinin 'Bearer ' ile başlamasını kontrol et
  expect_true(startsWith(result$token, 'Bearer '), 
              info = "The 'token' element should start with 'Bearer '.")
})

test_that("spotify_token returns a list with 'token' containing 122 letters", {
  # spotify_token fonksiyonunu çağır
  result <- spotify_token()
  
  # Fonksiyonun döndürdüğü listenin ikinci elementinin içinde 122 harf olup olmadığını kontrol et
  expect_equal(nchar(result$token), 122, 
               info = "The 'token' element should contain 122 letters.")
})