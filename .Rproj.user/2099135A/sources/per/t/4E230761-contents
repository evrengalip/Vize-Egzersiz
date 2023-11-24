install.packages("testthat")
library(testthat)

current_dir <- getwd()
relative_path <- paste(current_dir, "/Labex2_Q1_210401085_Evren_Galip.R", sep = "")
source(relative_path)

test_that("Global Workspace'te spotify_search_artist değişkeni var mı?", {
  expect_true(exists("spotify_search_artist"))
})

test_that("spotify_search_artist değişkeninin tipi 'function' olmalı", {
  expect_is(spotify_search_artist, "function")
})


test_that("spotify_search_artist() herhangi bir artist ismi ile çağrıldığında döndürdüğü çıktı bir liste olmalı", {
  # spotify_search_artist fonksiyonunu çağır
  result <- spotify_search_artist("arbitrary_artist_name")
  
  # Dönen çıktının bir liste olup olmadığını kontrol et
  expect_is(result, "list")
})

test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin iki elementi olmalı", {
  # spotify_search_artist fonksiyonunu çağır
  result <- spotify_search_artist("arbitrary_artist_name")
  
  # Dönen listenin iki element içerip içermediğini kontrol et
  expect_length(result, 2)
})

test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin ilk elementinin ismi status_code olmalı", {
  # spotify_search_artist fonksiyonunu çağır
  result <- spotify_search_artist("arbitrary_artist_name")
  
  # Dönen listenin ilk elementinin isminin "status_code" olup olmadığını kontrol et
  expect_identical(names(result)[1], "status_code")
})


test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin ilk elementinin class’ı numeric olmalı", {
  # spotify_search_artist fonksiyonunu çağır
  result <- spotify_search_artist("arbitrary_artist_name")
  
  # Dönen listenin ilk elementinin class’ının numeric olup olmadığını kontrol et
  expect_is(result$status_code, "integer")
})

test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin status_code adlı elementinin değeri 200’e eşit olmalı", {
  # spotify_search_artist fonksiyonunu çağır
  result <- spotify_search_artist("arbitrary_artist_name")
  
  # Dönen listenin status_code adlı elementinin değerinin 200'e eşit olup olmadığını kontrol et
  expect_equal(result$status_code, 200)
})

test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin ikinci elementinin ismi search_results olmalı", {
  # spotify_search_artist fonksiyonunu çağır
  result <- spotify_search_artist("arbitrary_artist_name")
  
  # Dönen listenin ikinci elementinin adının "search_results" olup olmadığını kontrol et
  expect_identical(names(result)[2], "search_results")
})

test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin ikinci elementinin class’ı data.frame olmalı", {
  # spotify_search_artist fonksiyonunu çağır
  result <- spotify_search_artist("arbitrary_artist_name")
  
  # Dönen listenin ikinci elementinin class’ının "data.frame" olup olmadığını kontrol et
  expect_is(result$search_results, "data.frame")
})

test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin ikinci elementinin iki sütun barındırmalı", {
  # spotify_search_artist fonksiyonunu çağır
  result <- spotify_search_artist("arbitrary_artist_name")
  
  # Dönen listenin ikinci elementinin sütun sayısını kontrol et
  expect_length(colnames(result$search_results), 2)
})

test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin ikinci elementinin sütun isimleri c('artist', 'id') olmalı", {
  # spotify_search_artist fonksiyonunu çağır
  result <- spotify_search_artist("arbitrary_artist_name")
  
  # Dönen listenin ikinci elementinin sütun isimlerini kontrol et
  expect_identical(colnames(result$search_results), c("artist", "id"))
})


test_that("spotify_search_artist('The Doors') çağrıldığında döndürdüğü listenin ikinci elementinin birinci satırının 'id' adlı sütunu '22WZ7M8sxp5THdruNY3gXt' olmalı", {
  # spotify_search_artist fonksiyonunu çağır
  result <- spotify_search_artist("The Doors")
  
  # Dönen listenin ikinci elementinin birinci satırının 'id' sütununun değerini kontrol et
  expect_identical(result$search_results$id[1], "22WZ7M8sxp5THdruNY3gXt")
})

