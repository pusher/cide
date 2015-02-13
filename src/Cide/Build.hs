{-# LANGUAGE OverloadedStrings #-}
module Cide.Build where

import Turtle

name = "build"

run :: IO ()
run = do
    cd "/tmp"
    mkdir "test"
    output "test/foo" "Hello, world!"  -- Write "Hello, world!" to "test/foo"
    stdout (input "test/foo")          -- Stream "test/foo" to stdout
    rm "test/foo"
    rmdir "test"
    sleep 1
    die "Urk!"
