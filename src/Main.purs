module Main where

import Airbnbeast.WebServer as WebServer
import HTTPure (ServerM)

main :: ServerM
main = WebServer.startServer 8080

