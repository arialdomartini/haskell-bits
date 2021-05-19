module Main where

sayHello :: String -> IO ()
sayHello name = putStrLn ( "Hello " ++ name )

main :: IO()
main = sayHello "Forrest"
