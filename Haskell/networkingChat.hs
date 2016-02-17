-- CMPS112 - Networking Chat v.Haskell

-- Cristian Gonzalz
-- Ian Zentner


-----------------------------------------------------------------------------------------------
----------------------------------   Networking Chat    ---------------------------------------
-----------------------------------------------------------------------------------------------


main = do
    putStrLn "Hello, welcome to Networking Chat!"
    putStrLn "Plase insert the user that you wish to connect with: "
    user2 <- getLine
    putStrLn ("You want to connect with " ++ user2 ++ ", (Y or N)?")
    answer <- getLine
    if(confirm answer)
    then --connect with user2    
    
confirm::String->IO Bool
confirm s | s == "Y"    = return True
          | otherwise   = return False
          
