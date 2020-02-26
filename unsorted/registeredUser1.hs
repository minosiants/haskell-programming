-- registeredUser1.hs
module RegisteredUser where 


newtype Username = Username String 
newtype AccountNumber = AccountNumber Integer 


data User = UnregisteredUser 
          | RegisteredUser Username AccountNumber


printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistered User"
printUser (RegisteredUser (Username name) 
                          (AccountNumber acctNum)) 
          = putStrLn $ name ++ " " ++ show acctNum
 
-- let rUser = Username "kaspar"
-- let rAcc = AccountNumber "123"
-- let user = RegisteredUser rUser rAcc
