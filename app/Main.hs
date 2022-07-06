import Control.Exception
import Data.Char
import Data.List.Split
import System.Directory
import System.IO
import Data.List
import Text.Read
import Data.Time
import Control.Monad.Trans.State.Lazy (StateT(runStateT), get, runState)
import Control.Monad.Trans.Class (lift)

data UserInfo = UserInfo {userName :: String, password :: String} deriving (Show)
data Wallet = Wallet {walletName :: String, currency :: String} deriving (Show, Eq)
data Transaction = Transaction {trxId :: String, date :: String, transactionType :: String, amount :: Double, description :: String} deriving (Show)
type UserData = (UserInfo, [Wallet], [[Transaction]])
type WalletDetail = (Wallet, [Transaction])

userInfoFileName = "UserInfo.txt"
walletInfoFileName = "WalletInfo.txt"

--------------------------------------
-- INPUT UTILS
--------------------------------------
putStrAndFlush :: String -> IO()
putStrAndFlush str = do
    putStr str
    hFlush stdout

getDouble :: String -> IO Double
getDouble str = do
    val <- getLine
    if (readMaybe val :: Maybe Double) == Nothing then do
        putStrAndFlush ("Put number!\n" ++ str)
        getDouble str
    else
        return (read val :: Double)

getNumeric :: String -> (Int, Int) -> IO Int
getNumeric str (least, most) = do
    val <- getLine
    if (readMaybe val :: Maybe Int) == Nothing then do
        putStrAndFlush ("Put number!\n" ++ str)
        getNumeric str (least, most)
    else
        if (read val < least || read val > most) && least /= most then do
            putStrAndFlush ("Insert a valid number!\n" ++ str)
            getNumeric str (least, most)
        else
            return (read val)
checkPassword :: String -> String -> IO ()
checkPassword pass str = do
    checkStateResult <- runStateT checkPasswordState pass
    if fst checkStateResult then
        return()
    else do
        putStrAndFlush ("Wrong password!\n" ++ str)
        checkPassword pass str
        logToFile "Login attempted with failure"

checkPasswordState :: StateT String IO Bool
checkPasswordState = do
    val <- Control.Monad.Trans.Class.lift getPassword
    pass <- Control.Monad.Trans.State.Lazy.get
    return (val == pass)

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getPassword :: IO String
getPassword = do
    hFlush stdout
    pass <- withEcho False getLine
    putChar '\n'
    return pass

--------------------------------------
-- FILE OPERATIONS
--------------------------------------
loadUser :: IO UserInfo
loadUser = do
    content <- readFile userInfoFileName
    let contentLine = lines content
    let split = splitOn "|" (head contentLine)
    return (getUserPass split)

loadWallet :: IO [Wallet]
loadWallet = do
    content <- readFile walletInfoFileName
    let contentLine = lines content
    return (loadWallet' contentLine)

loadWallet' :: [String] -> [Wallet]
loadWallet' [] = []
loadWallet' (x : xs) = do
    let split = splitOn "|" x
    let (a, b) = (head split, head (tail split))
    Wallet {walletName = a, currency = b} : loadWallet' xs

loadWalletDetail :: [Wallet] -> [IO [Transaction]]
loadWalletDetail [] = []
loadWalletDetail (Wallet {walletName = name} : xs) = loadWalletDetail' name : loadWalletDetail xs

loadWalletDetail' :: String -> IO [Transaction]
loadWalletDetail' fileName = do
    content <- readFile (fileName ++ ".txt")
    let contentLine = lines content
    return (loadTransactions contentLine)

loadTransactions :: [String] -> [Transaction]
loadTransactions = map (populateTransactionFields . splitOn "|")

populateTransactionFields :: [String] -> Transaction
populateTransactionFields (a : b : c : d : e : []) = Transaction {trxId = a, date = b, transactionType = c, amount = read d, description = e}

saveUserData :: UserInfo -> IO()
saveUserData UserInfo { userName = name, password = pass } = do
    writeFile userInfoFileName (name ++"|" ++ pass)

prepareWalletDataString :: [Wallet] -> String
prepareWalletDataString [] = ""
prepareWalletDataString (Wallet {walletName = a, currency = b} : xs) = a ++ "|" ++ b ++ "\n" ++ prepareWalletDataString xs

saveWalletData :: [Wallet] -> IO()
saveWalletData wallets = writeFile walletInfoFileName (prepareWalletDataString wallets)

prepareTransactionDataString :: [Transaction] -> String
prepareTransactionDataString [] = ""
prepareTransactionDataString (Transaction {trxId = a, date = b, transactionType = c, amount = d, description = e} : xs) = a ++ "|" ++ b ++ "|" ++ c ++ "|" ++ show d ++ "|" ++ e ++ "\n" ++ prepareTransactionDataString xs

saveTransactionData :: String -> [Transaction] -> IO()
saveTransactionData fileName transactions = writeFile fileName (prepareTransactionDataString transactions)

logToFile :: String -> IO()
logToFile txt = do
    time <- formatTime defaultTimeLocale "%d/%m/%Y - %H:%M:%S: " <$> getZonedTime
    appendFile "activity.log" (time ++ " " ++ txt ++ "\n")

--------------------------------------
-- USER REGISTRATION & SIGN IN
--------------------------------------
isUserFileExist :: IO Bool
isUserFileExist = doesFileExist userInfoFileName

checkPasswordValid :: String -> Bool
checkPasswordValid pass = length pass >= 8 && any isAlpha pass && any isNumber pass

passwordSetup :: String -> IO UserInfo
passwordSetup name = do
    putStrAndFlush "Insert password (Min. 8 characters, alphanumeric): "
    pass <- getPassword

    if checkPasswordValid pass
        then
        ( do
            putStrAndFlush "Repeat password: "
            repeatPass <- getPassword

            if pass /= repeatPass
                then do
                putStrLn "Mismatched password!\n"
                passwordSetup name
                else return UserInfo {userName = name, password = pass}
        )
        else passwordSetup name

getUserPass :: [String] -> UserInfo
getUserPass (a : b : []) = UserInfo {userName = a, password = b}

getUserData :: UserInfo -> (String, String)
getUserData UserInfo {userName = name, password = pass} = (name, pass)

firstRegister :: IO ()
firstRegister = do
    putStrLn "=========================="
    putStrLn "Welcome to Expense Manager"
    putStrLn "=========================="
    putStrLn "Please enter your info for initial setup\n"
    putStrAndFlush "Insert your name: "
    name <- getLine
    user <- passwordSetup name
    saveUserData user

    putStrLn "\nAccount created!\n\nPlease setup your first wallet"
    logToFile ("User " ++ name ++ " is registered")
    addWallet (user, [], [])

authenticate :: UserData -> IO()
authenticate (userData, wallets, transactions) = do
    let (name, pass) = getUserData userData
    putStrLn "\n=================="
    putStrLn ("Welcome " ++ name)
    putStrLn "=================="
    putStrAndFlush "Insert your password to access your account: "
    checkPassword pass "Insert your password to access your account: "

    logToFile ("User " ++ name ++ " successfully logged in")
    showMainMenu (userData, wallets, transactions)

--------------------------------------
-- WALLET OPERATIONS
--------------------------------------
addWallet :: UserData -> IO ()
addWallet (userData, wallets, transactions) = do
    putStrAndFlush "Wallet name: "
    wName <- getLine

    putStrAndFlush "Currency: "
    cName <- getLine

    putStrAndFlush "Initial balance: "
    bal <- getDouble "Initial balance: "

    let newWallet = [Wallet {walletName = wName, currency = cName}]
    let walletList = wallets ++ newWallet

    localeDate <- formatTime defaultTimeLocale "%d/%m/%Y" <$> getZonedTime
    let newTransactions = [Transaction {trxId = show 1, date = localeDate, transactionType = "Income", amount = bal, description = "Initial balance"}]
    let transactionList = transactions ++ [newTransactions]

    saveWalletData walletList
    saveTransactionData (wName ++ ".txt") newTransactions
    logToFile ("Wallet " ++ wName ++ " is added with initial balance of " ++ show bal ++ " " ++ cName)

    showMainMenu (userData, walletList, transactionList)

deleteWallet :: Int -> UserData -> IO()
deleteWallet no (userData, wallets, transactions) = do
    let (walletFront, walletBack) = splitAt (no - 1) wallets
    let (a, b) = getWalletData (head walletBack)
    let newWallets = walletFront ++ tail walletBack

    let (transactionsFront, transactionsBack) = splitAt (no - 1) transactions
    let newTransactions = transactionsFront ++ tail transactionsBack

    saveWalletData newWallets
    removeFile (a ++ ".txt")
    putStrLn "\nWallet deleted!"
    logToFile ("Wallet " ++ a ++ " is deleted")

    showMainMenu (userData, newWallets, newTransactions)

calculateExpense :: Double -> [Transaction] -> Double
calculateExpense expense [] = expense
calculateExpense expense (Transaction {transactionType = tType, amount = am} : xs) = 
    if tType == "Expense" then calculateExpense (expense + am) xs else calculateExpense expense xs

calculateIncome :: Double -> [Transaction] -> Double
calculateIncome income [] = income
calculateIncome income (Transaction {transactionType = tType, amount = am} : xs) = 
    if tType == "Income" then calculateIncome (income + am) xs else calculateIncome income xs    

calculateBalance :: Double -> [Transaction] -> Double
calculateBalance balance [] = balance
calculateBalance balance (Transaction {transactionType = tType, amount = am} : xs) =
    if tType == "Income"
        then calculateBalance (balance + am) xs
        else calculateBalance (balance - am) xs

showBalance :: WalletDetail -> IO ()
showBalance (_, []) = return ()
showBalance ((Wallet {walletName = wName, currency = cName}, transactions)) = do
    let balance = calculateBalance 0 transactions
    putStrLn (wName ++ ": " ++ show balance ++ " " ++ cName)

showZeroBalance :: WalletDetail -> IO ()
showZeroBalance ((Wallet {walletName = wName, currency = cName}, transactions)) =
    putStrLn (wName ++ ": 0.0 " ++ cName)

showBalances :: ([Wallet], [[Transaction]]) -> IO ()
showBalances ([], _) = return ()
showBalances (x : xs, y : ys) = do
    if length y > 0 then showBalance (x, y) else showZeroBalance(x, y)
    showBalances (xs, ys)

showWallets :: Int -> [Wallet] -> IO ()
showWallets no [] = putStrLn (show no ++ ". Cancel" )
showWallets no (Wallet {walletName = wName, currency = cName} : xs) = do
    putStrLn (show no ++ ". " ++ wName)
    showWallets (no + 1) xs

getWalletData:: Wallet -> (String, String)
getWalletData Wallet {walletName = a, currency = b} = (a,b)

--------------------------------------
-- TRANSACTION OPERATIONS
--------------------------------------
addSpace :: Int -> String
addSpace 0 = ""
addSpace x = " " ++ addSpace (x-1)

printLine :: Int -> IO()
printLine 0 = putStrLn ""
printLine x = do
    putStrAndFlush "-"
    printLine (x-1)

printTransactionTitle :: IO()
printTransactionTitle = do
    putStrLn ""
    printLine 60
    putStrLn "ID  | Date       | Type    | Amount      | Description"
    printLine 60

showTransaction :: Transaction -> IO()
showTransaction Transaction {trxId = a, date = b, transactionType = c, amount = d, description = e} = putStrLn (a ++ addSpace (4 - length a) ++ "| " ++ b ++ addSpace (11 - length b) ++ "| " ++ c ++ addSpace (8 - length c) ++ "| " ++ show d ++ addSpace (12 - length (show d)) ++ "| " ++ e)

showTransactions :: [Transaction] -> IO()
showTransactions [] = printLine 60
showTransactions (x : xs) = do
    showTransaction x
    showTransactions xs

getTransactionData :: Transaction -> (String, String, String, Double, String)
getTransactionData Transaction { trxId = a, date = b, transactionType = c, amount = d, description = e } = (a, b, c, d, e)

findTransactionById :: Int -> String -> [Transaction] -> Maybe Int
findTransactionById index id [] = Nothing
findTransactionById index id (Transaction {trxId = trxId} : xs) = if id == trxId then Just index else findTransactionById (index + 1) id xs

updateTransaction :: Int -> String -> Transaction -> Transaction
updateTransaction field replacement (Transaction {trxId = a, date = b, transactionType = c, amount = d, description = e}) = do
    case field of
        1 -> Transaction {trxId = a, date = replacement, transactionType = c, amount = d, description = e}
        2 -> Transaction {trxId = a, date = b, transactionType = replacement, amount = d, description = e}
        3 -> Transaction {trxId = a, date = b, transactionType = c, amount = read replacement, description = e}
        _ -> Transaction {trxId = a, date = b, transactionType = c, amount = d, description = replacement}

filterTransaction :: Int -> String -> [Transaction] -> IO()
filterTransaction choice filterReq transactions = do
    let filtered = filter (compareTransaction choice filterReq) transactions
    if length filtered > 0 then do
        printTransactionTitle
        showTransactions filtered
    else
        putStrLn "\nNo transaction found"

compareTransaction :: Int -> String -> Transaction -> Bool
compareTransaction choice filterReq Transaction {trxId = a, date = b, transactionType = c, amount = d, description = e} = case choice of
    1 -> filterReq == a
    2 -> filterReq `isInfixOf` b
    3 -> filterReq == c
    4 -> read filterReq == d
    _ -> map toLower filterReq `isInfixOf` map toLower e

--------------------------------------
-- MENUS
--------------------------------------
showMainMenu :: UserData -> IO ()
showMainMenu (UserInfo {userName = name, password = pass}, wallets, transactions) = do
    putStrLn "\n=================="
    putStrLn ("Welcome " ++ name)
    putStrLn "=================="

    if length wallets > 0 then
        putStrLn "Your balance is: "
    else
        putStrLn "Please add a wallet to continue"
    showBalances (wallets, transactions)

    putStrLn "\nWhat to do?"
    putStrLn "1. Select wallet    2. Add wallet       3. Delete wallet      4. Exit"
    putStrAndFlush "Input: "
    input <- getNumeric "Input: " (1, 4)

    let user = UserInfo {userName = name, password = pass}
    case input of
        1 -> do
            putStrLn "\nSelect a wallet"
            showWallets 1 wallets
            putStrAndFlush "Input ID: "
            input <- getNumeric "Input ID: " (1, length wallets + 1)

            if input <= length wallets then do
                let index = input - 1
                showWalletMenu index (user, wallets, transactions)
            else showMainMenu (user, wallets, transactions)

        2 -> addWallet (user, wallets, transactions)

        3 -> do
            putStrLn "\nWhich wallet?"
            showWallets 1 wallets
            putStrAndFlush "Input ID: "
            input <- getNumeric "Input ID: " (1, length wallets + 1)

            if input <= length wallets then do
                deleteWallet input (user, wallets, transactions)
            else showMainMenu (user, wallets, transactions)

        _ -> do
            logToFile "Application exit"
            putStrLn "\nThank you"

showWalletMenu :: Int -> UserData -> IO()
showWalletMenu index (user, wallets, transactions) = do
    let wallet = wallets !! index
    let (nam, cur) = getWalletData wallet

    let transactionList = transactions !! index

    putStrLn ("\n=== " ++ nam ++ " ===")
    let balance = calculateBalance 0 transactionList
    let income = calculateIncome 0 transactionList
    let expense = calculateExpense 0 transactionList
    putStrLn ("Balance: " ++ show balance ++ " " ++ cur)
    putStrLn ("Total income: " ++ show income ++ " " ++ cur)
    putStrLn ("Total expense: " ++ show expense ++ " " ++ cur)
    putStrLn "\nWhat to do?"
    putStrLn "1. Add transaction    2. Edit transaction     3. Delete transaction"
    putStrLn "4. Show transactions  5. Filter transaction   6. Go back"
    putStrAndFlush "Input: "
    input <- getNumeric "Input: " (1,6)

    let wallet = Wallet { walletName = nam, currency = cur }
    case input of
        1 -> do
            let finalTransaction = transactionList !! (length transactionList - 1)
            let (a, b, c, d, e) = getTransactionData finalTransaction

            date <- formatTime defaultTimeLocale "%d/%m/%Y" <$> getZonedTime

            putStrLn "Is it\n1. Income   2. Expense"
            putStrAndFlush "Input: "
            txType <- getNumeric "Input: " (1,2)

            putStrAndFlush "Amount: "
            amount <- getDouble "Amount: "

            putStrAndFlush "Description: "
            desc <- getLine

            let id = if length transactionList == 0 then "1" else show ((read a) + 1)
            let txTyp = if txType == 1 then "Income" else "Expense"
            let newTrx = Transaction { trxId = id, date = date, transactionType = txTyp, amount = amount, description = desc }
            let newList = transactionList ++ [newTrx]
            let (transactionFront, transactionBack) = splitAt index transactions
            let newTransactionList = transactionFront ++ [newList] ++ tail transactionBack

            saveTransactionData (nam ++ ".txt") newList
            putStrLn "\nTransaction added!"
            logToFile ("A new " ++ txTyp ++ " is added to wallet " ++ nam ++ " with the amount of " ++ show amount ++ " " ++ cur)
            
            showWalletMenu index (user, wallets, newTransactionList)

        2 -> do
            putStrAndFlush "Transaction ID: "
            txId <- getNumeric "Transaction ID: " (0, 0)
            let findTrx = findTransactionById 0 (show txId) transactionList

            case findTrx of
                Nothing -> do
                    putStrLn "\nTransaction not found!"
                    logToFile ("Failed to find a transaction with ID " ++ show txId ++ " in wallet " ++ nam ++ " to edit")
                    showWalletMenu index (user, wallets, transactions)
                Just trxIndex -> do
                    printTransactionTitle
                    showTransaction (transactionList !! trxIndex)
                    printLine 60

                    putStrLn "\nWhat to change?"
                    putStrLn "1. Date         2. Type     3. Amount\n4. Description  5. Cancel"
                    putStrAndFlush "Input: "
                    change <- getNumeric "Input: " (1,5)
                    case change of
                        1 -> do
                            putStrAndFlush "Insert new date (dd/mm/yyyy): "
                            update <- getLine

                            let newTrans = updateTransaction 1 update (transactionList !! trxIndex)
                            let (transactionListFront, transactionListBack) = splitAt trxIndex transactionList
                            let newTransactionList = transactionListFront ++ [newTrans] ++ tail transactionListBack

                            let (allTransactionFront, allTransactionBack) = splitAt index transactions
                            let newAllTransaction = allTransactionFront ++ [newTransactionList] ++ tail allTransactionBack

                            saveTransactionData (nam ++ ".txt") newTransactionList
                            logToFile ("Updated transaction " ++ show txId ++ " in wallet " ++ nam ++ " with a new date " ++ update)
                            putStrLn "\nTransaction updated!"
                            showWalletMenu index (user, wallets, newAllTransaction)
                        2 -> do
                            putStrAndFlush "Is it\n1. Income    2. Expense\nInput: "
                            update <- getNumeric "Input: " (1,2)

                            let txType = if update == 1 then "Income" else "Expense"
                            let newTrans = updateTransaction 2 txType (transactionList !! trxIndex)
                            let (transactionListFront, transactionListBack) = splitAt trxIndex transactionList
                            let newTransactionList = transactionListFront ++ [newTrans] ++ tail transactionListBack

                            let (allTransactionFront, allTransactionBack) = splitAt index transactions
                            let newAllTransaction = allTransactionFront ++ [newTransactionList] ++ tail allTransactionBack

                            saveTransactionData (nam ++ ".txt") newTransactionList
                            logToFile ("Updated transaction " ++ show txId ++ " in wallet " ++ nam ++ " to become an " ++ txType)
                            putStrLn "\nTransaction updated!"
                            showWalletMenu index (user, wallets, newAllTransaction)
                        3 -> do
                            putStrAndFlush "Insert new amount: "
                            update <- getDouble "Insert new amount: "

                            let newTrans = updateTransaction 3 (show update) (transactionList !! trxIndex)
                            let (transactionListFront, transactionListBack) = splitAt trxIndex transactionList
                            let newTransactionList = transactionListFront ++ [newTrans] ++ tail transactionListBack

                            let (allTransactionFront, allTransactionBack) = splitAt index transactions
                            let newAllTransaction = allTransactionFront ++ [newTransactionList] ++ tail allTransactionBack

                            saveTransactionData (nam ++ ".txt") newTransactionList
                            logToFile ("Updated transaction " ++ show txId ++ " in wallet " ++ nam ++ " with a new amount of " ++ show update)
                            putStrLn "\nTransaction updated!"
                            showWalletMenu index (user, wallets, newAllTransaction)
                        4 -> do
                            putStrAndFlush "Insert new description: "
                            update <- getLine

                            let newTrans = updateTransaction 4 update (transactionList !! trxIndex)
                            let (transactionListFront, transactionListBack) = splitAt trxIndex transactionList
                            let newTransactionList = transactionListFront ++ [newTrans] ++ tail transactionListBack

                            let (allTransactionFront, allTransactionBack) = splitAt index transactions
                            let newAllTransaction = allTransactionFront ++ [newTransactionList] ++ tail allTransactionBack

                            saveTransactionData (nam ++ ".txt") newTransactionList
                            logToFile ("Updated transaction " ++ show txId ++ " in wallet " ++ nam ++ " with a new description: " ++ update)
                            putStrLn "\nTransaction updated!"
                            showWalletMenu index (user, wallets, newAllTransaction)
                        _ -> showWalletMenu index (user, wallets, transactions)

        3 -> do
            putStrAndFlush "Transaction ID: "
            txId <- getNumeric "Transaction ID: " (0, 0)
            let findTrx = findTransactionById 0 (show txId) transactionList

            case findTrx of
                Nothing -> do
                    logToFile ("Failed to find a transaction with ID " ++ show txId ++ " in wallet " ++ nam ++ " to delete")
                    putStrLn "\nTransaction not found!"
                    showWalletMenu index (user, wallets, transactions)
                Just trxIndex -> do
                    let (transactionFront, transactionBack) = splitAt trxIndex transactionList
                    let newTransactionList = transactionFront ++ tail transactionBack

                    let (allTransactionFront, allTransactionBack) = splitAt index transactions
                    let newAllTransaction = allTransactionFront ++ [newTransactionList] ++ tail allTransactionBack

                    saveTransactionData (nam ++ ".txt") newTransactionList
                    logToFile ("Deleted transaction " ++ show txId ++ " in wallet " ++ nam)
                    putStrLn "\nTransaction deleted!"
                    showWalletMenu index (user, wallets, newAllTransaction)

        4 -> do
            if length transactionList > 0 then do
                logToFile ("Viewed all transactions in wallet " ++ nam)
                printTransactionTitle
                showTransactions transactionList
            else
                putStrLn "\nNo transactions in this wallet"

            putStrAndFlush "\nPress enter to continue..."
            wait <- getLine
            showWalletMenu index (user, wallets, transactions)
        5 -> do
            putStrLn "Filter by:"
            putStrLn "1. ID       2. Date         3. Type\n4. Amount   5. Description  6. Cancel"
            putStrAndFlush "Input: "
            choice <- getNumeric "Input: " (1,6)

            case choice of
                1 -> do
                    putStrAndFlush "Filter by ID: "
                    filter <- getLine
                    filterTransaction choice filter transactionList

                    logToFile ("Filtered transaction with ID " ++ show filter ++ " in wallet " ++ nam)
                    putStrAndFlush "\nPress enter to continue..."
                    wait <- getLine
                    showWalletMenu index (user, wallets, transactions) 
                2 -> do
                    putStrAndFlush "Filter by date: "
                    filter <- getLine
                    filterTransaction choice filter transactionList

                    logToFile ("Filtered transaction with date " ++ filter ++ " in wallet " ++ nam)
                    putStrAndFlush "\nPress enter to continue..."
                    wait <- getLine
                    showWalletMenu index (user, wallets, transactions) 
                3 -> do
                    putStrAndFlush "1. Income  2. Expense\nInput: "
                    filter <- getNumeric "Input: " (1,2)
                    filterTransaction choice (if filter == 1 then "Income" else "Expense") transactionList

                    logToFile ("Filtered transaction with type of " ++ (if filter == 1 then "Income" else "Expense") ++ " in wallet " ++ nam)
                    putStrAndFlush "\nPress enter to continue..."
                    wait <- getLine
                    showWalletMenu index (user, wallets, transactions) 
                4 -> do
                    putStrAndFlush "Filter by amount: "
                    filter <- getDouble "Filter by amount: "
                    filterTransaction choice (show filter) transactionList

                    logToFile ("Filtered transaction with amount of " ++ show filter ++ " in wallet " ++ nam)
                    putStrAndFlush "\nPress enter to continue..."
                    wait <- getLine
                    showWalletMenu index (user, wallets, transactions)
                5 -> do
                    putStrAndFlush "Filter by description: "
                    filter <- getLine
                    filterTransaction choice filter transactionList

                    logToFile ("Filtered transaction containing description " ++ filter ++ " in wallet " ++ nam)
                    putStrAndFlush "\nPress enter to continue..."
                    wait <- getLine
                    showWalletMenu index (user, wallets, transactions) 
                _ -> showWalletMenu index (user, wallets, transactions) 

        _ -> showMainMenu (user, wallets, transactions)

main :: IO ()
main = do
    userFileExist <- isUserFileExist
    if userFileExist
        then do
            user <- loadUser
            wallets <- loadWallet
            transactions <- sequence (loadWalletDetail wallets)
            authenticate (user, wallets, transactions)
        else firstRegister
