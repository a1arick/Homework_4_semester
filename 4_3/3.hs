import System.IO
import Data.List 

data Person = Person String String

instance Show Person where
    show (Person name number) = name ++ " " ++ number 

data Phonebook = Phonebook [Person]

instance Show Phonebook where
    show (Phonebook []) = " "
    show (Phonebook (x:xs)) = show (x) ++ "\n" ++ show (Phonebook xs) 

addPerson :: Person -> Phonebook -> Phonebook
addPerson person (Phonebook book) = Phonebook (person : book)

--let daa = Phonebook [(Person "aa" "11"), (Person "bb" "22"),(Person "cc" "33")]

findByName :: String -> Phonebook -> Maybe String
findByName name (Phonebook []) = Nothing
findByName name (Phonebook ((Person tempName tempNumber) : xs)) | name == tempName    = Just tempNumber
								| otherwise           = findByName name (Phonebook xs)

findByNumber :: String -> Phonebook -> Maybe String
findByNumber number (Phonebook []) = Nothing
findByNumber number (Phonebook ((Person tempName tempNumber) : xs)) | number == tempNumber    = Just tempName
								    | otherwise               = findByNumber number (Phonebook xs)
saveToFile :: Phonebook -> String -> IO ()
saveToFile phonebook file = do writeFile file (show phonebook)

loadFromFile :: String -> IO (Phonebook)
loadFromFile nameFile = do
	file <- readFile nameFile
	let temp = words file
	return (Phonebook (parseFunction temp  []))

parseFunction :: [String] -> [Person] -> [Person]
parseFunction [] xs  = reverse xs
parseFunction (name:number:s) xs  = parseFunction s ((Person name number) : xs)
    

help :: IO ()                
help = do
	putStrLn "Comands:"
	putStrLn "'0' - exit"
	putStrLn "'1' 'name number' - add person in phonebook"
	putStrLn "'2' 'name' - find number by name"
	putStrLn "'3' 'number' - find name by number"
	putStrLn "'4' 'fileName' - save phonebook to file"
	putStrLn "'5' 'fileName' - load phonebook from file"
	putStrLn "'show' - show data in phonebook"



phonebook :: IO ()
phonebook = do help
               comands (Phonebook [])

comands :: Phonebook -> IO ()
comands phonebook = do
	command <- getLine
	case (words command) of
		"0":[] ->
            		putStrLn "exit"
		"1":name:number:[] -> do
            		comands (addPerson (Person name number) phonebook)
			putStrLn "ok"
        	"2":name:[] -> do
            		let maybeValue = findByName name phonebook
			case maybeValue of
				Just value -> do
					putStrLn value
				Nothing -> do
					putStrLn "Nothing"
            		comands phonebook
        	"3":number:[] -> do
            		let maybeValue = findByNumber number phonebook
			case maybeValue of
				Just value -> do
					putStrLn value
				Nothing -> do
					putStrLn "Nothing"
            		comands phonebook
		"4":nameFile:[] -> do
			saveToFile phonebook nameFile
            		comands phonebook
        	"5":nameFile:[] -> do
           		newPhonebook <- loadFromFile nameFile
			comands newPhonebook
		"show":[] -> do
			putStrLn (show (phonebook))
			comands phonebook
        	
       

	
    
    
		


    

