import System.Random
import System.IO
import Data.List.Split
import Control.Monad

welcome = "--------------------------------------------\n"
    ++ "| #  #   #   #   #  #### #   #   #   #   # |\n"
    ++ "| #  #  # #  ##  # #     ## ##  # #  ##  # |\n"
    ++ "| #### ##### # # # #  ## # # # ##### # # # |\n"
    ++ "| #  # #   # #  ## #   # #   # #   # #  ## |\n"
    ++ "| #  # #   # #   #  ###  #   # #   # #   # |\n"
    ++ "--------------------------------------------\n\n"
    ++ "Welcome to the game Hangman!\n\n"
    ++ "The objective in this game is to guess the word. \n"
    ++ "You can enter both uppercase and lowercase letters.\n"
    ++ "If you think you know the word, you can type it in.\n"
    ++ "You will lose if you have guessed 10 letters wrong.\n\n"

   
hang0 =  "Amount of wrong letters: 0\n\n"
    ++ "\n"
    ++ "\n"
    ++ "\n"
    ++ "\n"
    ++ "\n"
    ++ "\n"
    ++ "____________\n\n"

hang1 = "Amount of wrong letters: 1\n\n"
    ++ "\n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "__|_________\n\n"

hang2 = "Amount of wrong letters: 2\n\n"
    ++ "  _______\n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "__|_________\n\n"

hang3 = "Amount of wrong letters: 3\n\n"
    ++ "  _______\n"
    ++ "  |/\n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "__|_________\n\n"

hang4 = "Amount of wrong letters: 4\n\n"
    ++ "  _______\n"
    ++ "  |/   | \n"
    ++ "  |    O \n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "  |\n"
    ++ "__|_________\n\n"

hang5 = "Amount of wrong letters: 5\n\n"
    ++ "  _______\n"
    ++ "  |/   | \n"
    ++ "  |    O \n"
    ++ "  |    |\n"
    ++ "  |    |\n"
    ++ "  |\n"
    ++ "__|_________\n\n"

hang6 = "Amount of wrong letters: 6\n\n"
    ++ "  _______\n"
    ++ "  |/   | \n"
    ++ "  |    O \n"
    ++ "  |   \\|\n"
    ++ "  |    | \n"
    ++ "  |\n"
    ++ "__|_________\n\n"

hang7 = "Amount of wrong letters: 7\n\n"
    ++ "  _______\n"
    ++ "  |/   | \n"
    ++ "  |    O \n"
    ++ "  |   \\|/\n"
    ++ "  |    | \n"
    ++ "  |\n"
    ++ "__|_________\n\n"

hang8 = "Amount of wrong letters: 8\n\n"
    ++ "  _______\n"
    ++ "  |/   | \n"
    ++ "  |    O \n"
    ++ "  |   \\|/\n"
    ++ "  |    | \n"
    ++ "  |   /\n"
    ++ "__|_________\n\n"

hang9 = "Amount of wrong letters: 9\n\n"
    ++ "  _______\n"
    ++ "  |/   | \n"
    ++ "  |    O \n"
    ++ "  |   \\|/\n"
    ++ "  |    | \n"
    ++ "  |   / \\\n"
    ++ "__|_________\n\n"

hang10 = "Amount of wrong letters: 10\n\n"
    ++ "  _______\n"
    ++ "  |/   | \n"
    ++ "  |    X \n"
    ++ "  |   \\|/\n"
    ++ "  |    | \n"
    ++ "  |   / \\\n"
    ++ "__|_________\n\n"


getGuillotine wrongs
    | wrongs == 0 = hang0
    | wrongs == 1 = hang1
    | wrongs == 2 = hang2
    | wrongs == 3 = hang3
    | wrongs == 4 = hang4
    | wrongs == 5 = hang5
    | wrongs == 6 = hang6
    | wrongs == 7 = hang7
    | wrongs == 8 = hang8
    | wrongs == 9 = hang9
    | otherwise   = hang10

getWordsFromFile file = splitOn "|" file
     
trimWhitespace [] = []
trimWhitespace (' ': xs) = trimWhitespace xs
trimWhitespace ('\n': xs) = trimWhitespace xs
trimWhitespace (x:xs) = x : trimWhitespace xs


getDotWord [] _ _= "" 
getDotWord (x:xs) y zs =
    if x == y  || elem x zs then x:getDotWord xs y zs
    else '.':getDotWord xs y zs

processGuess [] dotWord _ wrongs turn  = return (wrongs, turn, dotWord)
processGuess (x:xs) dotWord gameWord wrongs turn = 
    if elem x gameWord then do
        putStr ("The letter " ++ [x] ++ " was correct.\n\n")
        let wrongs' = wrongs
        let turn' = turn + 1
        let dotWord' = getDotWord gameWord x dotWord 
        putStr ("The word including letters you guessed: " ++ dotWord' ++ "\n\n")
        putStr (getGuillotine wrongs')
        when (xs /= []) $ putStr(show turn' ++ ".\n")
        if dotWord' == gameWord then return (wrongs, 0, dotWord)
        else processGuess xs dotWord' gameWord wrongs' turn'
    else do
        putStr ("The letter " ++ [x] ++ " was incorrect. \n\n")
        let wrongs' = wrongs + 1
        let turn' = turn + 1
        let dotWord' = getDotWord gameWord x dotWord
        putStr ("The word including letters you guessed: " ++ dotWord' ++ "\n\n")
        putStr (getGuillotine wrongs')
        if wrongs' == 10 then return (wrongs', turn', dotWord')
        else do
            when (xs /= []) $ putStr(show turn' ++ ".\n")
            processGuess xs dotWord' gameWord wrongs' turn'


play gameWord dotWord wrongs turn = do
    putStr (show turn ++ ".   Enter the letter(s) you want to guess: ")
    guess <- getLine
    (wrongs', turn', dotWord') <-processGuess guess dotWord gameWord wrongs turn
    if wrongs' >= 10 || turn' <= 0 then do
        putStr "---------------\n--- Results ---\n---------------\n\n"
        if wrongs' >= 10 then do
            putStr ("You guessed the wrong word. The word was " ++ gameWord ++ ". Better luck next time!\n")
        else do
            putStr "Congratulations you guessed the right word!\n"
    else play gameWord dotWord' wrongs' turn'



main = do
    hSetBuffering stdout NoBuffering
    putStr welcome
    let wordFile = "words.txt"
    fileString <- readFile wordFile
    let words = getWordsFromFile fileString
    let wordLen = length words 
    wordIndex <-randomRIO(0,wordLen-1)
    let gameWord = trimWhitespace(words !! wordIndex)
    let dotWord = getDotWord gameWord '.' ""
    putStr ("This is the word you need to guess: " ++ dotWord ++ "\n\n")
    play gameWord dotWord 0 1
