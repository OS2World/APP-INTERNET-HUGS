-- This file contains the example program from section 7.7 of the Haskell
-- report (version 1.1) for a program using synchronisation.

main :: Dialogue
main  = readChan stdin abort (\userInput -> readNums (lines userInput))

readNums           :: [String] -> Dialogue
readNums inputLines = readIntP "Enter first number: " inputLines
                        (\num1 inputLines1 ->
                          readIntP "Enter second number: " inputLines1
                            (\num2 _ -> reportResult num1 num2))

reportResult       :: Int -> Int -> Dialogue
reportResult num1 num2
  = appendChan stdout ("Their sum is: "++ show (num1 + num2)) abort done
                                  

-- readIntP prints a prompt and then reads a line of input.  If the
-- line contains an integer, the value of the integer is passed to the
-- success continuation.  If a line cannot be parsed as an integer,
-- an error message is printed and the user is asked to try again.
-- If EOF is detected, the program is aborted.

readIntP :: String -> [String] -> (Int -> [String] -> Dialogue) -> Dialogue
readIntP prompt inputLines succ
  = appendChan stdout prompt abort
      (case inputLines of
         (l1 : rest) -> case (reads l1) of
                          [(n,"")] -> succ n rest
                          _        -> appendChan stdout
                                       "Error - retype the number\n" abort
                                       (readIntP prompt rest succ)
         _           -> appendChan stdout "Early EOF" abort done)

