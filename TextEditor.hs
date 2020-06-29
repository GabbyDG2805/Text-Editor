--CMP2092M Programming Paradigms Assignment by Gabriella Di Gregorio 15624188, April 2019

--In Haskell, lists are a homogenous data structure. It stores several elements of the same type. (Learnyouahaskell.com, 2011)
--Data.List must be imported in order to perform operations on lists.
--System.IO is the standard IO library and this must also be used to input and output to the console as well as other files.
import Data.List
import System.IO

--The code operates on one variable only which will be the text/sentence input into the editor
--This is initiated as four strings, one for the left of the cursor, one for the right, one for selected text, and one for text that has been copied.
data Text = Text {left :: String, right :: String, selection :: String, clipboard :: String} deriving (Show)

--This function simply sets the four strings to be empty to begin with. These can be changed and operated upon.
create :: Text
create = Text [] [] [] []

--This function creates a more readable output to the console to make it clearer what each of the other functions do.
--It prints the contents of the left string, a cursor (|), and the contents of the right hand side.
display :: Text -> IO()
display t = putStrLn ((left t) ++ "|" ++ (right t))

--This is another function for the inteface, but this one clearly shows the user what is currently selected.
--I decided to have two different functions for this since it looked either untidy or unclear if this function was used every time, even when no selection has been made.
displaySelection :: Text -> IO()
displaySelection t = putStrLn ((left t) ++ "|" ++ (right t) ++ "\t\tYou have selected: " ++ (selection t))

--This function moves the cursor one to the left, so it will appear in front of the previous character
--This behaves the same as pressing the left arrow key would on popular text editors
--Firstly it checks if you are able to move left, because if you are already at the start of the sentence, this means the left string is currently empty so no more moves can be made to the left.
--If you are already leftmost of the sentence, it will simply display the same again rather than throwing an error.
--However, you are able to keep moving to the left until you reach the start, so more of the text will be put into the right string as you move along.
--This is done by keeping all of the left except the last character in the left string, and then concatenating the very last character of the left with right in the right string.
moveLeft :: Text -> Text
moveLeft t = 
 if (left t) == []
  then t
 else (Text (init (left t)) ([last (left t)] ++ (right t)) [] [])

--This function behaves very similarly to the previous one, except this one is for moving one character to the right.
--So this time, there must be contents in the right string in order to be able to move to the right. This is why it first checks if the right string is empty.
--Again, if this string is empty it will simply display the same again rather than throwing an error.
--You can keep moving to the right until the end of the text (when all of the text will be in the left string) as one character will be added to the left string with each right move.
--This is done by combining the contents of the left with the first character of the right in the left string, and putting the remainder of the right string (all but the first character) into the right string.
moveRight :: Text -> Text
moveRight t =
 if (right t) == []
  then t 
 else (Text ((left t) ++ [(head (right t))]) (tail (right t)) [] [])

--The next function allows the user to move the cursor to the start of the line of text.
--This is done by placing all of the text into the right string and emptying the left.
--However, if the left string is already empty then the cursor is already at the start so this move cannot be made. If this is the case, the text is displayed in its current state rather than throwing an error.
moveStart :: Text -> Text
moveStart t = 
 if (left t) == []
  then t
 else (Text [] ((left t) ++ (right t)) [] [])

--Similar to the function above, this one moves the cursor to the other end of the sentence.
--This is done by combining the contents of the left and right into the left string and emptying the right string.
--If the right string is already empty then the cursor is already at the end so this function does not need to do anything, it simply prevents an error.
moveEnd :: Text -> Text
moveEnd t =
 if (right t) == []
  then t
 else (Text ((left t) ++ (right t)) [] [] [])

--This function selects all of the text left of the cursor, which means anything in the left string.
--Whether the cursor is currently at the end of the sentence, in the middle, or after the first word, anywhere it is will select what is left of it.
--This is done by copying the contents of the left string into the selection string.
--However, if the left string is empty then the cursor is at the start of the text so there is nothing in the left string. This makes the function unable to operate so an error check takes place.
selectLeft :: Text -> Text
selectLeft t = 
 if (left t) == []
  then t
 else (Text [] ((left t) ++ (right t)) (left t) [])

--This function is the same idea as the previous one, except it selects everything to the right of the cursor/in the right string
--If this function is displayed, the contents of the right string would be shown in the selection string.
--This time, if the right string is empty then there will be nothing to select, so the if statement prevents an error by returning the text in its current state.
selectRight :: Text -> Text
selectRight t = 
 if (right t) == []
  then t
 else (Text ((left t) ++ (right t)) [] (right t) [])

--The copy and paste functions make use of the fourth string. I called it 'clipboard' as this is what it is refered to by a very well-known text-editor, Microsoft Word
--A copy can only occur if something has been selected so this is checked in the if statement. It sees whether the selection string is empty and if it is it simply returns the text instead of allowing an error.
--If there is something in the selection string, this can be copied by also placing its contents into the clipboard string.
copy :: Text -> Text
copy t =
 if (selection t) == []
  then t
 else (Text (left t) (right t) (selection t) (selection t))

--For a paste to work, it relies on the copy funtion being used first.
--So, it checks if the clipboard string is empty. If it is then nothing has been copied yet, but instead of throwing an error it just returns the text.
--If there is something inside the clipboard string, then this can be pasted by copying its contents into the left string (before the cursor)
paste :: Text -> Text
paste t = 
 if (clipboard t) == []
  then t
 else (Text ((left t) ++ (clipboard t)) (right t) [] (clipboard t))

--This is a very simple function since it simply concatenates the contents of the left and right strings into the selection string, so all of the text will be selected.
selectAllText :: Text -> Text
selectAllText t = Text (left t) (right t) ((left t) ++ (right t)) []

--This function puts one character to the left only into the selection string.
--Similar to the other left functions, it must see if the left string is currently empty because if it is then there are no characters on the left to be selected. This would return an error without this check in place.
--This is done fairly similarly to the moveLeft function in the way that all but the last character of the left is kept in the left string, then the very last character of the left is merged with the right into the right string.
--A selection is made by putting the very last character of the left into the selection string, as well as any other selection previously made as several characters may be selected one after another.
selectLeftLetter :: Text -> Text
selectLeftLetter t = 
 if (left t) == []
  then t
 else (Text (init (left t)) ([last (left t)] ++ (right t)) ([last (left t)] ++ (selection t)) [])

--This function combines the logic of the function above with that of the moveRight function. It selects a single character to the right of the cursor.
--As with the other right functions, it must check whether the right string is empty so it can avoid an error if it is. If it is empty then no characters would be on the right to select.
--Like moveRight, it puts the left and the very first character of the right into the left string, and keeps the rest of the right in the right string.
--The first character of the right is stored in the selection after any others that have already been selected.
selectRightLetter :: Text -> Text
selectRightLetter t = 
 if (right t) == []
  then t
 else (Text ((left t) ++ [(head (right t))]) (tail (right t)) ((selection t) ++ [(head (right t))]) [])

--This function jumps the cursor to the beginning of a word on the left
--Like the rest of the functions, the left must not be empty for this to be able to happen.
--Another if-else statement is needed if the left is not empty in order to check whether the last character in the left is a blank space (" ") instead of a character as this would indicate the start/end of a word.
--If it is already " " then it can be returned since it is already in between words
--However, this is a recursive function since it will keep looping through, moving one to the left until a blank space has been reached.
moveFrontOfWordLeft :: Text -> Text
moveFrontOfWordLeft t =
 if (left t) == []
  then t
 else
  if ([last (left t)]) == " "
   then t
  else moveFrontOfWordLeft (moveLeft t)

--This is a similar idea to the previous function, but moves to the beginning of a word on the right
--The usual check for the empty right string is made
--This time, the next check is to see whether the first character on the right is an empty space (" ") to indicate the start/end of a word
--If it is, it is not quite as simple as the previous function since another moveRight needs to be made to move the cursor to the start of the word on the right rather the end of the word on the left
--This time, the recursion loops through moving one to the right until the " " is found
moveFrontOfWordRight :: Text -> Text
moveFrontOfWordRight t =
 if (right t) == []
  then t
 else
  if ([(head (right t))]) == " "
   then (moveRight t)
  else moveFrontOfWordRight (moveRight t)

--This function is almost identical to the moveFrontOfWordLeft function
--However, this time instead of looping through moveLeft, it repeats selectLeftLetter until all characters up to a " " have been selected.
--The operation of this function was the main purpose of adding previously selected characters into the selection in selectLeftLetter so that it could be used to select a word rather than emptying after each selected character.
selectWordLeft :: Text -> Text
selectWordLeft t =
 if (left t) == []
  then t
 else
  if ([last (left t)]) == " " 
   then t
  else selectWordLeft (selectLeftLetter t)

--This function is almost identical to moveFrontOfWordRight with the same logic as the function above.
--For this one to work, the function is repeated, looping through selectRightLetter until a " " is reached.
selectWordRight :: Text -> Text
selectWordRight t =
 if (right t) == []
  then t
 else
  if ([(head (right t))]) == " "
   then t
  else selectWordRight (selectRightLetter t)

--This function deletes all of the contents of the left string (deletes all text left of the cursor)
--It does this by setting the left string to empty ([])
deleteLeft :: Text -> Text
deleteLeft t = Text [] (right t) [] []

--This one is similar, but deletes all right of the cursor by emptying the right string.
deleteRight :: Text -> Text
deleteRight t = Text (left t) [] [] []

--This function deletes a single character to the left of the cursor, like the backspace button would
--In order for this to work, there must be something on the left to delete so the usual error prevention has been made
--It operates by keeping only the first part of the left (except the very last character) in the left string, and the right in the right string as usual
deleteLeftLetter :: Text -> Text
deleteLeftLetter t = 
 if (left t) == []
  then t
 else (Text (init (left t)) (right t) [] [])

--This is very similar to the function above, but it deletes one character to the right of the cursor just like the Delete key would
--This time, the left remains in the left string, and all of the right string except the very first character remains in the right string
deleteRightLetter :: Text -> Text
deleteRightLetter t = 
 if (right t) == []
  then t
 else (Text (left t) (tail (right t)) [] [])

--This function allows the user to input some text (of any length) where the cursor currently is.
--Since it takes more information, we need to pass in String
--It works by adding an insertion string to the left string
--It is set in the main function that insertion is the user's input by using getLine.
insertInput :: Text -> String -> Text
insertInput t insertion = Text ((left t) ++ (insertion)) (right t) [] []

--This function saves the text (the contents of the left and right strings) into a txt file.
--Since it must take the name of the text file and output, it must take String and IO() as well as the usual Text
--When this function is called in the main or the console, it must be given a file name (in the form of a string) and the text that you would like to save.
--It combines the left and right strings so the whole sentence is saved regardless of cursor location.
save :: String -> Text -> IO()
save nameOfFile t = writeFile nameOfFile ((left t) ++ (right t))


--While a main function is not needed for the program to work, this will display all the functionality I have implemented in a clear and nicely displayed order and output.
--The first part of the video will show main called in GHCi alongside this code, so you can pause the video here to see what each line/function does.
--However, I will also prove that the main is not needed by demonstrating a few functions on a newly input text.
main :: IO()
main = do
--Put contents into the left string:
 let t = Text "Gabriella Di Gregorio 15624188" "" "" ""
 display t
--Save the text to a .txt file:
 save "text.txt" t
 putStrLn "Your file has been saved!"
--Move the cursor to the start of the sentence:
 let h = moveStart t
 display h
--Select the single character to the right of the cursor:
 let srl = selectRightLetter h
 displaySelection srl
--Move the cursor to the end of the sentence:
 let e = moveEnd srl
 display e
--Move the cursor one character to the left:
 let l = moveLeft e
 display l
--Move the cursor another character to the left:
 let ll = moveLeft l
 display ll
--Select the single character on the left-hand side of the cursor:
 let slc = selectLeftLetter ll
 displaySelection slc
--Copy the current selection:
 let c = copy slc
--Paste the selected character left of the cursor:
 let p = paste c
 display p
--Delete one character to the left of the cursor (the one that had just been pasted):
 let dlc = deleteLeftLetter p
 display dlc
--Move the cursor one character to the right:
 let r = moveRight dlc
 display r
--Select all text on the right-hand side of the cursor:
 let sr = selectRight r
 displaySelection sr
--Move in front of the word to the left:
 let lw = moveFrontOfWordLeft sr
 display lw
--Select everything to the left of the cursor:
 let sl = selectLeft lw
 displaySelection sl
--Move to the front of the next word on the right:
 let rw = moveFrontOfWordRight sl
 display rw
--Delete everything on the left-hand side of the cursor:
 let dl = deleteLeft rw
 display dl
--Prompts the user to input text then inserts it to the left of the cursor:
 putStr "Please type what you would like to add: "
 insertion <- getLine
 let i = insertInput dl insertion
 display i
--Select the next word on the right:
 let srw = selectWordRight i
 displaySelection srw
--Select all text, regardless of cursor position:
 let sa = selectAllText srw
 displaySelection sa
--Delete one character to the right of the cursor:
 let drl = deleteRightLetter sa
 display drl
--Delete everything on the right-hand side of the cursor:
 let dr = deleteRight drl
 display dr
--Select the next word on the left of th cursor:
 let slw = selectWordLeft dr
 displaySelection slw

--References:
--Learnyouahaskell.com. (2011). Starting Out - Learn You a Haskell for Great Good!. [online] 
--Available at: http://learnyouahaskell.com/starting-out [Accessed 24 Apr. 2019].
--    This online book was also used to help me learn the basics of Haskell and write my code.