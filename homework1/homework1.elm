{- Homework Week 1

   **Find last element in a list**

   ```
   > myLast [1,2,3,4]
   Just 4
   ```
-}


module Main exposing
    ( clap
    , compress
    , dropEvery
    , elementAt
    , isPalindrome
    , myButLast
    , myLast
    , myLength
    , myReverse
    )


myLast data =
    data
        |> List.reverse
        |> List.head
myLast data =
    data
        |> List.foldl (Just >> always) Nothing
myLast data =
    data
        |> List.foldl (\x y -> Just x) Nothing



{-
   **Find the last but one element of a list**

   ```
   > myButLast [1,2,3,4]
   Just 3
   ```
-}


myButLast data =
    data
        |> List.reverse
        |> List.drop 1
        |> List.head



{-
   **Find the K'th element of a list**

   ```
   > elementAt [1,2,3] 2
   Just 2
   ```
-}


elementAt data index =
    if List.length data < index then
        Nothing

    else
        data
            |> List.take index
            |> myLast



{-
   **Find the number of elements of a list**

   ```
   > myLength [123, 456, 789]
   3
   ```
-}


myLength list =
    List.length list



{-
   **Reverse a list**

   ```
   > myReverse [1,2,3,4]
   [4,3,2,1]
   ```
-}


myReverse list =
    list |> List.foldl (::) []



{-
   **Find out whether a list is a palindrome**

   ```
   > isPalindrome [1,2,4,8,16,8,4,2,1]
   True
   ```
-}


isPalindrome list =
    if myReverse list == list then
        True

    else
        False



{-
   **Eliminate consecutive duplicates of string elements**

   ```
   > compress "aaaabccaadeeee"
   "abcade"
   ```
-}


compress string =
    string
        |> String.toList
        |> List.map String.fromChar
        |> List.foldl
            (\p w ->
                if List.member p w then
                    w

                else
                    List.append w [ p ]
            )
            []
        |> String.concat



{-
   **Drop every N'th element from a string**

   ```
   > dropEvery "abcdefghik" 3
   "abdeghk"
   ```
-}


dropEvery string element =
    string
        |> String.toList
        |> List.map String.fromChar
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\x ->
                if modBy element (Tuple.first x + 1) /= 0 then
                    Just (Tuple.second x)

                else
                    Nothing
            )
        |> String.concat



{-
   **(optional) Insert the 👏 emoji between words**

   ```
   > clap "learning Elm is fun"
   "learning 👏 Elm 👏 is 👏 fun"
   ```
-}


clap string =
    string |> String.replace " " " 👏 "
