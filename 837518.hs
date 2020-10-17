-- MATHFUN
-- Functional Programming Coursework
-- Student Number: UP837518

import Data.Char
import Data.List

-- Types
type Title = String
type Artist = String
type Year = Int
type Sales = Int

type Album = (Title, Artist, Year, Sales)

type Database = [Album]
testDatabase :: [Album]
testDatabase = [("Greatest Hits","Queen",1981,6300000),
                ("Gold: Greatest Hits","ABBA", 1992,5400000),
                ("Sgt. Pepper'sle Lonely Hearts Club Band","The Beatles",1967,5340000),
                ("21","Adele",2011,5110000),
                ("(What'sle the Story), Morning Glory?","Oasis",1995,4940000),
                ("Thriller","Michael Jackson",  1982,4470000),
                ("The Dark Side of the Moon","Pink Floyd", 1973,4470000),
                ("Brothers in Arms","Dire Straits", 1985,4350000),
                ("Bad","Michael Jackson",  1987,4140000),
                ("Rumours","Fleetwood Mac",1977,4090000),
                ("Greatest Hits II","Queen",1991,3990000),
                ("Back to Black","Amy Winehouse",2006,3940000),
                ("The Immaculate Collection","Madonna",1990,3700000),
                ("25","Adele",2015,3500000),
                ("Stars","Simply Red", 1991,3450000),
                ("Come On Over", "Shania Twain", 1998,3430000),
                ("x","Ed Sheeran", 2014,3380000),
                ("Legend", "Bob Marley", 1984,3380000),
                ("Bat Out of Hell","Meat Loaf",  1977,3370000),
                ("Back to Bedlam","James Blunt",2004,3360000),
                ("Urban Hymns","The Verve",  1997,3340000),
                ("Bridge over Troubled Water","Simon & Garfunkel",1970,3260000),
                ("1","The Beatles",2000,3230000),
                ("Spirit", "Leona Lewis",2007,3170000),
                ("Crazy Love","Michael Bublé",2009,3130000),
                ("No Angel","Dido", 2000,3090000),
                ("White Ladder", "David Gray", 1998,3020000),
                ("The Fame","Lady Gaga",  2009,2990000),
                ("Only by the Night","Kings of Leon",2008,2980000),
                ("A Rush of Blood to the Head",  "Coldplay",2002,2960000),
                ("Talk on Corners","The Corrs",  1997,2960000),
                ("Spice","Spice Girls",1996,2960000),
                ("Life for Rent","Dido", 2003,2900000),
                ("Beautiful World","Take That",  2006,2880000),
                ("The Joshua Tree","U2",1987,2880000),
                ("Hopes and Fears","Keane",2004,2860000),
                ("The War of the Worlds",  "Jeff Wayne", 1978,2800000),
                ("X&Y","Coldplay",2005,2790000),
                ("Jagged Little Pill", "Alanis Morissette",1995,2780000),
                ("Tubular Bells","Mike Oldfield",1973,2760000),
                ("Scissor Sisters","Scissor Sisters",  2004,2760000),
                ("...But Seriously","Phil Collins", 1989,2750000),
                ("Tracy Chapman","Tracy Chapman",1988,2710000),
                ("Parachutes","Coldplay",2000,2710000),
                ("The Man Who","Travis", 1999,2687500),
                ("Greatest Hits","ABBA", 1975,2606000),
                ("I've Been Expecting You","Robbie Williams",  1998,2586500),
                ("Come Away with Me","Norah Jones",2002,2556650),
                ("Graceland","Paul Simon", 1986,2500000),
                ("Ladies & Gentlemen: The Best of","George Michael",1998,2500000)]

--Helper Functions used in some of the demos
--funtion which validates and checks whether two albums are the same or not
checkAlbumSimilar :: String -> Album -> Bool
checkAlbumSimilar title (titl, art, yr, sle)
 | title == titl = True
 | otherwise = False

--funtion which returns, from function checkAlbumSimilar, the same albums
searchForAlbum :: String -> Database -> Database
searchForAlbum title database = filter (checkAlbumSimilar title) database

--funtion that also check and validates like checkAlbumSimilar, but for two artists being the same or not
checkArtistSimilar :: String -> Album -> Bool
checkArtistSimilar artist (titl,art,yr,sle)
 | artist == art = True
 | otherwise = False

--funtion which returns, from function checkArtistSimilar, the same artist
searchForArtist :: String -> Database -> Database
searchForArtist artist database = filter (checkArtistSimilar artist) database

--Demo 1
--funtion displaying the output of the database on each line, one by one in art list 
outputAlbum :: Album -> String
outputAlbum (title, artist, year, sales) =  "Title: " ++ title ++ "     Artist: " ++ artist ++ "     Year: " ++ show (year) ++ "     Sales: " ++ show (sales) ++"\n"

--funtion Converting the list into art single string
albumToStr :: Database -> String
albumToStr database = concat(map outputAlbum database)

--funtion which calls itself to output the contents of the database
printAlbums :: Database -> String
printAlbums database = concat(map outputAlbum database)

--Demo 2
--A function checking two albums and displays less than or greater than depending on the number of sales for the given albums..
maxSales (titlOne, artOne, yrOne, sleOne) (titlTwo, artTwo, yrTwo, sleTwo)
 |(sleOne) <= (sleTwo) = GT
 |(sleOne) > (sleTwo) = LT

--A function which outputs the overall top 10 Albums based on sales in descending order
outputTopTen :: Database -> String
outputTopTen database = albumToStr(take 10 (sortBy maxSales database))

--Demo 3
--A funtion that looks for albums released inbetween two given years
albumReleaseYear :: Year -> Year -> Album -> Bool
albumReleaseYear startingYear endingYear (titl, art, yr, sle)
 | (yr >= startingYear && yr <= endingYear) = True
 | otherwise = False

--A function which outputs all albums realeased between two given years
outputAlbumDates :: Year -> Year -> Database -> String
outputAlbumDates startingYear endingYear database = albumToStr (filter (albumReleaseYear startingYear endingYear) (database))

--Demo 4
--A function to check all album names that begin with the given prefix by the user.
prefixFilter :: [Char] -> Album -> Bool
prefixFilter prefix (titl, art, yr, sle)
 | isPrefixOf prefix titl = True
 | otherwise = False

--A function providing all albums from an artist with the given prefix 
outputPrefix :: [Char] -> Database -> String
outputPrefix prefix database = albumToStr (filter (prefixFilter prefix) database)

--Demo 5
--A function which calculates the total amount of sales for the current artist
totalSales :: String -> Database -> Int
totalSales albumArtist [] = 0
totalSales albumArtist ((titl, art, yr, sle) : xs)
 |(albumArtist == art) = sle + totalSales albumArtist xs
 |otherwise = totalSales albumArtist xs

--Demo 6
--A function taking an artist and art from the database, which then outputs the amount of times an artist shows in the database
countFrequencyOfAlbumCharts :: Artist -> Database -> Int
countFrequencyOfAlbumCharts artist [] = 0
countFrequencyOfAlbumCharts artist ((_, art, _, _):titl) = (if artist == art then 1 else 0) + (countFrequencyOfAlbumCharts artist titl)

--A function using list comprehension for all elements in the database, to count the appearances in the top 50 which are sorted based on sales and removes duplicates.
--'nub' is a element taken from Data.List which removes duplicates and combines them all into one 
countFrequencyTopFifty :: Database -> [(Artist, Int)]
countFrequencyTopFifty database = nub [ (art, countFrequencyOfAlbumCharts art (sortedTopFifty database)) | (_, art, _, _) <- database ]

--A function displaying the amount of times an artist has been displayed from the database.
outputArtistFrequency :: Database -> String
outputArtistFrequency databases = frequencyToString (countFrequencyTopFifty databases)

--A function displaying the name of the artist and the amount of times theyve been included in the top 50.
outputFrequency :: (Artist, Int) -> String
outputFrequency (artist, times)  =  "Artist: " ++ artist ++ " Times: " ++ show (times) ++ "\n"

--A function displaying the amount of times an artist appears, retrieved from the database.
frequencyToString :: [(Artist, Int)] -> String
frequencyToString frequency = concat(map outputFrequency frequency)

--Demo 7
--A function which takes the top 49 elements, sorted by max sales, from the database. 'init' function allows us to take the top 49 elements.
deleteLowestAlbumSales :: Database -> Database
deleteLowestAlbumSales database = init $ take 50 $ sortBy maxSales database

--A function which allows you to add an artist with a list of albums to the artist database and sorts them by max sales.
addNewAlbum :: [Album] -> Database -> Database
addNewAlbum album database = sortBy maxSales (database ++ album)

--A function returning the top 50 albums, sorted by max sales from the database
sortedTopFifty :: Database -> Database
sortedTopFifty database = take 50 (sortBy maxSales database)

--Demo 8
--A function that uses list comprehension to change an 'Album' from the database by updating its sales and sorting it by max sales.
changeAlbumSales :: (Title, Artist, Int) -> Database -> Database
changeAlbumSales (titl, art, sle) database = sortBy maxSales [ if titl == titlOne && art == artOne then (titl, art, yr, sle + sleOne) else album | album@(titlOne, artOne, yr, sleOne) <- database ]

-- ************************************************************************************************************************************************************************************
-- Code for the User interface

demo :: Int -> IO ()
--demo 1  = putStrLn (albumToStr testData)
demo 1 = putStrLn (albumToStr testDatabase)
--demo 2  = putStrLn (albumToStr (top10 testData))
demo 2 = putStrLn (outputTopTen testDatabase)
--demo 3  = putStrLn ( all albums released between 2000 and 2008 inclusive )
demo 3 = putStrLn (outputAlbumDates  2000 2008 testDatabase)
--demo 4  = putStrLn ( all albums with titles beginning with "Th" )
demo 4 = putStrLn (outputPrefix "Th" testDatabase)
--demo 5  = putStrLn ( total sales figure for "Queen" )
demo 5 = print (totalSales "Queen" testDatabase)
--demo 6  = putStrLn ( all artists with the number of times they appear in top 50 )
demo 6 = putStrLn (outputArtistFrequency testDatabase)
--demo 7  = putStrLn ( albums after removing 50th album and adding "Progress"
--                     by "Take That" from 2010 with 2700000 sales )
demo 7 = putStrLn (albumToStr (addNewAlbum [("Progress", "Take That", 2010, 2700000)] (deleteLowestAlbumSales testDatabase)))
--demo 8  = putStrLn ( albums after increasing sales of "21" by "Adele" by 400000 )
demo 8 = putStrLn (albumToStr (changeAlbumSales ("21", "Adele", 400000) testDatabase))

--Code to initiate User Interface on Haskell Platform
main :: IO ()
--Takes all information from the database and reads it from the albums.txt file
main = do tempDatabase <- readFile "albums.txt"
          let database = read tempDatabase
          --Welcome Message when you access user interface
          putStrLn "Welcome to the top50 Albums User Interface! \nPlease enter a username to proceed: "
          --getLine function reads input from the user in this case to enter the username
          username <- getLine
          --User interface hold the username and all information in the database
          database <- userInterface (username, database)
          --Write changes to album.txt file
          writeFile "albums.txt" (show database)
          putStrLn "All changes have been successfully made."

--Outputs the options to choose from
userInterface :: (String, Database) -> IO Database
userInterface (username, database) = do let info = (username, database)
                                        let msgOne = "Input anything to return to main menu: "
                                        putStrLn "***************************************************************************************************"
                                        putStrLn "                                  Top 50 Charts - Albums Database                                  "
                                        putStrLn "***************************************************************************************************"
                                        putStrLn "1. - Output all Albums from Top 50 Charts\n"
                                        putStrLn "2. - Output the top 10 Albums in descending order of max sales\n"
                                        putStrLn "3. - Output all Albums released in the chosen given year\n"
                                        putStrLn "4. - Output all Albums with the titles that begin with any given prefix\n"
                                        putStrLn "5. - Output the total amount of sales for any given artist \n"
                                        putStrLn "6. - Output a list of pairs of artists names with the no. of albums they've got in top 50\n"
                                        putStrLn "7. - Remove the 50th (lowest-selling) album and add (new) album into the list\n"
                                        putStrLn "8. - Change or Increase sales figures for an albums given its title, artist and additional sales\n"
                                        putStrLn "0. - Update the database and Exit application\n"
                                        putStrLn "***************************************************************************************************\n"
                                        putStr "Enter 0 to exit save database & exit application:  \nOr \nEnter any number to complete an action:  "
                                        input <- getLine
                                        if input /= "0"
                                           --Read input from the user and get directed to that selection based on users choice
                                           then case input of
                                                     "1" -> do info <- selection 1 info
                                                               putStr msgOne
                                                               entry <- getLine
                                                               userInterface info
                                                     "2" -> do info <- selection 2 info
                                                               putStr msgOne
                                                               entry <- getLine
                                                               userInterface info
                                                     "3" -> do info <- selection 3 info
                                                               putStr msgOne
                                                               entry <- getLine
                                                               userInterface info
                                                     "4" -> do info <- selection 4 info
                                                               putStr msgOne
                                                               entry <- getLine
                                                               userInterface info
                                                     "5" -> do info <- selection 5 info
                                                               putStr msgOne
                                                               entry <- getLine
                                                               userInterface info
                                                     "6" -> do info <- selection 6 info
                                                               putStr msgOne
                                                               entry <- getLine
                                                               userInterface info
                                                     "7" -> do info <- selection 7 info
                                                               putStr msgOne
                                                               entry <- getLine
                                                               userInterface info
                                                     "8" -> do info <- selection 8 info
                                                               putStr msgOne
                                                               entry <- getLine
                                                               userInterface info
                                                    --Displays error message for incorrect input
                                                     _ -> do putStrLn "Invalid input\nPlease enter a number: "
                                                             userInterface info
                                        else return (snd info)

--Allows user to view information of Demo 1
selection :: Int -> (String, Database) -> IO (String, Database)
selection 1 (username, database) = do putStrLn "************************************************************************************"
                                      putStrLn "                     Output all Albums from Top 50 Charts                           "
                                      putStrLn "************************************************************************************"
                                      putStrLn (printAlbums database)
                                      return (username, database)
--Allows user to view information of Demo 2
selection 2 (username, database) = do putStrLn "************************************************************************************"
                                      putStrLn "            Output the top 10 Albums in descending order of max sales               "
                                      putStrLn "************************************************************************************"
                                      putStrLn (outputTopTen database)
                                      return (username, database)
--Allows user to view information of Demo 3 based on input of starting year and ending year
selection 3 (username, database) = do putStrLn "************************************************************************************"
                                      putStrLn "               Output all Albums released in the chosen given year                  "
                                      putStrLn "************************************************************************************"
                                      putStr "Input the starting year: "
                                      --Takes input for the starting year from the user as an integer
                                      inputOne <- getLine
                                      case reads inputOne :: [(Integer, String)] of
                                           [(n, "")] -> do let startingYear = read inputOne :: Int
                                                           putStr "Input the ending year: "
                                                           --Takes input for the ending year from the user an integer
                                                           inputTwo <- getLine
                                                           case reads inputTwo :: [(Integer, String)] of
                                                                [(n, "")] -> do let endingYear = read inputTwo :: Int
                                                                                --Checks and outputs all Albums between the user inputted starting and ending year from the database
                                                                                let albums = (outputAlbumDates  startingYear endingYear database)
                                                                                if albums == ""
                                                                                   --Displays error message for incorrect input
                                                                                   then do putStrLn "No Albums Returned."
                                                                                           return (username, database)
                                                                                   else do putStrLn "Returned the following Albums."
                                                                                           putStrLn albums
                                                                                           return (username, database)
                                                                        --Displays error message for incorrect input
                                                                _ -> do putStrLn "Invalid Input\nPlease enter a year (Numerically)"
                                                                        return (username, database)
                                                   --Displays error message for incorrect input
                                           _ -> do putStrLn "Invalid Input\nPlease enter a year (Numerically)"
                                                   return (username, database)
--Allows user to view information of Demo 4 by entering a prefix of choice
selection 4 (username, database) = do putStrLn "************************************************************************************"
                                      putStrLn "         Output all Albums with the titles that begin with any given prefix         "
                                      putStrLn "************************************************************************************"
                                      putStr "Enter any prefix of choice to search: "
                                      prefix <- getLine
                                      let albums = (outputPrefix  prefix database)
                                      if albums ==""
                                                --Displays error message for incorrect input
                                        then do putStrLn "No Album exists with the following prefix"
                                                return (username,database)
                                        else do putStrLn "Outputted albums starting with the provided prefix"
                                                putStrLn albums
                                                return (username,database)
--Allows user to view information of Demo 5 by entering the artist to search
selection 5 (username, database) = do putStrLn "************************************************************************************"
                                      putStrLn "               Output the total amount of sales for any given artist                "
                                      putStrLn "************************************************************************************"
                                      putStr "Input Artist you wish to search: "
                                      artist <- getLine
                                      let albums = (totalSales  artist database)
                                      if albums == 0
                                                --Displays error message for incorrect input
                                        then do putStrLn "Artist does not exist in the list"
                                                return (username,database)
                                        else do putStrLn "Total Sales for given Artist provided: "
                                                print albums
                                                return (username,database)
--Allows user to view information of Demo 6
selection 6 (username, database) = do putStrLn "************************************************************************************"
                                      putStrLn "Output a list of pairs of artists names with the no. of albums they've got in top 50"
                                      putStrLn "************************************************************************************"
                                      putStrLn (outputArtistFrequency database)
                                      return (username, database)
--Allows user to view information of Demo 7 by entering the new title they wish to add and the amount of sales for that album
selection 7 (username, database) = do putStrLn "************************************************************************************"
                                      putStrLn "      Remove the 50th (lowest-selling) album and add (new) album into the list      "
                                      putStrLn "************************************************************************************"
                                      putStrLn "Enter title for your new Album"
                                      title <- getLine
                                      if title == ""
                                        then return (username,database)
                                      else do let checkAlbum = searchForAlbum title database
                                              if checkAlbum /= []
                                                        --Displays message to show input already exists
                                                then do putStrLn "Album already exists."
                                                        selection 7 (username, database)
                                                else do putStrLn "Enter the name of the new Artist "
                                                        artist <- getLine
                                                        putStrLn "Enter  the amount of sales for the Album "
                                                        albumSales <- getLine
                                                        let sales = (read albumSales :: Int)
                                                        putStr "Enter the year in which the Album was released(Numerically): "
                                                        releasePeriod <- getLine
                                                        --Reads release time and updates what is read from the user then returns it
                                                        case reads releasePeriod :: [(Integer, String)] of
                                                             [(n, "")] -> do let year = read releasePeriod :: Int
                                                                             let updatedDatabase = (addNewAlbum [(title, artist, year, sales)] (deleteLowestAlbumSales database))
                                                                             print  (printAlbums updatedDatabase)
                                                                             return (username, updatedDatabase)
                                                                     --Displays error message for incorrect input
                                                             _ -> do putStrLn "The year you entered is an invalid number. "
                                                                     selection 7 (username, database)
--Allows user to view information of Demo 8 by entering the title and name of the artist they wish to change and the additional sales made
selection 8 (username, database) = do putStrLn "***************************************************************************************************"
                                      putStrLn "    Change or Increase sales figures for an albums given its title, artist and additional sales    "
                                      putStrLn "***************************************************************************************************"
                                      putStrLn "Enter title of Album you wish to change"
                                      title <- getLine
                                      putStrLn "Entre name of Artist you wish to change"
                                      artist <- getLine
                                      putStrLn "Increasing the amount of sales"
                                      incrementedSales <- getLine
                                      --reads new original sales
                                      let newSales = (read incrementedSales :: Int)
                                      --changes the sales and updates the database
                                      let updatedDatabase  = (changeAlbumSales (title, artist, newSales) database)
                                      --Prints albums from the original database
                                      let databaseOne = printAlbums database
                                      --A new database which has all the changes the user has made in the user interface
                                      let databaseTwo = printAlbums updatedDatabase
                                      --Checks whether databaseOne is equal to databaseTwo.If not, databaseTwo overides databaseOne. If it is, nothing happens.
                                      if databaseOne == databaseTwo
                                                 --Displays error message for incorrect input
                                         then do putStrLn "Invalid Album."
                                                 return (username, database)
                                                 --Displays message to confirm changes made
                                         else do putStrLn "Changes where successfully made."
                                                 return (username, updatedDatabase)
