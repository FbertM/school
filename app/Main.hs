module Main where


import Control.Exception
import System.IO
import System.Directory
import Text.Read (readMaybe)
import Data.Char (toLower)
import Data.List (delete, findIndex)
import Data.Time
import Control.Monad.Trans.Writer ( WriterT, tell, runWriterT )
import Control.Monad.IO.Class ( liftIO )
import Control.DeepSeq

data Pelajaran = Pel {namaPelajaran :: String } 
                | NoPel
                deriving (Eq,Show )
data Murid = Murid {namaMurid :: String, kelas :: String , umur :: Int , nik :: Int } | NoMurid deriving (Eq,Show )
data Persentase = Persentase { namaPeriod:: String , persentase:: Int} | NoPers deriving (Eq,Show )
data Nilai = Nilai {murid:: String, mapel :: String, period:: String, nilai :: Int, pers :: Int} deriving (Eq,Show )
data Raport = Raport {namaMuridRaport ::String, mapelRaport :: String, jumlah:: Int,nilaiRaport::Int} deriving (Show,Eq)

askTheString :: String -> (Int,Int) -> IO String
askTheString question (mini,maxi)= do 
    putStr question
    hFlush stdout
    ans <- getLine
    if (length ans < mini || length ans > maxi) then do
        putStr ("Minimum Panjang String " ++  show mini ++ " dan Maximal :" ++ show maxi)
        ans <- askTheString question (mini,maxi)
        return ans
        else
            return ans

askTheInteger :: String -> IO Int 
askTheInteger question = do 
    putStr question
    hFlush stdout
    ans <- getLine
    if((readMaybe ans :: Maybe Int) == Nothing) then do
        putStr "Tolong Input berupa numeric"
        ans <- askTheInteger question
        return ans
    else
        return (read ans :: Int)

pkExistNilai :: [Nilai] -> Nilai -> Bool
pkExistNilai [] y = False
pkExistNilai (x:xs) y
    | getNama(x) == getNama(y) && getMapel x == getMapel y && getPeriod x == getPeriod y = True
    | otherwise = pkExistNilai xs y
    where getNama(Nilai {murid = x}) =  x
          getMapel(Nilai {mapel =x })= x
          getPeriod(Nilai {period= x}) = x

inputNilai :: [Murid] -> [Pelajaran] -> [Persentase] -> [Nilai] -> WriterT [String] IO [Nilai]
inputNilai murid pel pers nilai = do 
    liftIO $ viewMurid murid
    noMurids <- liftIO $ askTheInteger "Input Number of the Chosen Student :"
    
    liftIO $ viewPelajaran pel
    noPel <- liftIO $ askTheInteger "Input Number of the Chosen Subject :"
    
    liftIO $ viewPersentase pers
    nopers <- liftIO $ askTheInteger "Input Number of the Chosen Activity :"
    score <- liftIO $ askTheInteger "Nilai :"
    let murid = naRid ( murid' noMurids )
    let mapel =  naPel ( pel' noPel )
    let (pers,persen) =  naPers ( pers' nopers)
    tell ["Chooose Student :  " ++ murid]
    tell ["Choose Subject  " ++ mapel]
    tell ["Choose Subject Activity " ++ pers]
    tell ["Score : " ++ show score]
    tell ["Percentage Subject Activity of "++ pers ++ " is : " ++ show persen]
    let tmpNilai = Nilai {murid = murid , mapel = mapel, period = pers , pers = persen, nilai = score}
    if pkExistNilai nilai tmpNilai == True 
        then do 
            tell ["Input already exist with The Primary Key of Student, Subject and Student Activity"]
            return nilai
        else do 
            tell ["Input Examination succedd"]
            return (nilai ++ [tmpNilai])
    where naPel (Pel {namaPelajaran = x}) = x
          naRid (Murid {namaMurid = x}) = x
          naPers (Persentase {namaPeriod =x,persentase = y}) =(x,y) 
          murid' noMurids = murid !! (noMurids -1)
          pel' noPel = pel !! (noPel -1)
          pers' nopers =pers !! (nopers -1)

pkExistMurid :: [Murid] -> Murid -> Bool
pkExistMurid [] y = False
pkExistMurid (x:xs) y
    | getNama(x) == getNama(y) && getKelas x == getKelas y && getNik x == getNik y = True
    | otherwise = pkExistMurid xs y
    where getNama(Murid{namaMurid = x}) = fmap toLower x
          getKelas(Murid{kelas =x })= x
          getNik(Murid{nik= x}) = x

inputMurid :: [Murid] -> WriterT [String] IO [Murid]
inputMurid murid = do 
    namaMurid <- liftIO $ askTheString "Student Name :" (2,25)
    kelas <- liftIO $ askTheString "Grade :" (2,6)
    nik <- liftIO $ askTheInteger "NIK :" 
    umur <- liftIO $ askTheInteger "Age : "
    tell ["Student Name : " ++ namaMurid]
    tell ["Grade : " ++ kelas]
    tell ["NIK : " ++ show nik]
    tell ["Umur :" ++ show umur]
    let tmpMurid =   Murid {namaMurid = namaMurid , kelas = kelas, nik = nik , umur = umur}
    if pkExistMurid murid tmpMurid == True
        then do 
            tell ["Input already existed"]
            return murid
        else do 
            tell ["Input Succedd"]
            return (murid ++ [tmpMurid])

pkExistPersentase :: [Persentase] -> Persentase -> Bool
pkExistPersentase [] y = False
pkExistPersentase (x:xs) y
    | getNama(x) == getNama(y)  = True
    | otherwise = pkExistPersentase xs y
    where getNama(Persentase {namaPeriod = x}) = fmap toLower x

inputPersentase :: [Persentase] ->WriterT [String] IO [Persentase]
inputPersentase pers = do 
    tmp <- liftIO $ askTheString "Activity of Subject :" (2,20)
    persen <- liftIO $ askTheInteger "Percentage Activity in (Integer) (%) : "
    tell ["Activity of Subject : " ++ tmp]
    tell ["Percentage Activity " ++ tmp ++ " is : " ++ show persen]
    let tmpPersen = (Persentase {namaPeriod = tmp, persentase = persen })
    if  pkExistPersentase pers tmpPersen == True
        then do 
            tell ["Input already Exist of The Activity Subject"]
            return pers
        else do 
            tell ["Input Succedd"]
            return (pers ++ [tmpPersen])

pkExistPelajaran :: [Pelajaran] -> Pelajaran -> Bool
pkExistPelajaran [] y = False
pkExistPelajaran (x:xs) y
    | getNama(x) == getNama(y)  = True
    | otherwise = pkExistPelajaran xs y
    where getNama(Pel {namaPelajaran = x}) = fmap toLower x

inputPelajaran :: [Pelajaran] ->WriterT [String] IO [Pelajaran]
inputPelajaran pel = do 
    tmp <- liftIO $ askTheString "Input Subject Name :" (2,20)
    tell ["Subject Name : " ++ tmp]
    let tmpPelajaran = (Pel {namaPelajaran = tmp})
    if ((pkExistPelajaran pel tmpPelajaran) == True)
        then do 
            tell ["Input Subject Name "++ tmp ++ " already exist"]
            return pel
        else do 
            tell ["Input Subject succedd"]
            return (pel ++ [tmpPelajaran])

makeString :: String -> [(String , Int)] -> String 
makeString str [] = str
makeString str (x:xs)  = do 
    let tmpStr = fst x
    let tmpInt = concat(replicate ((snd x)-length(tmpStr)) " ")
    makeString (str ++ tmpStr ++ tmpInt) xs

viewPel :: Integer -> [Pelajaran] -> IO ()
viewPel no [] = putStr ""
viewPel no (Pel{namaPelajaran = x}:xs) = do
    putStrLn (makeString "" [(show no,6),(x,35)])
    viewPel (no+1) xs

viewPelajaran :: [Pelajaran] -> IO ()
viewPelajaran pel = do 
    putStrLn (makeString "" [("No",5) ,("|",1),("Mata Pelajaran ",20)])
    putStrLn $ concat (replicate 26 "-")
    viewPel 1 pel
    putStrLn $ concat (replicate 26 "_")

viewPers :: Integer -> [Persentase] -> IO ()
viewPers no [] = putStr ""
viewPers no (Persentase{namaPeriod = x,persentase = y}:xs) = do
    putStrLn (makeString "" [(show no,6),(x,21),(show y,20)])
    viewPers (no+1) xs

viewPersentase :: [Persentase] -> IO ()
viewPersentase pers = do 
    putStrLn (makeString "" [("No",5) ,("|",1),("Period ",20),("|",1),("Persentase (%)",20)])
    putStrLn $ concat (replicate 47 "-")
    viewPers 1 pers
    putStrLn $ concat (replicate 47 "_")

viewNil :: Integer -> [Nilai] -> IO ()
viewNil no [] = putStr ""
viewNil no (Nilai{murid = x,mapel = y,period = z,nilai = w}:xs) = do
    putStrLn (makeString "" [(show no,6),(x,21),(y,21),(z,21),(show w,6)])
    viewNil (no+1) xs

viewNilai :: [Nilai] -> IO ()
viewNilai nilai = do 
    putStrLn (makeString "" [("No",5) ,("|",1),("Name ",20),("|",1),("Subject",20),("|",1),("Activity",20),("|",1),("Score2",5)])
    putStrLn $ concat (replicate 85 "-")
    viewNil 1 nilai
    putStrLn $ concat (replicate 85 "_")


viewMur :: Integer -> [Murid] -> IO ()
viewMur no [] = putStr ""
viewMur no (Murid{namaMurid = x,nik = y,kelas = z,umur = w}:xs) = do
    putStrLn (makeString "" [(show no,6),(x,26),(show y,16),(z,7),(show w,5)])
    viewMur (no+1) xs

viewMurid :: [Murid] -> IO ()
viewMurid murid = do 
    putStrLn (makeString "" [("No",5) ,("|",1),("Name ",25),("|",1),("NIK",15),("|",1),("Grade",6),("|",1),("Age",5)])
    putStrLn $ concat (replicate 60 "-")
    viewMur 1 murid
    putStrLn $ concat (replicate 60 "_")

existPelajaran :: [Nilai] -> String -> Bool
existPelajaran [] _ = False
existPelajaran (Nilai {mapel = x}:xs) findOut  
    | findOut == x = True
    | otherwise = existPelajaran xs findOut 

existPeriod :: [Nilai] -> String -> Bool
existPeriod [] _ = False
existPeriod (Nilai {period = x}:xs) findOut  
    | findOut == x = True 
    | otherwise =  existPeriod xs findOut 

existMurid :: [Nilai] -> String -> Bool
existMurid [] _ = False
existMurid (Nilai {murid = x}:xs) findOut  
    | findOut == x = True 
    | otherwise =  existMurid xs findOut  

updateGloba ::(Eq a)=> a -> a -> [a] -> [a] -> [a]
updateGloba _ _ pers [] = pers
updateGloba upd find startPel (x : xs)  = do
    if x == find then startPel ++ [upd] ++ xs
     else updateGloba upd find (startPel ++ [x]) xs


extractPelajaran :: Pelajaran -> [String]
extractPelajaran Pel {namaPelajaran = x }  = [x]

extractPelajarans :: [Pelajaran] -> [String]
extractPelajarans [] = []
extractPelajarans (x:xs) = extractPelajaran x ++ extractPelajarans xs


writes :: FilePath -> String -> IO ()
writes x y = do
    writeFile x y

askPelajaran :: [Nilai] -> [Pelajaran] -> IO ()
askPelajaran nilai pel = do 
    let pathFile = "mapel.txt"
    putStrLn $ concat(replicate 30 "-")
    putStrLn $ makeString "" [("",3),("Subject",14),("",3)]
    putStrLn $ concat(replicate 30 "-")
    putStrLn "1. Input Subject"
    putStrLn "2. Update Subject"
    putStrLn "3. Delete Subject"
    putStrLn "4. View Subject"
    putStrLn "5. Exit"
    quest <- getLine
    case quest of 
        "1" -> do
            (tmp,log) <- runWriterT(inputPelajaran pel)
            writeFile pathFile $ unlines $ (extractPelajarans tmp)
            appendLogs log
            askPelajaran nilai tmp
        "2" -> do 
            viewPelajaran pel
            no <- askTheInteger "Please Input Number to be Updated :"
            if (no < 1 || no > length pel) then do
                putStrLn "The Number you Input not exist"
                askPelajaran nilai pel
                else do 
                    let find = pel !! (no - 1) 
                    if existPelajaran nilai (naPel find) == True then do
                        putStrLn "Subject can't be changed because it already existed inside the Examination Student"
                        askPelajaran nilai pel
                        else do
                            askPel <- askTheString "Updated Subject :" (2,20)
                            let tmpSubject = Pel {namaPelajaran =askPel }
                            let tmp =  (updateGloba tmpSubject  find [] pel)
                            writeFile pathFile $ unlines $ (extractPelajarans tmp)
                            appendLog $ " : Update Subject From " ++ show find ++ " To be " ++ show tmpSubject 
                            askPelajaran nilai tmp
                    where naPel (Pel {namaPelajaran = x}) = x
        "3" -> do 
            viewPelajaran pel
            no <- askTheInteger "Please Input Number to be Deleted :"
            if (no < 1 || no > length pel) then do
                putStrLn "The Number you Input not exist"
                askPelajaran nilai pel
                else do 
                    let find = pel !! (no - 1) 
                    if existPelajaran nilai (naPel find) == True then do
                        putStrLn "Subject can't be deleted because it already existed inside the Examination Student"
                        askPelajaran nilai pel
                        else do
                            let tmp = (delete find pel)
                            writeFile pathFile $ unlines $ (extractPelajarans tmp)
                            appendLog $ " : Subject " ++ show find ++ " is deleted " 
                            askPelajaran nilai tmp
                    where naPel (Pel {namaPelajaran = x}) = x
        "4" -> do
            viewPelajaran pel
            askPelajaran nilai pel
        "5" -> return ()
        _  -> do
            putStrLn "Input the existed Number"
            askPelajaran nilai pel

extractPersen :: Persentase -> [String]
extractPersen Persentase { namaPeriod=x , persentase=y}  = [x, show y]

extractPersens :: [Persentase] -> [String]
extractPersens [] = []
extractPersens x = concat(fmap extractPersen x)

askPersentase :: [Nilai] -> [Persentase] -> IO ()
askPersentase nilai pers = do 
    let pathFile = "pers.txt"
    putStrLn $ concat(replicate 30 "-")
    putStrLn $ makeString "" [("",10),("Activity of Subject",10),("",10)]
    putStrLn $ concat(replicate 30 "-")
    putStrLn "1. Input Activity Subject"
    putStrLn "2. Update Activity Subject"
    putStrLn "3. Delete Activity Subject"
    putStrLn "4. View Activity Subject"
    putStrLn "5. Exit"
    quest <- getLine
    case quest of 
        "1" -> do
            (tmp,log) <- runWriterT (inputPersentase pers)
            writeFile pathFile $ unlines $ (extractPersens tmp)
            appendLogs log
            askPersentase nilai tmp
        "2" -> do 
            viewPersentase pers
            no <- askTheInteger "Please Input Number to be Updated :"
            if (no < 1 || no > length pers) then do
                putStrLn "The Number you Input not exist"
                askPersentase nilai pers
                else do 
                    let find = pers !! (no - 1) 
                    persValue <- askTheInteger "Percentage updated to be : "
                    let tmp = (updateGloba (Persentase{namaPeriod = (naPel find) , persentase = persValue }) find [] pers)
                    writeFile pathFile $ unlines $ (extractPersens tmp)
                    appendLog $ " : Update Activity Subject From " ++ show find ++ " To be " ++ show tmp 
                    askPersentase nilai tmp
                    where naPel (Persentase {namaPeriod = x}) = x
        "3" -> do 
            viewPersentase pers
            no <- askTheInteger "Please Input Number to be deleted :"
            if (no < 1 || no > length pers) then do
                putStrLn "The Number you Input not exist"
                askPersentase nilai pers
                else do 
                    let find = pers !! (no - 1) 
                    if existPeriod nilai (naPel find) == True then do
                        putStrLn "Activity can't be deleted because it's already inside the Examination Student"
                        askPersentase nilai pers
                        else do 
                            let tmp = (delete find pers)
                            writeFile pathFile $ unlines $ (extractPersens tmp)
                            appendLog $ " : Activity Subject " ++ show find ++ " is deleted " 
                            askPersentase nilai tmp
                    where naPel (Persentase {namaPeriod = x}) = x
        "4" -> do 
            viewPersentase pers
            askPersentase nilai pers
        "5" -> return ()
        _  -> putStrLn "Input the existed Number"


extractMurid :: Murid -> [String]
extractMurid Murid {namaMurid =x, kelas =y , umur= z , nik = w }   = [x, y, show z , show w]

extractMurids :: [Murid] -> [String]
extractMurids [] = []
extractMurids x = concat (fmap extractMurid x)

appendLogs :: [String] -> IO()
appendLogs [] = return ()
appendLogs (x:xs) = do
    appendLog (" : " ++ x)
    appendLogs xs

appendLog :: String -> IO ()
appendLog str = do 
    current <- curTime
    appendFile "log.txt" (show current ++ str ++ "\n")

askMurid :: [Nilai] -> [Murid] -> IO ()
askMurid nilai murid = do 
    let pathFile = "murid.txt"
    putStrLn $ concat(replicate 30 "-")
    putStrLn $ makeString "" [("",10),("Murid",10),("",10)]
    putStrLn $ concat(replicate 30 "-")
    putStrLn "1. Input Student Detail"
    putStrLn "2. Update Student Detail"
    putStrLn "3. Delete Student Detail"
    putStrLn "4. View Student Detail"
    putStrLn "5. Exit"
    quest <- getLine
    case quest of 
        "1" -> do
            (tmp,log) <- runWriterT(inputMurid murid)
            writeFile pathFile $ unlines $ (extractMurids tmp)
            appendLogs log
            askMurid nilai tmp
        "2" -> do 
            viewMurid murid
            no <- askTheInteger "Please Input Number to be Updated :"
            if (no < 1 || no > length murid) then do
                putStrLn "The Number you Input not exist"
                askMurid nilai murid
                else do 
                    let find = murid !! (no - 1) 
                    kelas <- askTheString "Grade Updated :" (2,5)
                    nik <- askTheInteger "NIK Updated :"
                    umur <- askTheInteger "Age Updated :"
                    let clas 
                         | kelas == "" = naKel find
                         | otherwise = kelas
                    let tmpMurid = (Murid{namaMurid = naPel find, kelas = clas, nik = nik, umur =umur})
                    let tmp =(updateGloba tmpMurid find [] murid)
                    writeFile pathFile $ unlines $ (extractMurids tmp)
                    appendLog $ " : Update Murid From " ++ show find ++ " To be " ++ show tmpMurid 
                    askMurid nilai tmp
                    where naPel (Murid {namaMurid = x}) = x
                          naKel (Murid {kelas =x }) = x
                          naNik (Murid {nik =x}) = x
        "3" -> do 
            viewMurid murid
            no <- askTheInteger "Please Input Number to be deleted :"
            if (no < 1 || no > length murid) then do
                putStrLn "The Number you Input not exist"
                askMurid nilai murid
                else do 
                    let find = murid !! (no - 1) 
                    if existMurid nilai (naPel find) == True then do
                        putStrLn "Student can't be delted because it is already used in Examination Student"
                        askMurid nilai murid
                        else do
                            let tmp = (delete find murid)
                            writeFile pathFile $ unlines $ (extractMurids tmp)
                            askMurid nilai tmp
                            appendLog $ " : Murid " ++ show find ++ " is deleted " 
                    where naPel (Murid {namaMurid = x}) = x
        "4" -> do 
            viewMurid murid
            askMurid nilai murid
        "5" -> return ()
        _  -> putStrLn "Please Input the existed Number"

findRaport :: [Raport] -> [Raport] -> String -> String -> Int -> [Raport]
findRaport [] tmp murid mapel score= tmp ++ [Raport {namaMuridRaport = murid, mapelRaport = mapel,jumlah =  1 ,nilaiRaport = score}]
findRaport (tempRaport@(Raport {namaMuridRaport = x, mapelRaport = y,jumlah = z ,nilaiRaport = w}): xs) tmp murid mapel score
    | (murid == x && mapel == y) = (tmp ++ [Raport {namaMuridRaport = x, mapelRaport = y,jumlah = z + 1 ,nilaiRaport = w + score}] ++ xs)
    | otherwise = findRaport xs (tmp ++ [tempRaport]) murid mapel score

groupByNilai :: [Nilai] -> [Raport] -> IO [Raport]
groupByNilai [] y = return y
groupByNilai (x:xs) y = do 
    let (a,b,c,d,e) = getRaport x
    let score = d* e `div` 100
    groupByNilai xs (findRaport y [] a b score)
    where getRaport (Nilai {murid = a, mapel = b, period = c, nilai = d, pers = e}) = (a,b,c,d,e)

viewRap :: Integer -> [Raport] -> IO ()
viewRap no [] = putStr ""
viewRap no (Raport {namaMuridRaport =x, mapelRaport =y, jumlah=z,nilaiRaport=w} :xs) = do
    putStrLn (makeString "" [(show no,6),(x,26),(y,21),(show w,7),(show z,6)])
    viewRap (no+1) xs

viewRaps :: [Raport] -> IO ()
viewRaps murid = do 
    putStrLn (makeString "" [("No",5) ,("|",1),("Nama ",25),("|",1),("Subject",20),("|",1),("Score",6),("|",1),("#Num of Act. Subj",6)])
    putStrLn $ concat (replicate 66 "-")
    viewRap 1 murid
    putStrLn $ concat (replicate 66 "_")

viewRaport :: [Nilai] -> IO ()
viewRaport nilai = do 
    raportNilai <- groupByNilai nilai []
    viewRaps raportNilai

viewRapDet :: Integer -> [Raport] -> IO ()
viewRapDet no [] = putStr ""
viewRapDet no (Raport {namaMuridRaport =x, mapelRaport =y, jumlah=z,nilaiRaport=w} :xs) = do
    let score = w `div` z
    putStrLn (makeString "" [(show no,6),(x,26),(show score,6)])
    viewRapDet (no+1) xs

viewRapDets :: [Raport] -> IO ()
viewRapDets murid = do 
    putStrLn (makeString "" [("No",5) ,("|",1),("Name ",25),("|",1),("Score",6)])
    putStrLn $ concat (replicate 40 "-")
    viewRapDet 1 murid
    putStrLn $ concat (replicate 40 "_")

viewDetRaport :: [Nilai] -> IO ()
viewDetRaport nilai = do 
    raportNilai <- groupByNilai nilai []
    raportDet <- groupByNilaiDet raportNilai []
    viewRapDets raportDet



findRaportDetail :: [Raport] -> [Raport] -> String -> Int -> [Raport]
findRaportDetail [] tmp murid score= tmp ++ [Raport {namaMuridRaport = murid, mapelRaport = "",jumlah =  1 ,nilaiRaport = score}]
findRaportDetail (tempRaport@(Raport {namaMuridRaport = x, mapelRaport = y,jumlah = z ,nilaiRaport = w}): xs) tmp murid score
    | murid == x  = (tmp ++ [Raport {namaMuridRaport = x, mapelRaport = "",jumlah = z + 1 ,nilaiRaport = w + score}] ++ xs)
    | otherwise = findRaportDetail xs (tmp ++ [tempRaport]) murid score

groupByNilaiDet :: [Raport] -> [Raport] -> IO [Raport]
groupByNilaiDet [] y = return y 
groupByNilaiDet (x:xs) y = do 
    let (a,b) = getRaport x
    groupByNilaiDet xs (findRaportDetail y [] a b)
    where getRaport (Raport {namaMuridRaport =a,nilaiRaport = b }) = (a,b)

extractNilai :: Nilai -> [String]
extractNilai Nilai {murid=x, mapel=y, period=z, nilai=w, pers=v}   = [x, y,z, show w, show v]

extractNilais :: [Nilai] -> [String]
extractNilais [] = []
extractNilais x = concat $ fmap extractNilai x

askNilai :: [Nilai] -> [Murid] -> [Pelajaran] -> [Persentase] -> IO ()
askNilai nilai murid pel pers = do 
    let pathFile = "nilai.txt"
    putStrLn $ concat(replicate 30 "-")
    putStrLn $ makeString "" [("",10),("Examination Student",15),("",10)]
    putStrLn $ concat(replicate 30 "-")
    putStrLn "1. Input The Examination Student"
    putStrLn "2. Update The Examination Student"
    putStrLn "3. Delete The Examination Student"
    putStrLn "4. View The Examination Student"
    putStrLn "5. Exit"
    quest <- getLine
    case quest of 
        "1" -> do
            (tmp,log) <- runWriterT (inputNilai murid pel pers nilai)
            writeFile pathFile $ unlines $ (extractNilais tmp)
            appendLogs log
            askNilai tmp murid pel pers
        "2" -> do 
            viewNilai nilai
            no <- askTheInteger "Please Input the Number to be Updated :"
            if (no < 1 || no > length nilai) then do
                putStrLn "The Number you input not exist"
                askNilai nilai murid pel pers
                else do 
                    let find = nilai !! (no - 1) 
                    score <- askTheInteger "Score  Updated :"
                    
                    let (mur,map,per,persen) = naPel find
                    let tmpNilai = (Nilai {murid = mur, mapel = map, period = per, nilai = score, pers = persen})
                    let tmp = (updateGloba tmpNilai find [] nilai)
                    writeFile pathFile $ unlines $ (extractNilais tmp)
                    appendLog $ " : Update Examination Student From " ++ show find ++ " To be " ++ show tmpNilai 
                    askNilai tmp murid pel pers
                    where naPel (Nilai {murid = x, mapel = y, period = z, pers = w}) = (x,y,z,w)
        "3" -> do 
            viewNilai nilai
            no <- askTheInteger "Please Input the Number to be Deleted :"
            if (no < 1 || no > length nilai) then do
                putStrLn "The Number you input not exist"
                askNilai nilai murid pel pers
                else do 
                    let find = nilai !! (no - 1) 
                    let tmp = (delete find nilai)
                    writeFile pathFile $ unlines $ (extractNilais tmp)
                    appendLog $ " : Examination Student " ++ show find ++ " is deleted " 
                    askNilai tmp murid pel pers
                    where naPel (Nilai {murid = x, mapel = y, period = z, pers = w}) = (x,y,z,w)
        "4" -> do 
            viewNilai nilai
            askNilai nilai murid pel pers
        "5" -> return ()
        _  -> do 
            putStrLn "Please Input the existed Number"
            askNilai nilai murid pel pers

importMapel :: String  -> Pelajaran
importMapel x = Pel {namaPelajaran = x }

importMapels :: [String] -> [Pelajaran]
importMapels [] = []
importMapels (x:xs) = [importMapel x ] ++ importMapels xs

importMurid :: String -> String -> String -> String -> Murid
importMurid x y z w= Murid {namaMurid = x, kelas = y  , umur = read z  , nik = read w } 

importMurids :: [String] -> [Murid]
importMurids [] = []
importMurids (x:y:z:w:xs) = [importMurid x y z w ] ++ importMurids xs

importPersen :: String -> String  -> Persentase
importPersen x y= Persentase { namaPeriod = x , persentase = read y}  

importPersens :: [String] -> [Persentase]
importPersens [] = []
importPersens (x:y:xs) = [importPersen x y] ++ importPersens xs

importNilai :: String -> String -> String -> String -> String -> Nilai
importNilai x y z w v= Nilai {murid = x, mapel = y , period =z , nilai = read w, pers = read v} 

importNilais :: [String] -> [Nilai]
importNilais [] = []
importNilais (x:y:z:w:v:xs) = [importNilai x y z w v ] ++ importNilais xs

getFileContentOrElse :: String -> FilePath -> IO String
getFileContentOrElse def filePath = readFile filePath `catch`
    \e -> const (return def) (e :: IOException)

removeFileCurrent :: String -> IO ()
removeFileCurrent str = do 
    x <- doesFileExist str
    if x == True
        then removeFile str
        else return ()

curTime :: IO String
curTime = do 
    curTime <- getZonedTime
    return $ show curTime

main :: IO ()
main = do 
    tmpFile <- getFileContentOrElse "" "mapel.txt"
    evaluate $ force tmpFile
    let pel = importMapels $ lines tmpFile

    tmpFile <- getFileContentOrElse "" "murid.txt"
    evaluate $ force tmpFile
    let murid = importMurids $ lines tmpFile


    tmpFile <- getFileContentOrElse "" "pers.txt"
    evaluate $ force tmpFile
    let pers = importPersens $ lines tmpFile

    tmpFile <- getFileContentOrElse "" "nilai.txt"
    evaluate $ force tmpFile
    let nilai = importNilais $ lines tmpFile

    -- need to load pelajaran , periode, persentase , nilai from the file
    putStrLn $ concat(replicate 30 "-")
    putStrLn $ makeString "" [("",10),("Raport Activity",10),("",10)]
    putStrLn $ concat(replicate 30 "-")
    putStrLn "1. Subject "
    putStrLn "2. Activity Subject (UTS , UAS, etc)"
    putStrLn "3. Student Detail"
    putStrLn "4. Examination Student"
    putStrLn "5. View Raport All Student Summary"
    putStrLn "6. View Raport by Detail Student"
    putStrLn "7. Exit"
    index <- getLine
    case index of 
        "1" -> do
            askPelajaran nilai pel
            -- removeFileCurrent "mapel.txt"
            -- renameFile "mapels.txt" "mapel.txt"
        "2" -> do
            askPersentase nilai pers
            -- removeFileCurrent "pers.txt"
            -- renameFile "perss.txt" "pers.txt"
        "3" -> do 
            askMurid nilai murid
            -- removeFileCurrent "murid.txt"
            -- renameFile "murids.txt" "murid.txt"
        "4" -> do 
            askNilai nilai murid pel pers
            -- removeFileCurrent "nilai.txt"
            -- renameFile "nilais.txt" "nilai.txt"
        "5" -> viewRaport nilai
        "6" -> viewDetRaport nilai
        "7" -> putStr ""
        _ -> main
    if index /= "7" then 
        main 
        else
            putStr ""

