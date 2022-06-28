module Main where


import Control.Exception
import System.IO
import System.Directory
import Text.Read (readMaybe)
import Data.Char (toLower)
import Data.List (delete, findIndex)

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

inputNilai :: [Murid] -> [Pelajaran] -> [Persentase] -> [Nilai] -> IO [Nilai]
inputNilai murid pel pers nilai = do 
    viewMurid murid
    noMurids <- askTheInteger "Pilih No Murid yang ingin dipilih :"
    viewPelajaran pel
    noPel <- askTheInteger "Pilih No Pelajaran yang ingin dipilih :"
    viewPersentase pers
    nopers <- askTheInteger "Pilih No Period yang ingin dipilih :"
    score <- askTheInteger "Nilai :"
    let murid = naRid ( murid' noMurids )
    let mapel = naPel ( pel' noPel )
    let (pers,persen) = naPers ( pers' nopers)
    let tmpNilai = Nilai {murid = murid , mapel = mapel, period = pers , pers = persen, nilai = score}
    if pkExistNilai nilai tmpNilai == True 
        then return nilai
        else return (nilai ++ [tmpNilai])
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

inputMurid :: [Murid] -> IO [Murid]
inputMurid murid = do 
    namaMurid <- askTheString "Nama Murid :" (2,25)
    kelas <- askTheString "Kelas :" (2,6)
    nik <- askTheInteger "NIK :" 
    umur <- askTheInteger "Umur : "
    let tmpMurid =   Murid {namaMurid = namaMurid , kelas = kelas, nik = nik , umur = umur}
    if pkExistMurid murid tmpMurid == True
        then return murid
        else return (murid ++ [tmpMurid])

pkExistPersentase :: [Persentase] -> Persentase -> Bool
pkExistPersentase [] y = False
pkExistPersentase (x:xs) y
    | getNama(x) == getNama(y)  = True
    | otherwise = pkExistPersentase xs y
    where getNama(Persentase {namaPeriod = x}) = fmap toLower x

inputPersentase :: [Persentase] ->IO [Persentase]
inputPersentase pers = do 
    tmp <- askTheString "Period :" (2,20)
    persen <- askTheInteger "Persentase : "
    let tmpPersen = (Persentase {namaPeriod = tmp, persentase = persen })
    if pkExistPersentase pers tmpPersen == True
        then return pers
        else return (pers ++ [tmpPersen])

pkExistPelajaran :: [Pelajaran] -> Pelajaran -> Bool
pkExistPelajaran [] y = False
pkExistPelajaran (x:xs) y
    | getNama(x) == getNama(y)  = True
    | otherwise = pkExistPelajaran xs y
    where getNama(Pel {namaPelajaran = x}) = fmap toLower x

inputPelajaran :: [Pelajaran] ->IO [Pelajaran]
inputPelajaran pel = do 
    tmp <- askTheString "Nama Mata Pelajaran :" (2,20)
    let tmpPelajaran = (Pel {namaPelajaran = tmp})
    if pkExistPelajaran pel tmpPelajaran == True
        then return pel
        else return (pel ++ [tmpPelajaran])

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
    putStrLn (makeString "" [("No",5) ,("|",1),("Nama ",20),("|",1),("Mata Pelajaran",20),("|",1),("Period",20),("|",1),("Nilai",5)])
    putStrLn $ concat (replicate 85 "-")
    viewNil 1 nilai
    putStrLn $ concat (replicate 85 "_")


viewMur :: Integer -> [Murid] -> IO ()
viewMur no [] = putStr ""
viewMur no (Murid{namaMurid = x,nik = y,kelas = z,umur = w}:xs) = do
    putStrLn (makeString "" [(show no,6),(x,21),(show y,26),(z,7),(show w,5)])
    viewMur (no+1) xs

viewMurid :: [Murid] -> IO ()
viewMurid murid = do 
    putStrLn (makeString "" [("No",5) ,("|",1),("Nama ",25),("|",1),("NIK (%)",15),("|",1),("Kelas",6),("|",1),("Umur",5)])
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

updatePel :: String -> Pelajaran -> [Pelajaran] -> [Pelajaran] -> [Pelajaran]
updatePel _ _  pel [] = pel
updatePel upd ask@(Pel {namaPelajaran = x}) startPel (pels@(Pel {namaPelajaran = y}) : xs)  = do
    if x == y then startPel ++ [(Pel {namaPelajaran = upd})] ++ xs
     else updatePel upd ask (startPel ++ [pels]) xs

updatePers :: Persentase -> Persentase -> [Persentase] -> [Persentase] -> [Persentase]
updatePers _ _ pers [] = pers
updatePers upd find startPel (x : xs)  = do
    if x == find then startPel ++ [upd] ++ xs
     else updatePers upd find (startPel ++ [x]) xs

updateMurid :: Murid -> Murid -> [Murid] -> [Murid] -> [Murid]
updateMurid _ _ pers [] = pers
updateMurid upd find startPel (x : xs)  = do
    if x == find then startPel ++ [upd] ++ xs
     else updateMurid upd find (startPel ++ [x]) xs

updateNilai :: Nilai -> Nilai -> [Nilai] -> [Nilai] -> [Nilai]
updateNilai _ _ pers [] = pers
updateNilai upd find startPel (x : xs)  = do
    if x == find then startPel ++ [upd] ++ xs
     else updateNilai upd find (startPel ++ [x]) xs


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
    let pathFile = "mapels.txt"
    putStrLn $ concat(replicate 30 "-")
    putStrLn $ makeString "" [("",3),("Mata Pelajaran",14),("",3)]
    putStrLn $ concat(replicate 30 "-")
    putStrLn "1. Input Mata Pelajaran"
    putStrLn "2. Update Mata Pelajaran"
    putStrLn "3. Delete MAta Pelajaran"
    putStrLn "4. View Mata Pelajaran"
    putStrLn "5. Exit"
    quest <- getLine
    case quest of 
        "1" -> do
            tmp <- (inputPelajaran pel)
            writeFile pathFile $ unlines $ (extractPelajarans tmp)
            askPelajaran nilai tmp
        "2" -> do 
            viewPelajaran pel
            no <- askTheInteger "Input No yang ingin diupdate :"
            if (no < 1 || no > length pel) then 
                putStrLn "No yang diinput tidak ada"
                else do 
                    let find = pel !! (no - 1) 
                    if existPelajaran nilai (naPel find) == True then
                        putStrLn "Tidak bisa diupdate. Sudah ada Nilai yang diinput dari mata pelajaran tersebut"
                        else do
                            askPel <- askTheString "Mata Pelajaran yang ingin di update :" (2,20)
                            let tmp =  (updatePel askPel find [] pel)
                            writeFile pathFile $ unlines $ (extractPelajarans tmp)
                            askPelajaran nilai tmp
                    where naPel (Pel {namaPelajaran = x}) = x
        "3" -> do 
            viewPelajaran pel
            no <- askTheInteger "Input No yang ingin didelete :"
            if (no < 1 || no > length pel) then do
                putStrLn "No yang diinput tidak ada"
                askPelajaran nilai pel
                else do 
                    let find = pel !! (no - 1) 
                    if existPelajaran nilai (naPel find) == True then do
                        putStrLn "Tidak bisa dihapus. Sudah ada Nilai yang diinput dari mata pelajaran tersebut"
                        askPelajaran nilai pel
                        else do
                            let tmp = (delete find pel)
                            writeFile pathFile $ unlines $ (extractPelajarans tmp)
                            askPelajaran nilai tmp
                    where naPel (Pel {namaPelajaran = x}) = x
        "4" -> do
            viewPelajaran pel
            askPelajaran nilai pel
        "5" -> do
            writeFile pathFile $ unlines $ (extractPelajarans pel)
        _  -> do
            putStrLn "Input sesuai dengan yang diarahkan"
            askPelajaran nilai pel

extractPersen :: Persentase -> [String]
extractPersen Persentase { namaPeriod=x , persentase=y}  = [x, show y]

extractPersens :: [Persentase] -> [String]
extractPersens [] = []
extractPersens x = concat(fmap extractPersen x)

askPersentase :: [Nilai] -> [Persentase] -> IO ()
askPersentase nilai pers = do 
    let pathFile = "perss.txt"
    putStrLn $ concat(replicate 30 "-")
    putStrLn $ makeString "" [("",10),("Persentase",10),("",10)]
    putStrLn $ concat(replicate 30 "-")
    putStrLn "1. Input Persentase Period"
    putStrLn "2. Update Persentase Period"
    putStrLn "3. Delete Persentase Period"
    putStrLn "4. View Persentase Period"
    putStrLn "5. Exit"
    quest <- getLine
    case quest of 
        "1" -> do
            tmp <- (inputPersentase pers)
            writeFile pathFile $ unlines $ (extractPersens tmp)
            askPersentase nilai tmp
        "2" -> do 
            viewPersentase pers
            no <- askTheInteger "Input No yang ingin diupdate :"
            if (no < 1 || no > length pers) then 
                putStrLn "No yang diinput tidak ada"
                else do 
                    let find = pers !! (no - 1) 
                    persValue <- askTheInteger "Persentase : "
                    let tmp = (updatePers (Persentase{namaPeriod = (naPel find) , persentase = persValue }) find [] pers)
                    writeFile pathFile $ unlines $ (extractPersens tmp)
                    askPersentase nilai tmp
                    where naPel (Persentase {namaPeriod = x}) = x
        "3" -> do 
            viewPersentase pers
            no <- askTheInteger "Input No yang ingin didelete :"
            if (no < 1 || no > length pers) then do
                putStrLn "No yang diinput tidak ada"
                askPersentase nilai pers
                else do 
                    let find = pers !! (no - 1) 
                    if existPeriod nilai (naPel find) == True then do
                        putStrLn "Tidak bisa dihapus. Sudah ada Nilai yang diinput dari Period tersebut"
                        askPersentase nilai pers
                        else do 
                            let tmp = (delete find pers)
                            writeFile pathFile $ unlines $ (extractPersens tmp)
                            askPersentase nilai tmp
                    where naPel (Persentase {namaPeriod = x}) = x
        "4" -> do 
            print pers
            viewPersentase pers
            askPersentase nilai pers
        "5" -> do 
             writeFile pathFile $ unlines $ (extractPersens pers)
        _  -> putStrLn "Input sesuai dengan yang diarahkan"


extractMurid :: Murid -> [String]
extractMurid Murid {namaMurid =x, kelas =y , umur= z , nik = w }   = [x, y, show z , show w]

extractMurids :: [Murid] -> [String]
extractMurids [] = []
extractMurids x = concat (fmap extractMurid x)

askMurid :: [Nilai] -> [Murid] -> IO ()
askMurid nilai murid = do 
    let pathFile = "murids.txt"
    putStrLn $ concat(replicate 30 "-")
    putStrLn $ makeString "" [("",10),("Murid",10),("",10)]
    putStrLn $ concat(replicate 30 "-")
    putStrLn "1. Input Murid"
    putStrLn "2. Update Murid"
    putStrLn "3. Delete Murid"
    putStrLn "4. View Murid"
    putStrLn "5. Exit"
    quest <- getLine
    case quest of 
        "1" -> do
            tmp <- (inputMurid murid)
            writeFile pathFile $ unlines $ (extractMurids tmp)
            askMurid nilai tmp
        "2" -> do 
            viewMurid murid
            no <- askTheInteger "Input No yang ingin diupdate :"
            if (no < 1 || no > length murid) then 
                putStrLn "No yang diinput tidak ada"
                else do 
                    let find = murid !! (no - 1) 
                    kelas <- askTheString "Kelas :" (2,5)
                    nik <- askTheInteger "NIK :"
                    umur <- askTheInteger "Umur :"
                    let clas 
                         | kelas == "" = naKel find
                         | otherwise = kelas
                    let tmpMurid = (Murid{namaMurid = naPel find, kelas = clas, nik = nik, umur =umur})
                    let tmp =(updateMurid tmpMurid find [] murid)
                    writeFile pathFile $ unlines $ (extractMurids tmp)
                    askMurid nilai tmp
                    where naPel (Murid {namaMurid = x}) = x
                          naKel (Murid {kelas =x }) = x
                          naNik (Murid {nik =x}) = x
        "3" -> do 
            viewMurid murid
            no <- askTheInteger "Input No yang ingin didelete :"
            if (no < 1 || no > length murid) then do
                putStrLn "No yang diinput tidak ada"
                askMurid nilai murid
                else do 
                    let find = murid !! (no - 1) 
                    if existMurid nilai (naPel find) == True then do
                        putStrLn "Tidak bisa dihapus. Sudah ada Nilai yang diinput dari Murid tersebut"
                        askMurid nilai murid
                        else do
                            let tmp = (delete find murid)
                            writeFile pathFile $ unlines $ (extractMurids tmp)
                            askMurid nilai tmp
                    where naPel (Murid {namaMurid = x}) = x
        "4" -> do 
            viewMurid murid
            askMurid nilai murid
        "5" -> writeFile pathFile $ unlines $ (extractMurids murid)
        _  -> putStrLn "Input sesuai dengan yang diarahkan"

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
    putStrLn (makeString "" [("No",5) ,("|",1),("Nama ",25),("|",1),("Mata Pelajaran",20),("|",1),("Nilai",6),("|",1),("#Jumlah",6)])
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
    putStrLn (makeString "" [("No",5) ,("|",1),("Nama ",25),("|",1),("Nilai",6)])
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
    let pathFile = "nilais.txt"
    putStrLn $ concat(replicate 30 "-")
    putStrLn $ makeString "" [("",10),("Nilai",10),("",10)]
    putStrLn $ concat(replicate 30 "-")
    putStrLn "1. Input Nilai"
    putStrLn "2. Update Nilai"
    putStrLn "3. Delete Nilai"
    putStrLn "4. View Nilai"
    putStrLn "5. Exit"
    quest <- getLine
    case quest of 
        "1" -> do
            tmp <- (inputNilai murid pel pers nilai)
            writeFile pathFile $ unlines $ (extractNilais tmp)
            askNilai tmp murid pel pers
        "2" -> do 
            viewNilai nilai
            no <- askTheInteger "Input No yang ingin diupdate :"
            if (no < 1 || no > length nilai) then 
                putStrLn "No yang diinput tidak ada"
                else do 
                    let find = nilai !! (no - 1) 
                    score <- askTheInteger "Nilai :"
                    
                    let (mur,map,per,persen) = naPel find
                    let tmpNilai = (Nilai {murid = mur, mapel = map, period = per, nilai = score, pers = persen})
                    let tmp = (updateNilai tmpNilai find [] nilai)
                    writeFile pathFile $ unlines $ (extractNilais tmp)
                    askNilai tmp murid pel pers
                    where naPel (Nilai {murid = x, mapel = y, period = z, pers = w}) = (x,y,z,w)
        "3" -> do 
            viewNilai nilai
            no <- askTheInteger "Input No yang ingin didelete :"
            if (no < 1 || no > length nilai) then do
                putStrLn "No yang diinput tidak ada"
                askNilai nilai murid pel pers
                else do 
                    let find = nilai !! (no - 1) 
                    let tmp = (delete find nilai)
                    writeFile pathFile $ unlines $ (extractNilais tmp)
                    askNilai tmp murid pel pers
                    where naPel (Nilai {murid = x, mapel = y, period = z, pers = w}) = (x,y,z,w)
        "4" -> do 
            viewNilai nilai
            askNilai nilai murid pel pers
        "5" -> writeFile pathFile $ unlines $ (extractNilais nilai)
        _  -> do 
            putStrLn "Input sesuai dengan yang diarahkan"
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

main :: IO ()
main = do 
    tmpFile <- getFileContentOrElse "" "mapel.txt"
    let pel = importMapels $ lines tmpFile

    tmpFile <- getFileContentOrElse "" "murid.txt"
    let murid = importMurids $ lines tmpFile


    tmpFile <- getFileContentOrElse "" "pers.txt"
    let pers = importPersens $ lines tmpFile

    tmpFile <- getFileContentOrElse "" "nilai.txt"
    let nilai = importNilais $ lines tmpFile

    -- need to load pelajaran , periode, persentase , nilai from the file
    putStrLn $ concat(replicate 30 "-")
    putStrLn $ makeString "" [("",10),("Main Menu",10),("",10)]
    putStrLn $ concat(replicate 30 "-")
    putStrLn "1. Mata Pelajaran"
    putStrLn "2. Periode (UTS,UAS, DKK)"
    putStrLn "3. Data Murid"
    putStrLn "4. Nilai"
    putStrLn "5. View Raport All Student Summary"
    putStrLn "6. View Raport Per Anak Detail"
    putStrLn "7. Exit"
    index <- getLine
    case index of 
        "1" -> do
            askPelajaran nilai pel
            removeFile "mapel.txt"
            renameFile "mapels.txt" "mapel.txt"
        "2" -> do
            askPersentase nilai pers
            removeFile "pers.txt"
            renameFile "perss.txt" "pers.txt"
        "3" -> do 
            askMurid nilai murid
            removeFile "murid.txt"
            renameFile "murids.txt" "murid.txt"
        "4" -> do 
            askNilai nilai murid pel pers
            removeFile "nilai.txt"
            renameFile "nilais.txt" "nilai.txt"
        "5" -> viewRaport nilai
        "6" -> viewDetRaport nilai
        "7" -> putStr ""
        _ -> main
    if index /= "7" then 
        main 
        else
            putStr ""

