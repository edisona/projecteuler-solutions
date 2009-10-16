-- File EulerForHs.hs
module EulerForHs where

import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M

-- Solutions list.
-- use "" if answer is not known.
-- Generally you should use solutionsMap instead of this
-- because then the problem numbers can go with the filtering,
-- and other data manipulation.
solutions :: [String]
solutions = [
	"233168", "4613732", "6857", "906609", "232792560", "25164150", "104743", "40824", "31875000", "142913828922",
	"70600674", "76576500", "5537376230", "837799", "137846528820", "1366", "21124", "1074", "171", "648", "31626",
	"871198282", "4179871", "2783915460", "4782", "983", "-59231", "669171001", "9183", "443839", "73682", "45228",
	"100", "40730", "55", "872187", "748317", "932718654", "840", "210", "7652413", "162", "16695334890", "5482660",
	"1533776805", "5777", "134043", "9110846700", "296962999629", "997651", "121313", "142857", "4075", "376", "249",
	"972", "153", "26241", "107359", "26033", "28684", "127035954683", "49", "1322", "272", "661", "7273",
	"6531031914842725", "510510", "8319823", "428570", "303963552391", "7295372", "402", "161667", "190569291", "71",
	"55374", "73162890", "40886", "427337", "260324", "425185", "101524", "2772", "1818", "1097343", "7587457", "743",
	"1217", "14234", "8581146", "1258", "518408346", "14316", "24702", "8739992577", "18769", "709", "756872327473",
	"37076114526", "228", "20313839404245", "329468", "73702", "21384", "259679", "180180", "38182",
	"9350130049860600", "612407567715", "1587000", "51161058134250", "16475640049", "168", "20492570929",
	"100808458960497", "44680", "248155780267521", "333082500", "2269", "1582", "21035", "21417", "2906969179",
	"18522", "18407904", "14516824220", "1000023", "149253", "173", "843296", "453647705", "18613426663617118", "4989",
	"2544559", "1120149658760", "1118049290473932", "10057761", "5673835352990", "878454337159", "1006193", "30758397",
	"354", "608720", "676333270", "846910284", "2129970655314432", "52852124", "-271248680", "0.464399", "301",
	"17971254122360635", "479742450", "3857447", "21295121502550", "53490", "409511334375", "14489159", "16576",
	"20574308184277971", "3D58725572C62302", "343047", "378158756814587", "2868868", "7130034", "3916160068885",
	"59206", "178653872807", "9857164023", "142989277", "227485267000992000", "1572729", "209566", "1,13717420,8",
	"96818198400000", "129325", "126461847755", "986262", "285196020571078987", "83735848679360680", "399788195976",
	"48861552", "1725323624056", "4640261571849533", "2325629", "17427258", "95962097", "10834893628237824", "371048281",
	"1918080160", "57060635927998347", "684465067343069", "61190912", "75085391", "322303240771079935", "1.710637717",
	"52374425", "0.00396087", "229161792008", "115039000", "1209002624", "34029210557338", "2944730", "0.5731441",
	"1389019170", "44043947822", "331951449665644800", "15964587728784", "1598174770174689458", "1922364685",
	"328968937309", "330.721154", "1677366278943", "806844323190414", "5437849", "6273134", "0", "64564225042",
	"139776,963904", "1884161251122450", "1590933", "61614848", "4137330", "2009", "0.11316017", "3780.618622", "86226",
	"11325263", "850481152593119296", "7526965179680", "0.83648556", "271204031455541309", "1259187438574927161",
	"1.002322108633", "123/59", "15836928", "9922545104535661", "0.001887854841", "7448717393364181966",
	"482316491800641154", "997104142249036713", "892371480", "96356848", "288084712410001", "810834388", "782252",
	"23507044290", "9275262564250418", "1425480602091519", "18946051", "104924.0", "11.492847", "8184523820510",
	"4.4474011180", "85765680", "139012411", "12747994", "20101196798", "", "", "", "", "", "", "", "", "", "", ""]

-- Solutions as a lookup map.
solutionsMap :: M.Map Int String
solutionsMap = M.fromList $ zip [1..] solutions

-- Get the answer for problem n.
-- For example:
-- 	getAnswer 1 = "233168"
getAnswer :: Int -> String
getAnswer = fromJust . flip M.lookup solutionsMap

-- Do we have the answer for problem n?
hasAnswer :: Int -> Bool
hasAnswer = not . (==) [] . getAnswer

-- Is the solution an integer, decimal, or something else?
-- SOther: Not integer nor decimal
-- SUnknown: Answer actually not known
data SolutionType =
	SInteger |
	SDecimal |
	SOther |
	SUnknown deriving (Show, Eq)

{--
 - Can the String be parsed as an integer?
 - isInt "1234" = True
 - isInt "-123" = True
 - isInt "12-3" = False
 - isInt "123abc" = False
 --}
isInt :: String -> Bool
isInt (f : xs)
	| f == '-' = isInt' xs
	| otherwise = isInt' (f:xs) where
		isInt' = all (`elem` ['0'..'9'])
isInt _ = False

{--
 - Can the String be parsed as a decimal?
 - A decimal has one decimal point, not the first
 - nor the last character of the String.
 - isDecimal "12.3" = True
 - isDecimal "123." = False
 - isDecimal ".123" = False
 - isDecimal "0.123" = True
 - isDecimal "-3.14" = True
 - isDecimal "-3.x4" = False
 --}
isDecimal :: String -> Bool
isDecimal (f : xs)
	| f == '-' = isDecimal' xs
	| otherwise = isDecimal' (f:xs) where
		isDecimal' zs = case length $ L.elemIndices '.' zs of
			1 -> case head zs of
				'.' -> False
				_ -> case last zs of
					'.' -> False
					_ -> all (`elem` '.':['0'..'9']) zs
			_ -> False
isDecimal _ = False

-- Solutions with their answer type.
solutionsByType :: M.Map Int (SolutionType, String)
solutionsByType = M.mapWithKey getTypeOf solutionsMap where
	getTypeOf k s
		| (not . hasAnswer) k = (SUnknown, s)
		| isInt s = (SInteger, s)
		| isDecimal s = (SDecimal, s)
		| otherwise = (SOther, s)

-- Solutions partitioned by their answer type.
solutionsPartitioned :: (M.Map Int String, M.Map Int String, M.Map Int String, M.Map Int String)
solutionsPartitioned = (fz SInteger, fz SDecimal, fz SOther, fz SUnknown) where
	fz b = M.map removeType . M.filter ((==) b . fst) $ solutionsByType
	removeType = \(_,x) -> x

-- Convenience functions.
integerSolutions, decimalSolutions :: M.Map Int String
integerSolutions = (\(x,_,_,_) -> x) solutionsPartitioned
decimalSolutions = (\(_,x,_,_) -> x) solutionsPartitioned



{-- Demonstration
 -- Functions. --}

sortedIntegerSolutions = L.sort $ map (readint . snd) $ M.toList integerSolutions
	where readint :: String -> Int; readint = read

sumOfIntegerSolutions = sum sortedIntegerSolutions

biggestIntegerSolution = maximum sortedIntegerSolutions

