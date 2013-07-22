import System.IO
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.FilePath
import System.Cmd
import Data.List
import Text.Regex.Posix

parentDirectory :: FilePath -> FilePath
parentDirectory d = joinPath (init (splitDirectories d))

listAncestors :: FilePath -> [FilePath]
listAncestors "" = []
listAncestors path = [path] ++ listAncestors (parentDirectory path)

findGitFolder :: FilePath -> IO (Maybe FilePath)
findGitFolder "" 	= return Nothing
findGitFolder path 	= do
	let gitPath = joinPath [path, ".git"]
	exists <- doesDirectoryExist gitPath
	if exists
	then return (Just gitPath)
	else findGitFolder (parentDirectory path) 

githubUrl :: FilePath -> IO (Maybe String)
githubUrl configPath = do
	results <- parseConfig configPath
	if (length results) > 0
	then return (Just ("https://github.com/" ++ ((last (head results)))))
	else return Nothing

parseConfig :: FilePath -> IO [[String]]
parseConfig configPath = do 
	content <- readFile configPath
	return (content =~ "github.com[:/]([^.]*)")

launchGithubForPath :: FilePath -> IO ()
launchGithubForPath path = do
						gitFolder <- findGitFolder path
						case gitFolder of 
							Just f  -> launchBrowser (githubUrl (joinPath [f, "config"]))
							Nothing -> putStrLn "Current folder is not inside a git repository"

--TODO .. change this to just take a string
launchBrowser url = do 
	toOpen <- url
	case toOpen of
		Just u ->  do 
#if defined(mingw32_HOST_OS)
			--windows
			putStrLn "OS: Windows"
			system ("start " ++ u)
#elif defined(linux_HOST_OS)
			--linux
			putStrLn "OS: Linux"
			system ("xdg-open " ++ u)
#elsif defined(bsd_HOST_OS)
			-- hopefully osx?
			putStrLn "OS: BSD"
			system ("open " ++ u)
#else 
			-- just hope chrome is here :)
			putStrLn "OS: Something else"
			system ("chrome " ++ u)
#endif
			putStrLn ("Launching " ++ u)
		Nothing -> putStrLn "No github remote found"

main = do
	currentFolder <- getCurrentDirectory 
	launchGithubForPath currentFolder