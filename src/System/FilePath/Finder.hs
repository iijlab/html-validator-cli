module System.FilePath.Finder
    ( findFiles
    )
where

import           Control.Monad         (filterM)
import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        listDirectory)
import           System.FilePath.Posix (takeExtension, takeFileName, (</>))


findFiles :: [String] -> [String] -> FilePath -> IO [FilePath]
findFiles extensions excludedDirs dir = do
    exist <- doesDirectoryExist dir
    let excluded = (takeFileName dir) `elem` excludedDirs
    if not exist || excluded
        then return []
        else do
            entries <- listDirectory dir
            let paths = map (dir </>) entries

            files <- filterM doesFileExist paths
            let foundFiles = filter (hasExtension extensions) files

            dirs             <- filterM doesDirectoryExist paths
            foundFilesInDirs <- mapM (findFiles extensions excludedDirs) dirs

            return $ concat (foundFiles : foundFilesInDirs)


-- >>> hasExtension [".htm", ".html"] "index.html"
-- True
-- >>> hasExtension [".htm", ".html"] "index.php.html"
-- True
-- >>> hasExtension [".htm", ".html"] "index.php"
-- False
-- >>> hasExtension [] "index.html"
-- False
-- >>> hasExtension [".htm", ".html"] "html"
-- False
hasExtension :: [String] -> FilePath -> Bool
hasExtension extensions path = (takeExtension path) `elem` extensions
