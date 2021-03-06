module Main (main) where

import Prelude hiding (readFile, writeFile)
import Text.Pandoc.Options (ReaderOptions(..), WriterOptions(..), def, HTMLMathMethod(MathML), Extension(..))
import Text.Pandoc.Readers.Markdown (readMarkdown)
import HTML (writeHtml)
import Text.Pandoc.Error (handleError)
import System.Environment (getArgs)
import Data.Set (fromList, Set)
import System.FilePath ((</>), (<.>), (-<.>), takeDirectory, takeBaseName, makeRelative)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import System.Console.GetOpt (OptDescr(Option), ArgDescr(NoArg, ReqArg), getOpt, ArgOrder(Permute), usageInfo)
import Paths_pandoc_custom (version)
import Data.Version (showVersion)
import Data.Maybe (listToMaybe, fromMaybe)
import Safe (atMay)
import Control.Applicative ((<|>))
import System.FilePath.Find (find, always, extension, (==?))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
import System.IO (stderr, hPutStrLn)
import Data.ByteString.Lazy.UTF8 (toString)

data InputFrom = FromSTDIN | FromFile FilePath | FromDirectory FilePath
data OutputTo = ToSTDOUT | ToFile FilePath | ToDirectory FilePath

data Opts = OptOut FilePath | OptIn FilePath | OptTemplate FilePath | OptHelp | OptVersion deriving Eq

undefinedCase :: String
undefinedCase = "Undefined case. Please contact to https://github.com/xnuk/blog/issues"

options :: [OptDescr Opts]
options = [ Option ['f', 'c', 's'] ["from", "input", "in", "src", "source"] (ReqArg OptIn "FILE")
              "Markdown file or directory to be converted. Search *.md recursively if it's directory"
          , Option ['t', 'o', 'd'] ["to", "output", "out", "dest"] (ReqArg OptOut "FILE")
              "HTML file or directory to be stored. Store recursively if it's directory"
          , Option [] ["template"] (ReqArg OptTemplate "FILE")
              "HTML template file"
          , Option ['h', '?'] ["help"] (NoArg OptHelp)
              "Show this message"
          , Option ['v'] ["version"] (NoArg OptVersion)
              "Show version info"
          ]

pathToTarget :: FilePath -> FilePath -> IO (InputFrom, OutputTo)
pathToTarget "" "" = return (FromSTDIN, ToSTDOUT)
pathToTarget "" path = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then fail "Output should not be directory when STDIN is given"
    else return (FromSTDIN, ToFile path)

pathToTarget path "" = do
  isDirectory <- doesDirectoryExist path
  return ((if isDirectory then FromDirectory else FromFile) path, ToSTDOUT)

pathToTarget ipath opath = do
  isInDirectory <- doesDirectoryExist ipath
  isOutDirectory <- doesDirectoryExist opath
  let inp = (if isInDirectory then FromDirectory else FromFile) ipath
      out = case inp of
        FromFile ipath' -> if isOutDirectory
          then ToFile $ opath </> takeBaseName ipath' <.> "html"
          else ToFile opath
        FromDirectory _ -> ToDirectory opath
        _ -> error ("pathToTarget : " ++ undefinedCase)
  return (inp, out)

extensions :: Set Extension
extensions = fromList
  [ Ext_footnotes --
  , Ext_inline_notes --
  , Ext_yaml_metadata_block
  , Ext_table_captions
  , Ext_simple_tables --
  , Ext_multiline_tables --
  , Ext_grid_tables --
  , Ext_pipe_tables
  , Ext_raw_tex
  , Ext_raw_html
  , Ext_tex_math_dollars
  , Ext_latex_macros
  , Ext_backtick_code_blocks
  , Ext_markdown_in_html_blocks
  , Ext_native_divs
  , Ext_native_spans
  , Ext_escaped_line_breaks
  , Ext_fancy_lists
  , Ext_startnum
  , Ext_definition_lists --
  , Ext_all_symbols_escapable
  , Ext_intraword_underscores
  , Ext_blank_before_blockquote
  , Ext_blank_before_header
  , Ext_strikeout -- ~~a~~
  , Ext_superscript
  , Ext_subscript
  , Ext_emoji
  , Ext_auto_identifiers
  , Ext_header_attributes -- # My header {#foo}
  , Ext_implicit_header_references -- [header name] <-- link
  , Ext_line_blocks -- preceding |s to remain preceding spaces
  , Ext_shortcut_reference_links -- [a]: http://
  ]

wOptions :: WriterOptions
wOptions = def
  { writerIncremental = False
  , writerHTMLMathMethod = MathML Nothing
  , writerIgnoreNotes = False
  , writerExtensions = extensions
  , writerHtml5 = True
  , writerHighlight = False
  }

main :: IO ()
main = do
  (opts, nonopts, opterrs) <- getOpt Permute options <$> getArgs
  let helpversion = filter (`elem` [OptHelp, OptVersion]) opts
      versionStr = showVersion version
      header = "Pandoc Custom by Xnuk Shuman - " ++ versionStr ++ "\n"
      (filepath, template) =
        ( pathToTarget
            (fromMaybe "" $ listToMaybe ins'  <|> atMay nonopts 0)
            (fromMaybe "" $ listToMaybe outs' <|> atMay nonopts 1)
        , template'
        )
        where (ins', outs', template') = foldr f ([], [], "") opts
                where f z (ins, outs, template'') = case z of
                        OptIn       x -> (x:ins,   outs, template'')
                        OptOut      x -> (  ins, x:outs, template'')
                        OptTemplate x -> (  ins,   outs, x         )
                        _             -> (  ins,   outs, template'')
  case helpversion of
    [] -> if null opterrs
      then if template == ""
        then fail "No template file given"
        else filepath >>= main2 template
      else do
        hPutStrLn stderr $ usageInfo header options ++ "\n"
        ioError . userError $ concat opterrs
    OptHelp:_ -> Prelude.putStrLn $ usageInfo header options
    OptVersion:_ -> Prelude.putStrLn versionStr
    _ -> fail $ "helpversion : " ++ undefinedCase

convert :: String -- template
        -> String -- tag
        -> String -- body
        -> ByteString
convert template tag = renderHtml . writeHtml writeOptions . handleError . readMarkdown readOptions
  where readOptions = def { readerExtensions = extensions }
        writeOptions = wOptions
          { writerStandalone = True
          , writerTemplate = template
          , writerVariables = [("root", "/blog/"), ("sexytag", tag)]
          }

main2 :: FilePath -> (InputFrom, OutputTo) -> IO ()
main2 templateSrc z = do
  template <- readFile templateSrc
  let conv = convert (toString template)
      write str toPath = do
        createDirectoryIfMissing True $ takeDirectory toPath
        writeFile toPath str
      fileToFile tag fromPath toPath = do
        src <- readFile fromPath
        write (conv tag (toString src)) toPath
      walkMarkdown fromPath f = do
        paths <- map (makeRelative fromPath) <$> find always (extension ==? ".md") fromPath
        mapM_ f paths
  case z of
    (FromFile fromPath, ToFile toPath) -> fileToFile "" fromPath toPath
    (FromDirectory fromPath, ToDirectory toPath) -> do
      createDirectoryIfMissing True toPath
      walkMarkdown fromPath $ \path ->
        fileToFile (dropWhile (`elem` ".\\/") $ takeDirectory path) (fromPath </> path) (toPath </> path -<.> "html")
    (FromDirectory fromPath, ToSTDOUT) -> walkMarkdown fromPath $ \path -> do
      putStrLn $ "\n\n" ++ path ++ ":\n\n"
      readFile (fromPath </> path) >>= putStrLn . toString . conv (dropWhile (`elem` ".\\/") $ takeDirectory path) . toString
    (FromSTDIN, ToFile toPath) -> getContents >>= (`write` toPath) . conv ""
    (FromFile fromPath, ToSTDOUT) -> readFile fromPath >>= putStrLn . toString . conv "" . toString
    (FromSTDIN, ToSTDOUT) -> getContents >>= putStrLn . toString . conv ""
    _ -> fail $ "main2 - case z : " ++ undefinedCase
