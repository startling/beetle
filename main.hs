{-# Language CPP #-}
{-# Language OverloadedStrings #-}
-- base
import Data.Monoid
import Control.Applicative
import Control.Monad
-- trifecta
import Text.Trifecta hiding (Parser)
-- beetle
import Beetle.Abstract
import qualified Beetle.Parse as P
import qualified Beetle.Compile as C
import Language.Javascript (overExpressions)
import Language.Javascript.Render (runRender, block)
-- text
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy.Builder
-- blaze-markup
import Text.Blaze
import Text.Blaze.Internal
-- blaze-html
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A
-- optparse-applicative
import Options.Applicative
-- provided by cabal
#ifdef MIN_VERSION_base(0,0,0)
import Paths_beetle
#else
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return
#endif

options :: IO (Parser (String, String, String, String, String))
options = getDataFileName "style.css" >>= \css ->
  getDataFileName "runtime.js" >>= \js ->
    return $ (,,,,)
    <$> strOption
       (  long "runtime"
       <> short 'r'
       <> metavar "file"
       <> value js
       <> help "Include an alternative Javascript runtime."
       )
    <*> strOption
       (  long "style"
       <> short 's'
       <> metavar "file"
       <> value css
       <> help "Include an alternative CSS file."
       )
    <*> strOption
      (  long "jquery"
      <> short 'q'
      <> metavar "url"
      <> value "https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
      <> help "JQuery source file to reference; defaults to Google's CDN."
      )
    <*> argument Just
      (  metavar "input"
      <> help "Beetle input file."
      )
    <*> argument Just
      (  metavar "output"
      <> help "File to output HTML to."
      )

parser :: IO (ParserInfo (String, String, String, String, String))
parser = options >>= \o -> return $ info (helper <*> o)
  (  progDesc "Compile Beetle files to Javascript."
  <> header "beetlec - compiler for the Beetle language."
  <> fullDesc
  )

-- Read the javascript runtime, the css file, and the beetle file
-- and write it out as a single html file.
main :: IO ()
main = parser >>= execParser >>=
  \(rtf, csf, jqf, inf, otf) -> do
    run <- T.readFile rtf
    css <- T.readFile csf
    d <- parseFromFile (many P.dec) inf
    e <- maybe (return []) return $ d
    let js = toLazyText . runRender . block . C.compile $ e
    let h = html jqf css $ "\n" <> run <> "\n\n" <> js <> "\n"
    writeFile "out.html" $ renderHtml h

-- An html template.
html :: String -> Text -> Text -> Html
html jq css js = B.docTypeHtml $ do
  B.head $ do
    B.meta ! A.charset "utf-8"
    B.style ( preEscapedLazyText css
      ) ! A.type_ "text/css"
    B.script (return ())
      ! A.src (stringValue jq)
    B.script
      ( preEscapedLazyText js
      ) ! A.type_ "text/javascript"
  B.body $ do
    B.div (return ()) ! A.id "content"
