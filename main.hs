{-# Language OverloadedStrings #-}
-- base
import Data.Monoid
import Control.Applicative
-- trifecta
import Text.Trifecta hiding (Parser)
-- beetle
import Beetle.Parse
import Beetle.Abstract
import Beetle.Compile
import Language.Javascript
-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Paths_beetle

options :: IO (Parser (String, String, String, String))
options = getDataFileName "style.css" >>= \css ->
  getDataFileName "runtime.js" >>= \js ->
    return $ (,,,)
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
    <*> argument Just
      (  metavar "input"
      <> help "Beetle input file."
      )
    <*> argument Just
      (  metavar "output"
      <> help "File to output HTML to."
      )

parser :: IO (ParserInfo (String, String, String, String))
parser = options >>= \o -> return $ info (helper <*> o)
  (  progDesc "Compile Beetle files to Javascript."
  <> header "beetlec - compiler for the Beetle language."
  <> fullDesc
  )

-- Read the javascript runtime, the css file, and the beetle file
-- and write it out as a single html file.
main :: IO ()
main = parser >>= execParser >>=
  \(rtf, csf, inf, otf) -> do
    run <- T.readFile rtf
    css <- T.readFile csf
    d <- parseFromFile (many dec) inf
    e <- maybe (return []) return $ d
    let js = T.intercalate "\n\n" . map
         (printStatement . declaration (Variable "fuck")) $ e
    let h = html css $ T.intercalate "\n\n" [run, js]
    writeFile "out.html" $ renderHtml h

-- An html template.
html :: Text -> Text -> Html
html css js = B.docTypeHtml $ do
  B.head $ do
    B.meta ! A.charset "utf-8"
    B.style ( preEscapedText css
      ) ! A.type_ "text/css"
    B.script (return ())
      ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
    B.script
      ( preEscapedText js
      ) ! A.type_ "text/javascript"
  B.body $ do
    B.div (return ()) ! A.id "content"
