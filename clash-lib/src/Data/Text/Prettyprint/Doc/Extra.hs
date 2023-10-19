{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Text.Prettyprint.Doc.Extra
  ( module Data.Text.Prettyprint.Doc.Extra
  , LayoutOptions (..)
  , PageWidth (..)
  , layoutCompact
  , layoutPretty
  , renderLazy
  )
where

import           Control.Applicative
import           Data.String                           (IsString (..))
import           Data.Text                             as T
import           Data.Text.Lazy                        as LT

#if MIN_VERSION_prettyprinter(1,7,0)
import qualified Prettyprinter                         as PP
import           Prettyprinter.Internal                hiding (Doc)
import           Prettyprinter.Render.Text
#else
import qualified Data.Text.Prettyprint.Doc             as PP
import           Data.Text.Prettyprint.Doc.Internal    hiding (Doc)
import           Data.Text.Prettyprint.Doc.Render.Text
#endif

type Doc = PP.Doc ()

layoutOneLine
  :: PP.Doc ann
  -> SimpleDocStream ann
layoutOneLine doc = scan 0 [doc]
  where
    scan _ [] = SEmpty
    scan !col (d:ds) = case d of
        Fail            -> SFail
        Empty           -> scan col ds
        Char c          -> SChar c (scan (col+1) ds)
        Text l t        -> let !col' = col+l in SText l t (scan col' ds)
        FlatAlt x _     -> scan col (x:ds)
        Line            -> scan col ds
        Cat x y         -> scan col (x:y:ds)
        Nest _ x        -> scan col (x:ds)
        Union _ y       -> scan col (y:ds)
        Column f        -> scan col (f col:ds)
        WithPageWidth f -> scan col (f Unbounded : ds)
        Nesting f       -> scan col (f 0 : ds)
        Annotated _ x   -> scan col (x:ds)

renderOneLine
  :: PP.Doc ann
  -> LT.Text
renderOneLine = renderLazy . layoutPretty defaultLayoutOptions


argPpr :: (Applicative f, PP.Pretty a) => a -> f Doc
argPpr = pure . PP.pretty

int :: Applicative f => Int -> f Doc
int = pure . PP.pretty

integer :: Applicative f => Integer -> f Doc
integer = pure . PP.pretty

char :: Applicative f => Char -> f Doc
char = pure . PP.pretty

lbrace :: Applicative f => f Doc
lbrace = pure PP.lbrace

rbrace :: Applicative f => f Doc
rbrace = pure PP.rbrace

colon :: Applicative f => f Doc
colon = pure PP.colon

semi :: Applicative f => f Doc
semi = pure PP.semi

equals :: Applicative f => f Doc
equals = pure PP.equals

comma :: Applicative f => f Doc
comma = pure PP.comma

dot :: Applicative f => f Doc
dot = pure PP.dot

lparen :: Applicative f => f Doc
lparen = pure PP.lparen

rparen :: Applicative f => f Doc
rparen = pure PP.rparen

space :: Applicative f => f Doc
space = pure PP.space

brackets :: Functor f => f Doc -> f Doc
brackets = fmap PP.brackets

braces :: Functor f => f Doc -> f Doc
braces = fmap PP.braces

tupled :: Functor f => f [Doc] -> f Doc
tupled = fmap PP.tupled

(<+>) :: Applicative f => f Doc -> f Doc -> f Doc
(<+>) = liftA2 (PP.<+>)
infixr 6 <+>

vcat :: Functor f => f [Doc] -> f Doc
vcat = fmap PP.vcat

hcat :: Functor f => f [Doc] -> f Doc
hcat = fmap PP.hcat

nest :: Functor f => Int -> f Doc -> f Doc
nest i = fmap (PP.nest i)

indent :: Functor f => Int -> f Doc -> f Doc
indent i = fmap (PP.indent i)

parens :: Functor f => f Doc -> f Doc
parens = fmap PP.parens

emptyDoc :: Applicative f => f Doc
emptyDoc = pure PP.emptyDoc

punctuate :: Applicative f => f Doc -> f [Doc] -> f [Doc]
punctuate = liftA2 PP.punctuate

encloseSep :: Applicative f => f Doc -> f Doc -> f Doc -> f [Doc] -> f Doc
encloseSep l r s is = PP.encloseSep <$> l <*> r <*> s <*> is

enclose :: Applicative f => f Doc -> f Doc -> f Doc -> f Doc
enclose = liftA3 PP.enclose

line :: Applicative f => f Doc
line = pure PP.line

line' :: Applicative f => f Doc
line' = pure PP.line'

softline :: Applicative f => f Doc
softline = pure PP.softline

softline' :: Applicative f => f Doc
softline' = pure PP.softline'

pretty :: (Applicative f, Pretty a) => a -> f Doc
pretty = pure . PP.pretty

stringS :: Applicative f => T.Text -> f Doc
stringS = pure . PP.pretty

string :: Applicative f => LT.Text -> f Doc
string = pure . PP.pretty

squotes :: Applicative f => f Doc -> f Doc
squotes = fmap PP.squotes

dquotes :: Functor f => f Doc -> f Doc
dquotes = fmap PP.dquotes

align :: Functor f => f Doc -> f Doc
align = fmap PP.align

hsep :: Functor f => f [Doc] -> f Doc
hsep = fmap PP.hsep

vsep :: Functor f => f [Doc] -> f Doc
vsep = fmap PP.vsep

isEmpty :: Doc -> Bool
isEmpty Empty = True
isEmpty _     = False

fill :: Applicative f => Int -> f Doc -> f Doc
fill = fmap . PP.fill

column :: Functor f => f (Int -> Doc) -> f Doc
column = fmap PP.column

nesting :: Functor f => f (Int -> Doc) -> f Doc
nesting = fmap PP.nesting

flatAlt :: Applicative f => f Doc -> f Doc -> f Doc
flatAlt = liftA2 PP.flatAlt

instance Applicative f => IsString (f Doc) where
  fromString = string . fromString

comment :: Applicative f => T.Text -> T.Text -> f Doc
comment prefix comm =
  let go s = PP.pretty prefix PP.<+> PP.pretty s in
  pure (PP.vsep (Prelude.map go (T.lines comm)))

squote :: Applicative f => f Doc
squote = string (LT.pack "'")
