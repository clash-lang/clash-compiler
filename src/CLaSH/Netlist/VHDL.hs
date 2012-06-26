{-# LANGUAGE OverloadedStrings #-}
module CLaSH.Netlist.VHDL where

import Text.PrettyPrint.Leijen.Text

import CLaSH.Netlist.Types
import CLaSH.Netlist.Util

genVHDL :: Component -> (Identifier,Doc)
genVHDL m = (componentName m, vhdl)
  where
    vhdl = entity m <$$>
           architecture m

entity :: Component -> Doc
entity m = text "entity" <+> text (componentName m) <+> text "is" <$>
             (case ports of
                [] -> empty
                _  -> indent 2 (text "port" <> parens (align $ vcat $ punctuate semi ports) <> semi)
             ) <$>
             text "end entity" <+> text (componentName m) <> semi
  where
    ports = [ text i <+> colon <+> text "in" <+> vhdlType ty
            | (i,ty) <- inputs m ] ++
            [ text (fst $ output m) <+> colon <+> text "out" <+> vhdlType (snd $ output m)
            ]

vhdlType :: HWType -> Doc
vhdlType Bool     = text "boolean"
vhdlType (SP _ _) = text "std_logic_vector"

architecture :: Component -> Doc
architecture _ = empty
