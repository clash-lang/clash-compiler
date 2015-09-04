{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds       #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Predefined 'SNat' singleton literals in the range [0 .. 1024]

Defines:

@
d0 = snat :: SNat 0
d1 = snat :: SNat 1
d2 = snat :: SNat 2
...
d1024 = snat :: SNat 1024
@

You can generate more 'SNat' literals using 'decLiteralsD' from "CLaSH.Promoted.Nat.TH"
-}
module CLaSH.Promoted.Nat.Literals where

import CLaSH.Promoted.Nat.TH

$(decLiteralsD 0 1024)
