# clash-cosim
Cosimulation allows inline Verilog (todo: VHDL) to be run as if it were a normal Haskell function. This was originally written [as part of a Master's project](https://essay.utwente.nl/70777/) at the University of Twente by John Verheij. Since then, support for Clash synthesizable code has been added.

TODO: re-add old examples

## Installation
Download the source and in the source directory run:

```bash
cabal install --only-dependencies
cabal install
```

Ubuntu 16.04 and 18.04 are supported, but other Linux platforms will probably work too. Run tests with:

```bash
runghc tests/test.hs
```

## Examples
All examples following assume the following header:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

import Clash.Prelude
import Clash.CoSim

import qualified Data.List as L

... examples here ...
```

We will show simple binary functions, which can be run with:

```
main :: IO ()
main = putStrLn $ show $ take 5 $ sample $ example $
            (stimuliGenerator $ 1 :> 2 :> 3 :> 4 :> 5  :> Nil)
            (stimuliGenerator $ 2 :> 4 :> 6 :> 8 :> 10 :> Nil)
```

where `example` is the function we've defined. For example, the first example (simple multiplier) will look like:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

import Clash.Prelude
import Clash.CoSim
import qualified Data.List as L

main :: IO ()
main = putStrLn $ show $ L.take 5 $ sample $ mult
            (stimuliGenerator $ 1 :> 2 :> 3 :> 4 :> 5  :> Nil)
            (stimuliGenerator $ 2 :> 4 :> 6 :> 8 :> 10 :> Nil)

mult
    :: Signal System (Signed 64)
    -> Signal System (Signed 64)
    -> Signal System (Signed 64)
mult x y = [verilog|
    parameter data_width = 64;

    input  signed [0:data_width-1] ${x};
    input  signed [0:data_width-1] ${y};
    output signed [0:data_width-1] result;

    assign result = ${x} * ${y};
    |]
```

and can be run using:

```bash
martijn@clashtop:~/code/clash-cosim-qbl$ runghc example.hs
[2,8,18,32,50]
```

### Simple 64-bit multiplier
```haskell
mult
    :: Signal System (Signed 64)
    -> Signal System (Signed 64)
    -> Signal System (Signed 64)
mult x y = [verilog|
    parameter data_width = 64;

    input  signed [0:data_width-1] ${x};
    input  signed [0:data_width-1] ${y};
    output signed [0:data_width-1] result;

    assign result = ${x} * ${y};
    |]
```

### Anonymous arguments
```haskell
mult
    :: Signal System (Signed 64)
    -> Signal System (Signed 64)
    -> Signal System (Signed 64)
mult = [verilog|
    parameter data_width = 64;

    input  signed [0:data_width-1] ${0};
    input  signed [0:data_width-1] ${1};
    output signed [0:data_width-1] result;

    assign result = ${0} * ${1};
    |]
```

### Custom settings
```haskell
mult
    :: Signal System (Signed 64)
    -> Signal System (Signed 64)
    -> Signal System (Signed 64)
mult = [verilogS|
    parameter data_width = 64;

    input  signed [0:data_width-1] ${0};
    input  signed [0:data_width-1] ${1};
    output signed [0:data_width-1] result;

    assign result = ${0} * ${1};
    |] defaultSettings { simulator = ModelSim }
```

### Compiling with Clash
Use clash as usual:

```bash
clash example.hs --verilog
```

## Developers
The `clash-cosim` package is composed of different layers, each of which will be explained here. We will use the file structure to explain these layers:

```
├── Setup.hs
├── src
│   ├── cbits
│   ├── Clash
│   │   ├── CoSim
│   │   │   ├── CodeGeneration.hs
│   │   │   ├── CoSimInstances.hs
│   │   │   ├── DSLParser.hs
│   │   │   ├── Simulator.hs
│   │   │   └── Types.hs
│   │   └── CoSim.hs
│   └── prims
└─── tests
```

The very core of the package is `src/Clash/Simulator.hs`, together with `src/cbits`. (This was written as part of a Master's project at the University of Twente, which can be read about [here](https://essay.utwente.nl/70777/).) These packages implement communication with external interpreters such as `Icarus Verilog` and `ModelSim`. It exports a single function `coSimN`: a [polyvariadic](https://stackoverflow.com/questions/3467279/how-to-create-a-polyvariadic-haskell-function) function. We could use it to write inline verilog, simply by calling it:

```haskell
add a b = coSimN source defaultSettings "test" a b
    where
        source = unlines [ "module test (a, b, result);"
                         , "    assign result = a * b;"
                         , "endmodule" ]
```

While this works for cosimulation, it has a few notable problems:

1. It is not synthesizable. In order for Clash to generate verilog code from Haskell code the latter has to be monomorphic and purely functional. If it is not, users can write a "primitive", which tells Clash what verilog to substitute for a specific function. However, this only works for monovariadic functions.

2. Variables in the inline verilog do not correspond with variables in the surrounding Haskell code, possibly leading to confusion.

3. The inline verilog must conform to a specific structure: the module name has to correspond with the module name passed to coSimN, and `result` has to be the last port of the module.

4. Polyvariadic functions generate very confusing errors messages when used incorrectly.

5. Ugly notation compared to other languages, which often allow a very simple inline construct. For example, in Rust one would write `asm!("xor %eax, %eax")` for inline assembly.

We solve (2), (3) and (5) by defining a quasiquoter parsing `${..}` constructs in its given source code, and passing these arguments to `coSimN`. This code can be found in `CoSim.hs` and `DSLParser.hs`. To solve (4), we generate seperate functions for every number of arguments (`coSim1`, `coSim2`, ..). The corresponding code can be found in `CodeGeneration.hs`. We solve (1) by generating Clash primitives for each generated coSim function, whenever we run `Setup.hs`. This code is hosted in `Setup.hs`.

## TODO

* Remove `foreign c` constructs in `Simulator.hs` by implementing the same logic in Haskell.

* Use reflection to automatically define in- and output ports in target HDL.

* Allow Clash blackbox notation to be used (?).

* Implement VHDL cosimulation