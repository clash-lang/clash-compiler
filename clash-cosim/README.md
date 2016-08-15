# clash-cosim
CλaSH (pronounced ‘clash’) is a functional hardware description language that borrows both its syntax and semantics from the functional programming language Haskell. The CλaSH compiler transforms these high-level descriptions to low-level synthesizable VHDL, Verilog, or SystemVerilog. [COPIED FROM http://www.clash-lang.org]

For my master thesis, I implemented co-simulation between CλaSH and Verilog (using the VPI). If you have any questions or suggestions about the code, code-style, the usage, or other things related to this co-simulation please let me know :)

This implementation can be seen as an extension for CλaSH and CλaSH is thus the first prerequisite. See http://www.clash-lang.org for the details on how to install CλaSH.

Furthermore, a Verilog simulator is needed. I recommend Icarus Verilog. Please build and install Icarus Verilog from source; for the latest instructions go to https://github.com/steveicarus/iverilog.

After installing CλaSH and Icarus Verilog, the MAKEFILE can be used to build and load this co-simulation. The MAKEFILE will automatically execute CλaSH. A FIR filter is added as example in the file 'CoSimTest.hs, which is copied from http://www.clash-lang.org.
To execute this example, please run:

       λ> sampleN 4 $ topEntity testInput
      [4,12,1,20]

Comparing this output with the 'expectedOutput' returns four times 'false' (meaning that the values are the same).

      λ> sampleN 4 $ expectedOutput $ topEntity testInput
      [False,False,False,False]

With the command ':verilog', the CλaSH-compiler will compile the FIR filter to Verilog code.

      λ> :verilog

After the compilation, the Verilog code can be tested with the co-simulation.

      λ> sampleN 4 $ verilog_fir testInput
      [4,12,1,20]

Two other examples are added, the first one defines a multiplier in Verilog.

      λ> let a = fromList [1..]
      λ> let b = verilog_mult a $ register 1 b
      λ> sampleN 10 b
      [1,2,6,24,120,720,5040,40320,362880,3628800]

The last one is an adder with the memory element in Verilog. 

      λ> sampleN 10 $ verilog_reg $ fromList [1..]
      [0,1,3,6,10,15,21,28,36,45]

The co-simulation works mainly with four commands. See the file 'CoSimTest.hs' for examples of these commands. The first one is a QuasiQuoter defined as:

[verilog| module ... endmodule |]

Between the brackets the Verilog top-entity must be defined. This QuasiQuoter will return a so called 'CoSimSettings'. The co-simulation will automatically iterate through this module to retrieve the input and output ports. The ports are retrieved from left to right. 

The function 'coSim' will execute the co-simulation. The first argument is the 'CoSimSettings', the second argument indicates the Verilog Simulator (in this case 'Icarus'), and the third argument is the name of the top-module. The next arguments are the input-values which will be connected to the Verilog ports.

The 'CoSimSettings' can be updated with two functions: 'coSimSeq' and 'coSimFiles'. The last one will add extra Verilog sources, defined as the path to the sources or the directory containing the sources.
With the function 'coSimSeq' files can also be added, the clock-period can be adjusted and a reset phase can be added. 

