#!/usr/bin/env bash
set -euo pipefail

# Help message
show_help() {
    local script_name
    script_name=$(basename "$0")
    echo "Fourmolu formatting Script.

Usage:
  $script_name diff    Format the current git diff.
  $script_name full    Format all source files.
  $script_name check   Check the formatting of the source files.
  $script_name (-h | --help)

Options:
  -h --help     Show this screen."
}

# Main script logic
if [[ "$#" -eq 0 || "$1" == "-h" || "$1" == "--help" ]]; then
    show_help
    exit 0
fi

# use array so we can easily append
# Ignore the clash-ghc/src-bin-* since they basically copies of upstream GHC code
readarray -d '' exclude_files < <(find ./clash-ghc/src-bin-* -type f -name "*.hs" -print0)
# Files that Fourmolu can't parse due to CPP/primops, skip them explicitly.
exclude_files+=(
  ./clash-cosim/src/Clash/CoSim/CodeGeneration.hs
  ./clash-ffi/src/Clash/FFI/VPI/Callback.hs
  ./clash-ffi/src/Clash/FFI/VPI/Callback/Reason.hs
  ./clash-ffi/src/Clash/FFI/VPI/Control.hs
  ./clash-ffi/src/Clash/FFI/VPI/Object.hs
  ./clash-ffi/src/Clash/FFI/VPI/Object/Type.hs
  ./clash-ffi/src/Clash/FFI/VPI/Object/Value.hs
  ./clash-ffi/src/Clash/FFI/VPI/Object/Value/Format.hs
  ./clash-ffi/tests/Clash/FFI/Test/Instances.hs
  ./clash-ghc/src-ghc/Clash/GHC/Evaluator/Primitive.hs
  ./clash-ghc/src-ghc/Clash/GHC/LoadInterfaceFiles.hs
  ./clash-ghc/src-ghc/Clash/GHC/LoadModules.hs
  ./clash-ghc/src-ghc/Clash/GHC/Util.hs
  ./clash-lib-hedgehog/src/Clash/Hedgehog/Unique.hs
  ./clash-lib/src/Clash/Core/FreeVars.hs
  ./clash-lib/src/Clash/Core/TermLiteral/TH.hs
  ./clash-lib/src/Clash/Core/VarEnv.hs
  ./clash-lib/src/Clash/Data/UniqMap.hs
  ./clash-lib/src/Clash/Driver/Manifest.hs
  ./clash-lib/src/Clash/Primitives/Sized/ToInteger.hs
  ./clash-prelude/src/Clash/CPP.hs
  ./clash-prelude/src/Clash/Class/BitPack/BitIndex.hs
  ./clash-prelude/src/Clash/Explicit/BlockRam/Blob.hs
  ./clash-prelude/src/Clash/Prelude/BlockRam.hs
  ./clash-prelude/src/Clash/Signal/Trace.hs
  ./clash-prelude/src/Clash/Sized/Internal/Mod.hs
  ./clash-prelude/src/Clash/Sized/RTree.hs
  ./clash-prelude/src/Clash/Sized/Vector.hs
  ./examples/CochleaPlus.hs
  ./examples/Queens.hs
  ./tests/Main.hs
  ./tests/shouldwork/Basic/AES.hs
  ./tests/shouldwork/Basic/T1591.hs
  ./tests/shouldwork/BitVector/UnpackUndefined.hs
  ./tests/shouldwork/BlackBox/LITrendering.hs
  ./tests/shouldwork/CustomReprs/Deriving/BitPackDerivation.hs
  ./tests/shouldwork/DDR/DDRin.hs
  ./tests/shouldwork/DDR/IntelDDR.hs
  ./tests/shouldwork/DDR/VendorDDR.hs
  ./tests/shouldwork/DDR/XilinxDDR.hs
  ./tests/shouldwork/FixedNumber/FixedS.hs
  ./tests/shouldwork/Numbers/Bits.hs
  ./tests/shouldwork/Numbers/BitsTB.hs
  ./tests/shouldwork/Numbers/ExpWithClashCF.hs
  ./tests/shouldwork/Numbers/ExpWithGhcCF.hs
  ./tests/shouldwork/Numbers/Integral.hs
  ./tests/shouldwork/Numbers/IntegralTB.hs
  ./tests/shouldwork/Numbers/NumConstantFoldingTB_1.hs
  ./tests/shouldwork/Numbers/NumConstantFoldingTB_2.hs
  ./tests/shouldwork/Numbers/Resize3.hs
  ./tests/shouldwork/Numbers/ShiftRotate.hs
  ./tests/shouldwork/Numbers/ShiftRotateBase.hs
  ./tests/shouldwork/Numbers/SignedProjectionTB.hs
  ./tests/shouldwork/Numbers/UndefinedConstantFolding.hs
  ./tests/shouldwork/Numbers/UndefinedConstantFoldingTB.hs
  ./tests/shouldwork/RTree/TRepeat2.hs
  ./tests/shouldwork/Signal/DualBlockRam.hs
  ./tests/shouldwork/Signal/DualBlockRamDefinitions.hs
  ./tests/shouldwork/Signal/DynamicClocks.hs
  ./tests/shouldwork/Signal/ROM/Async.hs
  ./tests/shouldwork/Signal/ROM/AsyncBlob.hs
  ./tests/shouldwork/Signal/ROM/Blob.hs
  ./tests/shouldwork/Signal/ROM/BlobVec.hs
  ./tests/shouldwork/Testbench/SyncTB.hs
  ./tests/shouldwork/TopEntity/T3129.hs
  ./tests/shouldwork/Vector/Iterate.hs
  ./tests/shouldwork/Vector/VecConst.hs
  ./tests/src/Test/Tasty/Clash.hs
)

# Make sure it doesn't matter from where this script is executed
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

if [ $1 == "diff" ] ; then
  src_files=$(git diff --cached --name-only --diff-filter=ACMR -- '*.hs')
else
  src_files=$(find ./* -type f -name "*.hs")
fi

src_files_str=$(printf "%s\n" "${src_files[@]}" | sed 's| |\\ |g')
exclude_files_str=$(printf "%s\n" "${exclude_files[@]}" | sed 's| |\\ |g')
src_files=$(echo "$src_files_str" | grep -vwE  "$exclude_files_str")

if [ -z "$src_files" ]; then
  exit 0;
fi

if [[ "$1" == "diff" || "$1" == "full" ]] ; then
  fourmolu --mode inplace $src_files
elif [[ "$1" == "check" ]] ; then
  fourmolu --mode check $src_files
else
  echo "Expected a single argument, \"full\", \"diff\", \"check\" or \"--help\"" >&2
  exit 3
fi
