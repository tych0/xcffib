#!/bin/bash -eux

MODE=$1

VENV=$(mktemp -d xcffib-test-$MODE.XXXXXXXXXX)

function cleanup {
    rm -rf "${VENV}"
}
trap cleanup EXIT

# mask CC in abi_precompiled so that we force compilation at runtime
if [ "${MODE}" == "abi" ]; then
    export CC=false
fi

python -m venv "${VENV}"
source "${VENV}/bin/activate"

pip install -r requirements.txt
flock xcffib.cabal pip install --no-cache -v .

# make a temporary env to test install and ensure we cd somewhere
# we won't pick up the local source xcffib module. the venv dir is as good a
# place as any
python -I -c "import xcffib; print(\"mode is\", xcffib.cffi_mode, \"path is\", xcffib.__file__); assert xcffib.cffi_mode == \"${MODE}\""
python -I -m pytest --durations=3 -n auto
