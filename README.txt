Requires:
ghc-7.10
stack 1.1

BUILD:
stack setup
stack build

RUN:
stack exec WaterVolumeSolver-exe filepath X -- +RTS -H3.5G