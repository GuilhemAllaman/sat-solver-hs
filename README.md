# Sat Solver

Sat solver to resolve CNF (Conjunctive Normal Forms)

Written in Haskell

Authors :
Julien Nivoix jnivoix@etud.u-pem.fr
Guilhem Allaman gallaman@etud.u-pem.fr
ESIPE - UPEM - 2019

### Prerequisite

- Install stack with following command :
`curl -sSL https://get.haskellstack.org/ | sh`

- Add following line at the bottom of to your `~/.bashrc` file :
`export PATH=$PATH:~/.local/bin`

- Reload your bash profile with following command :
`source .bashrc`

- Install latest ghc compiler :
`stack setup`

- Stack config file is located at `~/.stack/config.yaml`
add `allow-different-user: true` to avoid permissions errors


#### Init
`stack init`

#### Build 
Following command will build project into .stack-work/dist folder :
`stack build`
Following command will also copy bin file to your local bin folder ~/.local/bin :
`stack install`

#### Run
`stack exec sat-solver-hs-exe`

#### Generate doc
`stack haddock`

#### Clean
`stack clean` : delete `.stack-work/dist` folder
`stack-clean --full` : delete `.stack-work/` folder's files
