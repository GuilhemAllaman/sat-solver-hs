# Sat Solver

Sat solver to resolve CNF (Conjunctive Normal Forms)

Written in Haskell

ESIPE - UPEM - 2019


## Authors

Julien Nivoix jnivoix@etud.u-pem.fr

Guilhem Allaman gallaman@etud.u-pem.fr

## Algorithm's pseudo-code

Sat solver algorithm runs as following :

```
solve(f):

cnf = convert f to cnf format
a = empty assignment

while not done:

    lit : next literal to process
    if cnf has a unitary clause 'uc'
        lit = uc's unique lit
    else
        lit = most occuring lit in cnf

    cnf = simplify cnf with lit (remove clauses containing lit from cnf and remove lit's opposite from all cnf's clauses)

    insert lit's variable into a (with lit's boolean value)

    if cnf contains an empty clause (with no lits)
        f is not satisfiable -> return no assignment

    if cnf has no more clause
        f is satisfiable with assignment a -> return assignment a
```
After some tests we find that not all answers are given by that solution.
We now have a new algorithm that we try to implement :

```
Solve(calculToDo){
    list = literals without negatives
    request = simplify(calculToDo)
    solution = list.of(empty)
    function1(request, list, solution)
}
function1(request, list, solution){
    lit = take the most common literal (or a single) from list

    function2(request, lit, solution)
    if(lit.isEmpty ==true)
        return
    function1(request, list, solution)
}
function2(request, lit, solution){
    requestBis = delete !lit from request, delete clause with lit
    requestBis' = delete lit from request, delete clause with !lit
    new = Pick the most common literal (or a single) from list
    list2 = list.remove(new)
    if(no clause(requestBis))
        solution.add(lit)
    else
        function3(new , lit, list2, requestBis)
    if(no clause(requestBis'))
        solution.add(!lit)   
    else     
        function3(new , !lit, list2, requestBis')

}
function3(new, listof currentLitsTrue, list, request){
requestBis = delete !new from request, delete clause with new
    if(no clause)
        solution.add(new+currentLitsTrue)
        return    
    else
        requestBis' = delete new from request, delete clause with !new
        if(no clause)
            solution.add(!new+currentLitsTrue)
            return
    newBis = Pick the most common literal (or a single) from list
    list2 = list.remove(newBis)
    function3(newBis , currentLitsTrue, list2)        
}
```

## Install and stack commands

#### Prerequisite

- Install stack with following command :
`curl -sSL https://get.haskellstack.org/ | sh`

- Add following line at the bottom of to your `~/.bashrc` file (in case you want to 'install' compiled programs) :
`export PATH=$PATH:~/.local/bin`

- Reload your bash profile with following command :
`source .bashrc`

- Install latest ghc compiler :
`stack setup`

- Stack config file is located at `~/.stack/config.yaml`
add `allow-different-user: true` to avoid permissions errors

#### Init

Initiate stack project with following command :
`stack init`

#### Build

Following command will build project into .stack-work/dist folder :
`stack build`

Following command will also copy bin file to your local bin folder ~/.local/bin :
`stack install`

#### Run

Run program (Main module) with following command :
`stack exec sat-solver-hs-exe`

#### Clean

`stack clean` : delete `.stack-work/dist` folder
`stack-clean --full` : delete `.stack-work/` folder's files

#### Doc

Generate doc with following command :
`stack haddock`
