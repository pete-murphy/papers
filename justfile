# Shows the list of recipes
help:
    @just --list

# Open a repl for the project
repl paper:
    @ghcid --command="cabal repl {{paper}}" --allow-eval --test=":main"

# List all the projects in the cabal file
list:
    @cabal build --dry-run | awk -F 'lib:' 'NF>1{print $2}' | awk -F ')' '{print $1}'
