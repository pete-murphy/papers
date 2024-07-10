# Shows the list of recipes
help:
    @just --list

repl paper:
    @ghcid --command="cabal repl {{paper}}" --allow-eval --test=":main"
