#!/bin/bash

# bash assignment

# Informs you if you're local repo is up to date with the remote repo (Hint: theres already a git command to do this, just find it)

git fetch --dry-run


# Puts all uncommited changes in a file changes.log (Hint: just redirect the output of a certain git command)

git status > changes.log


# Puts each line from every file of your project with the tag #TODO into a file todo.log

git grep "#TODO" > todo.log


# Checks all haskell files for syntax errors and puts the results into error.log (Hint: ghc -fno-code file.hs)

#find . -name "*?.hs" | xargs -I {} ghc -fno-code "{}" > error.log


# Add at least one feature of your own design




# Document the script, how its used and a description of each feature in the Assign1 README file

	


# Feel free to look at other peoples Assignments on GitHub and integrate useful features they added into your own, but you must reference who you got the feature from in your README







