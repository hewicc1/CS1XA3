#!/bin/bash

# CS 1XA3 Assignment #1: ProjectAnalyze


#Functions:

# Informs you if you're local repo is up to date with the remote repo (Hint: theres already a git command to do this, just find it)
up_to_date() { # corresponds to requirement #1 in readme.md
echo hi
#git diff --staged
} 

# Puts all uncommited changes in a file changes.log (Hint: just redirect the output of a certain git command)
uncommited_changes() { #corresponds to requirement #2 in readme.md
git status > changes.log
git diff > changes2.log
}


# Puts each line from every file of your project with the tag #TODO into a file todo.log
todo() { # corresponds to requirement #3 in readme.md
echo "Total "#TODO"'s Found:" > todo.log
git grep --count "#TODO" >> todo.log # shows total number of #TODO's in the files
echo "" >> todo.log
echo "Instances Found:" >> todo.log
git grep "#TODO" >> todo.log # shows each line that has #TODO
echo "Created todo.log"
}

# Checks all haskell files for syntax errors and puts the results into error.log (Hint: ghc -fno-code file.hs)
haskell_errors() {
#find . -name "*?.hs" | xargs -I {} ghc -fno-code "{}" > error.log
#ghc -fno-code *?.hs > error.log
echo hi
}



# Runs Script:
# idea of starting prompt comes classmate Noa Barsky @ https://github.com/barskyn/CS1XA3/blob/master/Assign1/ProjectAnalyze.sh
echo "Would you like to start ProjectAnalyze.sh (y/n)"
read start
if [ $start != "y" ]
then
	exit
fi

up_to_date
uncommited_changes
todo
haskell_errors


