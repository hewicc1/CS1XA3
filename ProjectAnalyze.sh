#!/bin/bash

# CS 1XA3 Assignment #1: ProjectAnalyze


#Functions:

	# Required Functions:

# Informs you if you're local repo is up to date with the remote repo (Hint: theres already a git command to do this, just find it)
function up_to_date() { # corresponds to requirement #1 in readme.md
if [ $(git remote show origin | grep "up to date" | wc -l) -eq 1 ] # uses grep and wc to see if 'up to date' is mentioned in the output of git remote (manages tracked repos)
then
	echo "Your local repo is up to date with your remote repo." # if 'up to date' is found you are up to date
else
	echo "Your local repo is not up to date with your remote repo."
	echo "Would you like you update it using git pull? (y/n)" # if you are not up to date then asks users if he/she wants to perform git pull to update
	read inp
	if [ $inp = "y" ] # if responds with 'y' then executes git pull
	then
		git pull
	fi
fi	
} 

# Puts all uncommited changes in a file changes.log (Hint: just redirect the output of a certain git command)
function uncommited_changes() { #corresponds to requirement #2 in readme.md
git diff --staged > changes.log # uses the --staged flag to singal differences in only staged files
echo "Created changes.log"
}

# Puts each line from every file of your project with the tag #TODO into a file todo.log
function todo() { # corresponds to requirement #3 in readme.md
echo "Total "#TODO"'s Found:" > todo.log
git grep --count "#TODO" >> todo.log # shows total number of #TODO's in the files
echo "" >> todo.log
echo "Instances Found:" >> todo.log
git grep "#TODO" >> todo.log # shows each line that has #TODO
echo "Created todo.log"
}

# Checks all haskell files for syntax errors and puts the results into error.log (Hint: ghc -fno-code file.hs)
function haskell_errors() { # corresponds to requirement #4 in readme.md
find . -name "*.hs" | xargs -I {} ghc -fno-code "{}" &> error.log # redirects the output from stdout and stderr to error.log
sed -i '/The IO action ‘main’ is not defined in module ‘Main’/d' ./error.log # removes all errors that are about an undefined Main module
echo "Created error.log" 
}


	# Additional Features:

# Creates size.log that contains the entire size of git repo as well as size of non-hidden files/directories
function size_summary(){ # corresponds to additional feature #1
echo "Total Size:" > size.log
du -sh >> size.log # appends total size of repo
echo "" >> size.log
echo "Individual Files:" >> size.log
du -h --exclude "./.*" >> size.log # appends size of each file excluding hidden files
echo "Created size.log"
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
size_summary

