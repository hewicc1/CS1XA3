# CS 1XA3 Assignment #1: ProjectAnalyze.sh
## Goal:
Create a bash script ProjectAnalyze.sh to keep at the root of your git repo that at least the following functionality:
### Requirements:
1. Informs you if your local repo is up to date with the remote repo
2. Puts all uncommited changes in a file changes.log
3. Puts each line from every file of your project with the tag #TODO into a file todo.log
4. Checks all haskell files for syntax errors and puts the results into error.log
### Addition Features:
1. File size summary
### Instructions:
1. Keep and run script in root of git repo
2. Will notify if your local repo is upto date with remote repo. If not up to date you must execute git pull to update
3. Will create changes.log that has all uncommited changes
4. Will create todo.log that has the total number "of #TODO's" in files as well as each instance that is found
5. Will create error.log that will show syntax errors of all .hs (haskell) files
6. Will create size.log that contain a basic summary of the size of the repo and the size of its files
### Functions:
#### up_to_date
Corresponds to requirement #1.
Uses grep and wc to check the output of git remote show origin (which shows tracked repos from the origin) if 'up to date' is mentioned on one line.
If 'up to date' is mentioned it means your local repo is up to date with your remote repo.
If you are not up to date you are then asked if you would like to update your local repo using git pull, then performs git pull based on your answer.
#### uncommited_changes
Corresponds to requirement #2.
Creates changes.log and redirects the output of the git diff with the --staged flag to show all the differences in the added files only.
#### todo
Corresponds to requirement #3.
Creates todo.log then appends total numbers of "#TODO's" using the git grep --count flag then appends every instance using git grep with no flags.
#### haskell_errors
Corresponds to requirement #4.
Creates error.log that contains the stdout and stderr of checking the Haskell code for syntax errors.
Then removes all errors that are having an undefined Main module.
#### size_summary
Corresponds to additional feature #1.
First displays the total size of the repo to size.log then appends the size of individual human-readable files excluding hidden files.
### Citations:
* idea of starting prompt from classmate Noa Barsky @ https://github.com/barskyn/CS1XA3/blob/master/Assign1/ProjectAnalyze.sh
* used Markdown reference sheet @ https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet to format readme. Link was provided by classmate Noa Barsky on Slack.
