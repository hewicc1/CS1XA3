# CS 1XA3 Assignment #1: ProjectAnalyze.sh
## Goal:
Create a bash script ProjectAnalyze.sh to keep at the root of your git repo that at least the following functionality:
### Requirements:
1. Informs you if your local repo is up to date with the remote repo
2. Puts all uncommited changes in a file changes.log
3. Puts each line from every file of your project with the tag #TODO into a file todo.log
4. Checks all haskell files for syntax errors and puts the results into error.log
### Addition Features:
1. File size breakdown?
### Instructions:
1. Keep and run script in root of git repo
2. Will notify if your local repo is upto date with remote repo. If not up to date you must execute git pull to update
3. Will create changes.log that has all uncommited changes
4. Will create todo.log that has the total number "of #TODO's" in files as well as each instance that is found
5. Will create error.log that will show syntax errors of all .hs (haskell) files
### Functions:
#### up_to_date

#### uncommited_changes
Corresponds to requirement #2. \n
Creates changes.log and redirects the output of the git diff with the --staged flag to show all the differences in the added files only.
#### todo
Corresponds to requirement #3. \n
Creates todo.log then appends total numbers of "#TODO's" using the git grep --count flag then appends every instance using git grep with no flags.
#### haskell_errors

### Citations:
* idea of starting prompt from classmate Noa Barsky at https://github.com/barskyn/CS1XA3/blob/master/Assign1/ProjectAnalyze.sh
