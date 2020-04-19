#!/bin/sh

if [ "$1" = "" ]
then
    echo "You forgot to add the assignment name, e.g. 'Assessed1'."
    echo "Please run the script again with the right argument."
    exit 1
fi

if hostname -f | grep cca-\[ul\]g04 > /dev/null; then
    if ghc --version | grep 'version 8.6.5' > /dev/null; then
	echo 'Correct version of GHC found.'
    else
        echo "Unexpected version of GHC found; expected 8.6.5."
        echo "Please ensure you are on a lab machine and have run 'module load func-prog'."
        exit 1
    fi
else
    echo "Please run this script on a lab machine."
    exit 1
fi

if [ "$2" != "noGit" ]
then
    echo "Running git pull to make sure repository is up to date..."

    git pull

    if [ $? -ne 0 ]
    then
        echo ""
	echo "'git pull' failed."
	echo "Please make you are in the fp-learning folder and 'git pull' functions."
	exit 1
    fi

    git diff-index HEAD | grep Types.hs > /dev/null

    if [ $? -eq 0 ]
    then
	echo ""
	echo "It looks like you modified 'Types.hs'."
	echo "Please revert your changes by running:"
	echo "'git checkout Types.hs'"
	exit 1
    fi
fi

if ! [ -f "$1.hs" ]
then
    echo "File '$1.hs' not found."
    echo "Are you in the correct directory?"
    exit 1
fi

echo "Trying to compile your submission..."

# Create temporary directory
temp_dir=$(mktemp -d)

ghc $1.hs -odir $temp_dir -hidir $temp_dir

if [ $? -ne 0 ]
then
    echo ""
    echo "Your file '$1.hs' did not compile."
    echo "Please fix it before submitting."
    exit 1
fi

if ! [ -f "$temp_dir/$1.o" ]
then
    echo ""
    echo "The module name in '$1.hs' does match not the filename '$1'."
    echo "Please make sure you that"
    echo -e "\t(i) your file is called something like 'AssessedX.hs'"
    echo -e "\t(ii) you did not change the top of the template"
    echo "and try again."
    exit 1
fi

echo ""
echo "All checks passed."
echo "You are ready to submit!"

# Cleanup temporary directory
rm -r $temp_dir
