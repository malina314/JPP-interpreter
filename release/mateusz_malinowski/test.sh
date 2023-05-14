#!/bin/bash

TMP_OUT=$(mktemp)
TMP_ERR=$(mktemp)

echo "Testing good files"
for file in ./good/*.jpp
do
    echo "Testing $file"
	./interpreter $file 1>$TMP_OUT 2>$TMP_ERR

	if diff ${file%jpp}out $TMP_OUT>/dev/null
	then echo -e "OK";
	else echo -e "ERROR IN stdout"
    fi

	if diff ${file%jpp}err $TMP_ERR>/dev/null
	then echo -e "OK";
	else echo -e "ERROR IN stderr"
    fi

	echo ""
done


echo "Testing bad files"
for file in ./bad/*.jpp
do
    echo "Testing $file"
	./interpreter -v $file 1>$TMP_OUT 2>$TMP_ERR

	if diff ${file%jpp}out $TMP_OUT>/dev/null
	then echo -e "OK";
	else echo -e "ERROR IN stdout"
    fi

	if diff ${file%jpp}err $TMP_ERR>/dev/null
	then echo -e "OK";
	else echo -e "ERROR IN stderr"
    fi

	echo ""
done
