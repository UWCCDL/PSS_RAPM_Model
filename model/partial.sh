#!/bin/bash

expected=14401

if [ -f partial.txt ]; then
    rm partial.txt
fi

for j in simulations*tick*upper*.txt; do
    #echo $j
    n=`wc $j | awk '{print $1}'`
    
    if [ ! -f partial.txt ]; then
	head -1 $j >> partial.txt
    fi
    
    if [ $n -eq $expected ]; then
	m=$((n - 1))
	echo "Complete, using $m lines"
	tail -${m} $j >> partial.txt
    else
	p=$((n - 1))
	q=$((n - 2))
	echo "Incomplete, using $q lines"
	head -${p} $j | tail -${q} >> partial.txt
    fi
    #echo "done"
done

echo "done"
