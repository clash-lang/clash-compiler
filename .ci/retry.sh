#!/bin/bash
n=0
until [ "$n" -ge 5 ]
do
   $@ && exit 0
   n=$((n+1))
   sleep ${n}
done

exit 1
