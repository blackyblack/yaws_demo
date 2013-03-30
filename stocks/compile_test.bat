@echo off
echo compiling...
FOR %%f in (src/*.erl) DO erlc -Wall -I yaws/include/ -I include/ -DTEST -o ebin src/%%f
echo ok.