# PRMtools
PRMtools is a repository used for manipulating SAV files.

It is designed by PRM.


## General functions:
- filt_data(): This function removes the answers that have said one specific code more times
 that max_times. For example, in a poll we descart people who said 'Ns/Nr' more
 than 12 times.

## SPSS functions:
These functions try to approximate the way that SPSS work.

- COUNT: Replicates COUNT function.
- FRE: Replicates the FREQUENCIES function.