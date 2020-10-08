#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /home/ixaker/code/SheduledRepair/unit-tests/test
OFS=$IFS
IFS="
"
/usr/bin/ld.bfd -b elf64-x86-64 -m elf_x86_64  --dynamic-linker=/lib64/ld-linux-x86-64.so.2    -L. -o /home/ixaker/code/SheduledRepair/unit-tests/test /home/ixaker/code/SheduledRepair/unit-tests/link.res
if [ $? != 0 ]; then DoExitLink /home/ixaker/code/SheduledRepair/unit-tests/test; fi
IFS=$OFS
