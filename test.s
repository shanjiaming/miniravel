# ....

forstart:
 mul a1, a0, a1
 addi a0, a0, -1
 beqz a0, retblock
 j forstart

retblock:
 sw a1, 300(zero)
 ret

main:
 li a0, 5
 li a1, 1
 j forstart
