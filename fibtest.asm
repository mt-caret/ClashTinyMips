# fibtest.asm
# Register usage: $3: n $4: f1 $5: f2
# return value written to address 255
fib:                  # Assembly Code  Effect        Machine Code
  addi $3, $0, 8      # initialize n = 8    20030008
  addi $4, $0, 1      # initialize f1 = 1   20040001
  addi $5, $0, -1     # initialize f2 = -1    2005ffff
loop: beq $3, $0, end # Done with loop if n = 0 10600004
  add $4, $4, $5      # f1 = f1 + f2      00852020
  sub $5, $4, $5      # f2 = f1 - f2      00852822
  addi $3, $3, -1     # n = n - 1     2063ffff
  j loop              # repeat until done   08000003
end:  sb $4, 255($0)  # store result in address 255 a00400ff
