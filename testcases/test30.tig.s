L20:
addi $rd, $rs1, 8
move $rd, $rs
li $rd, 11
move $rd, $rs
li $rd, 0
move $rd, $rs
jal initArray
move $rd, $rs
li $rd, 11
sw $rs, 0($rt)
addi $rd, $rs1, 4
sw $rs, 0($rt)
lw $rd, 8($rt)
li $rd, 2
li $rd, 4
sub $rd, $rs1, $rs2
lw $rd, 0($rt)
blt $rs1, $rs2, L17
L18:
li $rd, 1
move $rd, $rs
jal exit
L17:
lw $rd, 8($rt)
lw $rd, 0($rt)
li $rd, 2
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
lw $rd, 0($rt)
li $rd, 0
j L19
L19:
