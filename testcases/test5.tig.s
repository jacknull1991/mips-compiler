L550:
addi $rd, $rs1, 8
move $rd, $rs
li $rd, 2
li $rd, 4
mul $rd, $rs1, $rs2
move $rd, $rs
jal malloc
move $rd, $rs
li $rd, 0
li $rd, 1
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
li $rd, 0
li $rd, 0
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
sw $rs, 0($rt)
li $rd, 0
j L549
L549:
