L7:
li $rd, 1
li $rd, 10
li $rd, 5
bgt $rs1, $rs2, L5
L6:
li $rd, 0
L5:
li $rd, 0
li $rd, 0
bne $rs1, $rs2, L8
L9:
li $rd, 0
j L10
L8:
li $rd, 5
addi $rd, $rs1, 6
j L7
L10:
