L11:
li $rd, 10
li $rd, 5
bgt $rs1, $rs2, L12
L13:
li $rd, 0
j L14
L12:
addi $rd, $rs1, 1
j L11
L14:
