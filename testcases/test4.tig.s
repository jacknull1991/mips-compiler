L546:
move $rd, $rs
li $rd, 10
move $rd, $rs
li $rd, 1
move $rd, $rs
li $rd, 1
move $rd, $rs
li $rd, 1
sw $rs, 0($rt)
li $rd, 1
sw $rs, 4($rt)
li $rd, 1
sw $rs, 8($rt)
jal L541
li $rd, 0
j L545
L545:
L548:
lw $rd, 28($rt)
li $rd, 0
beq $rs1, $rs2, L542
L543:
lw $rd, 28($rt)
li $rd, 0
mul $rd, $rs1, $rs2
move $rd, $rs
L544:
move $rd, $rs
j L547
L542:
li $rd, 1
j L544
L547:
