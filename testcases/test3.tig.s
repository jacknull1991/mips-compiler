L540:
addi $rd, $rs1, 8
move $rd, $rs
li $rd, 2
li $rd, 4
mul $rd, $rs1, $rs2
move $rd, $rs
jal malloc
move $rd, $rs
li $rd, 1000
li $rd, 1
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
la $rd, L537
li $rd, 0
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
sw $rs, 0($rt)
la $rd, L538
lw $rd, 8($rt)
sw $rs, 0($rt)
lw $rd, 8($rt)
li $rd, 0
j L539
L539:
L536: .ascii "Nobody"
L537: .ascii "Nobody"
L538: .ascii "Somebody"
