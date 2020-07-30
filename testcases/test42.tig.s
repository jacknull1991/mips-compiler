L244:
addi $rd, $rs1, 24
move $rd, $rs
li $rd, 2
li $rd, 4
mul $rd, $rs1, $rs2
move $rd, $rs
jal malloc
move $rd, $rs
li $rd, 1
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
move $rd, $rs
li $rd, 3
addi $rd, $rs1, 1
move $rd, $rs
li $rd, 1900
move $rd, $rs
jal initArray
move $rd, $rs
li $rd, 3
sw $rs, 0($rt)
addi $rd, $rs1, 4
sw $rs, 0($rt)
la $rd, L242
li $rd, 0
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
sw $rs, 0($rt)
addi $rd, $rs1, 20
move $rd, $rs
li $rd, 4
li $rd, 4
mul $rd, $rs1, $rs2
move $rd, $rs
jal malloc
move $rd, $rs
li $rd, 44
li $rd, 3
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
li $rd, 2432
li $rd, 2
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
la $rd, L241
li $rd, 1
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
la $rd, L240
li $rd, 0
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
sw $rs, 0($rt)
addi $rd, $rs1, 16
move $rd, $rs
li $rd, 100
addi $rd, $rs1, 1
move $rd, $rs
la $rd, L239
move $rd, $rs
jal initArray
move $rd, $rs
li $rd, 100
sw $rs, 0($rt)
addi $rd, $rs1, 4
sw $rs, 0($rt)
addi $rd, $rs1, 12
move $rd, $rs
li $rd, 5
addi $rd, $rs1, 1
move $rd, $rs
li $rd, 4
li $rd, 4
mul $rd, $rs1, $rs2
move $rd, $rs
jal malloc
move $rd, $rs
li $rd, 0
li $rd, 3
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
li $rd, 0
li $rd, 2
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
la $rd, L238
li $rd, 1
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
la $rd, L237
li $rd, 0
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
move $rd, $rs
move $rd, $rs
jal initArray
move $rd, $rs
li $rd, 5
sw $rs, 0($rt)
addi $rd, $rs1, 4
sw $rs, 0($rt)
addi $rd, $rs1, 8
move $rd, $rs
li $rd, 10
addi $rd, $rs1, 1
move $rd, $rs
li $rd, 0
move $rd, $rs
jal initArray
move $rd, $rs
li $rd, 10
sw $rs, 0($rt)
addi $rd, $rs1, 4
sw $rs, 0($rt)
li $rd, 0
j L243
L243:
L237: .ascii "aname"
L238: .ascii "somewhere"
L239: .ascii ""
L240: .ascii "Kapoios"
L241: .ascii "Kapou"
L242: .ascii "Allos"
