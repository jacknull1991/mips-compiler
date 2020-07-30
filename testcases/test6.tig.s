L558:
move $rd, $rs
li $rd, 0
move $rd, $rs
la $rd, L555
move $rd, $rs
jal L552
li $rd, 0
j L557
L557:
L560:
move $rd, $rs
lw $rd, 16($rt)
move $rd, $rs
la $rd, L553
move $rd, $rs
jal L552
move $rd, $rs
j L559
L559:
L562:
move $rd, $rs
lw $rd, 12($rt)
addi $rd, $rs1, 1
move $rd, $rs
jal L551
move $rd, $rs
j L561
L561:
L553: .ascii "str"
L554: .ascii "str"
L555: .ascii "str2"
L556: .ascii "str2"
