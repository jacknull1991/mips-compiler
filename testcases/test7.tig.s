L158:
li $rd, 0
move $rd, $rs
la $rd, L155
move $rd, $rs
jal L151
li $rd, 0
j L157
L157:
L160:
lw $rd, 16($rt)
move $rd, $rs
la $rd, L152
move $rd, $rs
jal L151
la $rd, L154
li $rd, 0
j L159
L159:
L162:
lw $rd, 12($rt)
addi $rd, $rs1, 1
move $rd, $rs
jal L150
li $rd, 0
j L161
L161:
L152: .ascii "str"
L153: .ascii "str"
L154: .ascii " "
L155: .ascii "str2"
L156: .ascii "str2"
