L169:
li $rd, 1
li $rd, 10
li $rd, 20
bgt $rs1, $rs2, L163
L164:
li $rd, 0
L163:
li $rd, 0
li $rd, 0
bne $rs1, $rs2, L165
L166:
li $rd, 40
L167:
li $rd, 0
j L168
L165:
li $rd, 30
j L167
L168:
