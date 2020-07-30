L1189:
sw $rs, 4($rt)
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
addi $rd, $rs1, 24
move $rd, $rs
lw $rd, 8($rt)
lw $rd, 8($rt)
add $rd, $rs1, $rs2
li $rd, 1
sub $rd, $rs1, $rs2
addi $rd, $rs1, 1
move $rd, $rs
li $rd, 0
move $rd, $rs
jal initArray
move $rd, $rs
lw $rd, 8($rt)
lw $rd, 8($rt)
add $rd, $rs1, $rs2
li $rd, 1
sub $rd, $rs1, $rs2
sw $rs, 0($rt)
addi $rd, $rs1, 4
sw $rs, 0($rt)
addi $rd, $rs1, 20
move $rd, $rs
lw $rd, 8($rt)
lw $rd, 8($rt)
add $rd, $rs1, $rs2
li $rd, 1
sub $rd, $rs1, $rs2
addi $rd, $rs1, 1
move $rd, $rs
li $rd, 0
move $rd, $rs
jal initArray
move $rd, $rs
lw $rd, 8($rt)
lw $rd, 8($rt)
add $rd, $rs1, $rs2
li $rd, 1
sub $rd, $rs1, $rs2
sw $rs, 0($rt)
addi $rd, $rs1, 4
sw $rs, 0($rt)
addi $rd, $rs1, 16
move $rd, $rs
lw $rd, 8($rt)
addi $rd, $rs1, 1
move $rd, $rs
li $rd, 0
move $rd, $rs
jal initArray
move $rd, $rs
lw $rd, 8($rt)
sw $rs, 0($rt)
addi $rd, $rs1, 4
sw $rs, 0($rt)
addi $rd, $rs1, 12
move $rd, $rs
lw $rd, 8($rt)
addi $rd, $rs1, 1
move $rd, $rs
li $rd, 0
move $rd, $rs
jal initArray
move $rd, $rs
lw $rd, 8($rt)
sw $rs, 0($rt)
addi $rd, $rs1, 4
sw $rs, 0($rt)
li $rd, 8
sw $rs, 8($rt)
move $rd, $rs
li $rd, 0
move $rd, $rs
jal L1126
li $rd, 0
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
j L1188
L1188:
L1191:
sw $rs, 4($rt)
sw $rs, 8($rt)
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
lw $rd, 36($rt)
lw $rd, 8($rt)
beq $rs1, $rs2, L1185
L1186:
li $rd, 0
sw $rs, 40($rt)
lw $rd, 40($rt)
lw $rd, 8($rt)
li $rd, 1
sub $rd, $rs1, $rs2
ble $rs1, $rs2, L1181
L1184:
li $rd, 0
L1187:
li $rd, 0
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
j L1190
L1185:
move $rd, $rs
jal L1127
move $rd, $rs
j L1187
L1183:
lw $rd, 40($rt)
addi $rd, $rs1, 1
sw $rs, 40($rt)
L1181:
lw $rd, 12($rt)
lw $rd, 40($rt)
lw $rd, -4($rt)
blt $rs1, $rs2, L1149
L1150:
li $rd, 1
move $rd, $rs
jal exit
L1149:
lw $rd, 12($rt)
lw $rd, 0($rt)
lw $rd, 40($rt)
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
lw $rd, 0($rt)
li $rd, 0
beq $rs1, $rs2, L1155
L1156:
li $rd, 0
L1157:
li $rd, 0
bne $rs1, $rs2, L1162
L1163:
li $rd, 0
L1164:
li $rd, 0
bne $rs1, $rs2, L1179
L1180:
lw $rd, 40($rt)
lw $rd, 8($rt)
li $rd, 1
sub $rd, $rs1, $rs2
blt $rs1, $rs2, L1183
L1192:
j L1184
L1155:
li $rd, 1
lw $rd, 20($rt)
lw $rd, 40($rt)
lw $rd, 36($rt)
add $rd, $rs1, $rs2
move $rd, $rs
lw $rd, -4($rt)
blt $rs1, $rs2, L1151
L1152:
li $rd, 1
move $rd, $rs
jal exit
L1151:
lw $rd, 20($rt)
lw $rd, 0($rt)
lw $rd, 40($rt)
lw $rd, 36($rt)
add $rd, $rs1, $rs2
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
lw $rd, 0($rt)
li $rd, 0
beq $rs1, $rs2, L1153
L1154:
li $rd, 0
L1153:
move $rd, $rs
j L1157
L1162:
li $rd, 1
lw $rd, 24($rt)
lw $rd, 40($rt)
addi $rd, $rs1, 7
lw $rd, 36($rt)
sub $rd, $rs1, $rs2
move $rd, $rs
lw $rd, -4($rt)
blt $rs1, $rs2, L1158
L1159:
li $rd, 1
move $rd, $rs
jal exit
L1158:
lw $rd, 24($rt)
lw $rd, 0($rt)
lw $rd, 40($rt)
addi $rd, $rs1, 7
lw $rd, 36($rt)
sub $rd, $rs1, $rs2
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
lw $rd, 0($rt)
li $rd, 0
beq $rs1, $rs2, L1160
L1161:
li $rd, 0
L1160:
move $rd, $rs
j L1164
L1179:
lw $rd, 12($rt)
lw $rd, 40($rt)
lw $rd, -4($rt)
blt $rs1, $rs2, L1165
L1166:
li $rd, 1
move $rd, $rs
jal exit
L1165:
li $rd, 1
lw $rd, 12($rt)
lw $rd, 0($rt)
lw $rd, 40($rt)
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
lw $rd, 20($rt)
lw $rd, 40($rt)
lw $rd, 36($rt)
add $rd, $rs1, $rs2
move $rd, $rs
lw $rd, -4($rt)
blt $rs1, $rs2, L1167
L1168:
li $rd, 1
move $rd, $rs
jal exit
L1167:
li $rd, 1
lw $rd, 20($rt)
lw $rd, 0($rt)
lw $rd, 40($rt)
lw $rd, 36($rt)
add $rd, $rs1, $rs2
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
lw $rd, 24($rt)
lw $rd, 40($rt)
addi $rd, $rs1, 7
lw $rd, 36($rt)
sub $rd, $rs1, $rs2
move $rd, $rs
lw $rd, -4($rt)
blt $rs1, $rs2, L1169
L1170:
li $rd, 1
move $rd, $rs
jal exit
L1169:
li $rd, 1
lw $rd, 24($rt)
lw $rd, 0($rt)
lw $rd, 40($rt)
addi $rd, $rs1, 7
lw $rd, 36($rt)
sub $rd, $rs1, $rs2
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
lw $rd, 16($rt)
lw $rd, 36($rt)
lw $rd, -4($rt)
blt $rs1, $rs2, L1171
L1172:
li $rd, 1
move $rd, $rs
jal exit
L1171:
lw $rd, 40($rt)
lw $rd, 16($rt)
lw $rd, 0($rt)
lw $rd, 36($rt)
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
move $rd, $rs
lw $rd, 36($rt)
addi $rd, $rs1, 1
move $rd, $rs
jal L1126
lw $rd, 12($rt)
lw $rd, 40($rt)
lw $rd, -4($rt)
blt $rs1, $rs2, L1173
L1174:
li $rd, 1
move $rd, $rs
jal exit
L1173:
li $rd, 0
lw $rd, 12($rt)
lw $rd, 0($rt)
lw $rd, 40($rt)
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
lw $rd, 20($rt)
lw $rd, 40($rt)
lw $rd, 36($rt)
add $rd, $rs1, $rs2
move $rd, $rs
lw $rd, -4($rt)
blt $rs1, $rs2, L1175
L1176:
li $rd, 1
move $rd, $rs
jal exit
L1175:
li $rd, 0
lw $rd, 20($rt)
lw $rd, 0($rt)
lw $rd, 40($rt)
lw $rd, 36($rt)
add $rd, $rs1, $rs2
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
lw $rd, 24($rt)
lw $rd, 40($rt)
addi $rd, $rs1, 7
lw $rd, 36($rt)
sub $rd, $rs1, $rs2
move $rd, $rs
lw $rd, -4($rt)
blt $rs1, $rs2, L1177
L1178:
li $rd, 1
move $rd, $rs
jal exit
L1177:
li $rd, 0
lw $rd, 24($rt)
lw $rd, 0($rt)
lw $rd, 40($rt)
addi $rd, $rs1, 7
lw $rd, 36($rt)
sub $rd, $rs1, $rs2
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
sw $rs, 0($rt)
j L1180
L1190:
L1194:
sw $rs, 4($rt)
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
li $rd, 0
sw $rs, 28($rt)
lw $rd, 28($rt)
lw $rd, 8($rt)
li $rd, 1
sub $rd, $rs1, $rs2
ble $rs1, $rs2, L1145
L1148:
la $rd, L1144
move $rd, $rs
jal print
li $rd, 0
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
move $rd, $rs
j L1193
L1147:
lw $rd, 28($rt)
addi $rd, $rs1, 1
sw $rs, 28($rt)
L1145:
li $rd, 0
sw $rs, 32($rt)
lw $rd, 32($rt)
lw $rd, 8($rt)
li $rd, 1
sub $rd, $rs1, $rs2
ble $rs1, $rs2, L1140
L1143:
la $rd, L1144
move $rd, $rs
jal print
lw $rd, 28($rt)
lw $rd, 8($rt)
li $rd, 1
sub $rd, $rs1, $rs2
blt $rs1, $rs2, L1147
L1195:
j L1148
L1142:
lw $rd, 32($rt)
addi $rd, $rs1, 1
sw $rs, 32($rt)
L1140:
lw $rd, 16($rt)
lw $rd, 28($rt)
lw $rd, -4($rt)
blt $rs1, $rs2, L1128
L1129:
li $rd, 1
move $rd, $rs
jal exit
L1128:
lw $rd, 16($rt)
lw $rd, 0($rt)
lw $rd, 28($rt)
li $rd, 4
mul $rd, $rs1, $rs2
add $rd, $rs1, $rs2
lw $rd, 0($rt)
lw $rd, 32($rt)
beq $rs1, $rs2, L1132
L1133:
la $rd, L1131
L1134:
move $rd, $rs
jal print
lw $rd, 32($rt)
lw $rd, 8($rt)
li $rd, 1
sub $rd, $rs1, $rs2
blt $rs1, $rs2, L1142
L1196:
j L1143
L1132:
la $rd, L1130
j L1134
L1193:
L1130: .ascii " O"
L1131: .ascii " ."
L1144: .ascii "
"
