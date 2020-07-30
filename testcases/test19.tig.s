L74:
la t302, L67
li t303, 0
move t123, t303
la t304, L71
move t124, t304
jal L67
move t102, t0
j L73
L73:
L76:
la t305, L67
li t306, 0
move t123, t306
la t307, L68
move t124, t307
jal L67
la t308, L70
move t102, t0
j L75
L75:
L78:
la t309, L66
lw t311, 12(t100)
addi t310, t311, 1
move t123, t310
jal L66
move t102, t0
j L77
L77:
L68: .ascii "str"
L69: .ascii "str"
L70: .ascii " "
L71: .ascii "str2"
L72: .ascii "str2"
