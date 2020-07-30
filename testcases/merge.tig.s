L1518:
addi t4774, t4352, 56
move t4767, t4774
addi t4775, t4352, 8
move t4766, t4775
jal getchar
move t4765, t4354
sw t4765, 0(t4766)
move t4375, t4352
jal L1491
li t4776, 0
sw t4776, 0(t4767)
addi t4777, t4352, 52
move t4769, t4777
move t4375, t4352
jal L1491
move t4768, t4354
sw t4768, 0(t4769)
addi t4778, t4352, 8
move t4771, t4778
jal getchar
move t4770, t4354
sw t4770, 0(t4771)
move t4773, t4352
move t4375, t4352
lw t4779, 52(t4352)
move t4376, t4779
lw t4780, 56(t4352)
move t4377, t4780
jal L1490
move t4772, t4354
move t4375, t4773
move t4376, t4772
jal L1488
li t4354, 0
j L1517
L1517:
L1520:
lw t4781, 48(t4352)
li t4782, 0
beq t4781, t4782, L1514
L1515:
move t4375, t4352
lw t4784, 48(t4352)
lw t4783, 0(t4784)
move t4376, t4783
jal L1489
la t4785, L1477
move t4375, t4785
jal print
move t4375, t4352
lw t4787, 48(t4352)
lw t4786, 4(t4787)
move t4376, t4786
jal L1488
li t4764, 0
L1516:
move t4354, t4764
j L1519
L1514:
la t4788, L1478
move t4375, t4788
jal print
move t4764, t4354
j L1516
L1519:
L1522:
lw t4789, 40(t4352)
li t4790, 0
blt t4789, t4790, L1511
L1512:
lw t4791, 40(t4352)
li t4792, 0
bgt t4791, t4792, L1508
L1509:
la t4793, L1470
move t4375, t4793
jal print
move t4762, t4354
L1510:
move t4763, t4762
L1513:
li t4354, 0
j L1521
L1511:
la t4794, L1507
move t4375, t4794
jal print
move t4375, t4352
li t4796, 0
lw t4797, 40(t4352)
sub t4795, t4796, t4797
move t4376, t4795
jal L1504
li t4763, 0
j L1513
L1508:
move t4375, t4352
lw t4798, 40(t4352)
move t4376, t4798
jal L1504
move t4762, t4354
j L1510
L1521:
L1524:
lw t4802, 44(t4352)
li t4803, 0
bgt t4802, t4803, L1505
L1506:
li t4354, 0
j L1523
L1505:
move t4375, t4352
lw t4805, 44(t4352)
li t4806, 10
div t4804, t4805, t4806
move t4376, t4804
jal L1504
lw t4808, 44(t4352)
lw t4811, 44(t4352)
li t4812, 10
div t4810, t4811, t4812
li t4813, 10
mul t4809, t4810, t4813
sub t4807, t4808, t4809
move t4801, t4807
la t4814, L1470
move t4375, t4814
jal ord
move t4800, t4354
add t4815, t4801, t4800
move t4375, t4815
jal chr
move t4799, t4354
move t4375, t4799
jal print
j L1506
L1523:
L1526:
lw t4820, 36(t4352)
li t4821, 0
beq t4820, t4821, L1501
L1502:
lw t4822, 32(t4352)
li t4823, 0
beq t4822, t4823, L1498
L1499:
lw t4825, 36(t4352)
lw t4824, 0(t4825)
lw t4827, 32(t4352)
lw t4826, 0(t4827)
blt t4824, t4826, L1495
L1496:
li t4829, 2
li t4830, 4
mul t4828, t4829, t4830
move t4375, t4828
jal malloc
move t4758, t4354
li t4833, 1
li t4834, 4
mul t4832, t4833, t4834
add t4831, t4758, t4832
move t4819, t4831
move t4375, t4352
lw t4835, 36(t4352)
move t4376, t4835
lw t4837, 32(t4352)
lw t4836, 4(t4837)
move t4377, t4836
jal L1490
move t4818, t4354
sw t4818, 0(t4819)
lw t4839, 32(t4352)
lw t4838, 0(t4839)
li t4842, 0
li t4843, 4
mul t4841, t4842, t4843
add t4840, t4758, t4841
sw t4838, 0(t4840)
move t4759, t4758
L1497:
move t4760, t4759
L1500:
move t4761, t4760
L1503:
move t4354, t4761
j L1525
L1501:
lw t4761, 32(t4352)
j L1503
L1498:
lw t4760, 36(t4352)
j L1500
L1495:
li t4845, 2
li t4846, 4
mul t4844, t4845, t4846
move t4375, t4844
jal malloc
move t4757, t4354
li t4849, 1
li t4850, 4
mul t4848, t4849, t4850
add t4847, t4757, t4848
move t4817, t4847
move t4375, t4352
lw t4852, 36(t4352)
lw t4851, 4(t4852)
move t4376, t4851
lw t4853, 32(t4352)
move t4377, t4853
jal L1490
move t4816, t4354
sw t4816, 0(t4817)
lw t4855, 36(t4352)
lw t4854, 0(t4855)
li t4858, 0
li t4859, 4
mul t4857, t4858, t4859
add t4856, t4757, t4857
sw t4854, 0(t4856)
move t4759, t4757
j L1497
L1525:
L1528:
addi t4865, t4352, 28
move t4861, t4865
move t4375, t4352
lw t4866, 24(t4352)
move t4376, t4866
jal L1467
move t4860, t4354
sw t4860, 0(t4861)
addi t4867, t4352, 24
move t4862, t4867
li t4869, 1
li t4870, 4
mul t4868, t4869, t4870
move t4375, t4868
jal malloc
move t4754, t4354
li t4871, 0
li t4874, 0
li t4875, 4
mul t4873, t4874, t4875
add t4872, t4754, t4873
sw t4871, 0(t4872)
sw t4754, 0(t4862)
lw t4877, 24(t4352)
lw t4876, 0(t4877)
li t4878, 0
bne t4876, t4878, L1492
L1493:
li t4756, 0
L1494:
li t4354, 0
j L1527
L1492:
li t4880, 2
li t4881, 4
mul t4879, t4880, t4881
move t4375, t4879
jal malloc
move t4755, t4354
li t4884, 1
li t4885, 4
mul t4883, t4884, t4885
add t4882, t4755, t4883
move t4864, t4882
move t4375, t4352
jal L1491
move t4863, t4354
sw t4863, 0(t4864)
lw t4886, 28(t4352)
li t4889, 0
li t4890, 4
mul t4888, t4889, t4890
add t4887, t4755, t4888
sw t4886, 0(t4887)
move t4756, t4755
j L1494
L1527:
L1530:
li t4901, 0
sw t4901, 16(t4352)
move t4375, t4352
jal L1468
lw t4903, 12(t4352)
addi t4902, t4903, 0
move t4892, t4902
move t4375, t4352
lw t4904, 8(t4352)
move t4376, t4904
jal L1469
move t4891, t4354
sw t4891, 0(t4892)
L1485:
move t4375, t4352
lw t4905, 8(t4352)
move t4376, t4905
jal L1469
move t4893, t4354
li t4906, 0
bne t4893, t4906, L1486
L1487:
lw t4907, 16(t4352)
li t4354, 0
j L1529
L1486:
addi t4908, t4352, 16
move t4898, t4908
lw t4910, 16(t4352)
li t4911, 10
mul t4909, t4910, t4911
move t4895, t4909
lw t4912, 8(t4352)
move t4375, t4912
jal ord
move t4894, t4354
add t4913, t4895, t4894
move t4897, t4913
la t4914, L1470
move t4375, t4914
jal ord
move t4896, t4354
sub t4915, t4897, t4896
sw t4915, 0(t4898)
addi t4916, t4352, 8
move t4900, t4916
jal getchar
move t4899, t4354
sw t4899, 0(t4900)
j L1485
L1529:
L1482:
lw t4920, 8(t4352)
move t4375, t4920
la t4921, L1477
move t4376, t4921
jal stringEqual
move t4917, t4354
li t4922, 0
bne t4917, t4922, L1479
L1480:
lw t4923, 8(t4352)
move t4375, t4923
la t4924, L1478
move t4376, t4924
jal stringEqual
move t4753, t4354
L1481:
li t4925, 0
bne t4753, t4925, L1483
L1484:
li t4354, 0
j L1531
L1479:
li t4753, 1
j L1481
L1483:
addi t4926, t4352, 8
move t4919, t4926
jal getchar
move t4918, t4354
sw t4918, 0(t4919)
j L1482
L1531:
L1533:
lw t4933, 8(t4352)
move t4375, t4933
jal ord
move t4927, t4354
move t4929, t4927
la t4934, L1470
move t4375, t4934
jal ord
move t4928, t4354
bge t4929, t4928, L1474
L1475:
li t4752, 0
L1476:
move t4354, t4752
j L1532
L1474:
li t4751, 1
lw t4935, 8(t4352)
move t4375, t4935
jal ord
move t4930, t4354
move t4932, t4930
la t4936, L1471
move t4375, t4936
jal ord
move t4931, t4354
ble t4932, t4931, L1472
L1473:
li t4751, 0
L1472:
move t4752, t4751
j L1476
L1532:
L1470: .ascii "0"
L1471: .ascii "9"
L1477: .ascii " "
L1478: .ascii "
"
L1507: .ascii "-"
