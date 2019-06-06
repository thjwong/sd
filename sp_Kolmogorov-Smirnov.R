print("Kolmogorov-Smirnov test - training: negative dist cumulative selection prob. vs optimal fullknow SecPro")
ks.test(subset(effortpropmean_stage0_zz, L1 %in% c("er41"))$value/100,subset(effortpropmean_stage0_zz, L1 %in% c("om41"))$value/100)

print("Kolmogorov-Smirnov test - training: negative dist cumulative selection prob. vs optimal Cayley Problem")
ks.test(subset(effortpropmean_stage0_zz, L1 %in% c("er41"))$value/100,subset(effortpropmean_stage0_zz, L1 %in% c("com41"))$value/100)

print("Kolmogorov-Smirnov test - training: optimal Cayley Problem vs optimal fullknow SecPro.")
ks.test(subset(effortpropmean_stage0_zz, L1 %in% c("om41"))$value/100,subset(effortpropmean_stage0_zz, L1 %in% c("com41"))$value/100)

print("Kolmogorov-Smirnov test - training: uniform dist cumulative selection prob. vs optimal fullknow SecPro")
ks.test(subset(effortpropmean_stage0_zz, L1 %in% c("er11"))$value/100,subset(effortpropmean_stage0_zz, L1 %in% c("om11"))$value/100)

print("Kolmogorov-Smirnov test - training: uniform dist cumulative selection prob. vs optimal Cayley Problem")
ks.test(subset(effortpropmean_stage0_zz, L1 %in% c("er11"))$value/100,subset(effortpropmean_stage0_zz, L1 %in% c("com11"))$value/100)

print("Kolmogorov-Smirnov test - training: optimal Cayley Problem vs optimal fullknow SecPro.")
ks.test(subset(effortpropmean_stage0_zz, L1 %in% c("om11"))$value/100,subset(effortpropmean_stage0_zz, L1 %in% c("com11"))$value/100)

print("Kolmogorov-Smirnov test - training: positive dist cumulative selection prob. vs optimal fullknow SecPro")
ks.test(subset(effortpropmean_stage0_zz, L1 %in% c("er14"))$value/100,subset(effortpropmean_stage0_zz, L1 %in% c("om14"))$value/100)

print("Kolmogorov-Smirnov test - training: positive dist cumulative selection prob. vs optimal Cayley Problem")
ks.test(subset(effortpropmean_stage0_zz, L1 %in% c("er14"))$value/100,subset(effortpropmean_stage0_zz, L1 %in% c("com14"))$value/100)

print("Kolmogorov-Smirnov test - training: optimal Cayley Problem vs optimal fullknow SecPro.")
ks.test(subset(effortpropmean_stage0_zz, L1 %in% c("om14"))$value/100,subset(effortpropmean_stage0_zz, L1 %in% c("com14"))$value/100)

print("Kolmogorov-Smirnov test - training: cumulative selection prob. positive dist. vs uniform dist.")
ks.test(subset(effortpropmean_stage0_zz, L1 %in% c("er41"))$value/100,subset(effortpropmean_stage0_zz, L1 %in% c("er11"))$value/100)

print("Kolmogorov-Smirnov test - training: cumulative selection prob. uniform dist. vs positive dist.")
ks.test(subset(effortpropmean_stage0_zz, L1 %in% c("er11"))$value/100,subset(effortpropmean_stage0_zz, L1 %in% c("er14"))$value/100)

print("Kolmogorov-Smirnov test - training: cumulative selection prob. positive dist. vs negative dist.")
ks.test(subset(effortpropmean_stage0_zz, L1 %in% c("er14"))$value/100,subset(effortpropmean_stage0_zz, L1 %in% c("er41"))$value/100)

print("Kolmogorov-Smirnov test - Test: negative dist cumulative selection prob. vs optimal fullknow SecPro")
ks.test(subset(effortpropmean_stage1_zz, L1 %in% c("er41"))$value/100,subset(effortpropmean_stage1_zz, L1 %in% c("om41"))$value/100)

print("Kolmogorov-Smirnov test - Test: negative dist cumulative selection prob. vs optimal Cayley Problem")
ks.test(subset(effortpropmean_stage1_zz, L1 %in% c("er41"))$value/100,subset(effortpropmean_stage1_zz, L1 %in% c("com41"))$value/100)

print("Kolmogorov-Smirnov test - Test: optimal Cayley Problem vs optimal fullknow SecPro.")
ks.test(subset(effortpropmean_stage1_zz, L1 %in% c("om41"))$value/100,subset(effortpropmean_stage1_zz, L1 %in% c("com41"))$value/100)

print("Kolmogorov-Smirnov test - Test: uniform dist cumulative selection prob. vs optimal fullknow SecPro")
ks.test(subset(effortpropmean_stage1_zz, L1 %in% c("er11"))$value/100,subset(effortpropmean_stage1_zz, L1 %in% c("om11"))$value/100)

print("Kolmogorov-Smirnov test - Test: uniform dist cumulative selection prob. vs optimal Cayley Problem")
ks.test(subset(effortpropmean_stage1_zz, L1 %in% c("er11"))$value/100,subset(effortpropmean_stage1_zz, L1 %in% c("com11"))$value/100)

print("Kolmogorov-Smirnov test - Test: optimal Cayley Problem vs optimal fullknow SecPro.")
ks.test(subset(effortpropmean_stage1_zz, L1 %in% c("om11"))$value/100,subset(effortpropmean_stage1_zz, L1 %in% c("com11"))$value/100)

print("Kolmogorov-Smirnov test - Test: positive dist cumulative selection prob. vs optimal fullknow SecPro")
ks.test(subset(effortpropmean_stage1_zz, L1 %in% c("er14"))$value/100,subset(effortpropmean_stage1_zz, L1 %in% c("om14"))$value/100)

print("Kolmogorov-Smirnov test - Test: positive dist cumulative selection prob. vs optimal Cayley Problem")
ks.test(subset(effortpropmean_stage1_zz, L1 %in% c("er14"))$value/100,subset(effortpropmean_stage1_zz, L1 %in% c("com14"))$value/100)

print("Kolmogorov-Smirnov test - Test: optimal Cayley Problem vs optimal fullknow SecPro.")
ks.test(subset(effortpropmean_stage1_zz, L1 %in% c("om14"))$value/100,subset(effortpropmean_stage1_zz, L1 %in% c("com14"))$value/100)

print("Kolmogorov-Smirnov test - Test: cumulative selection prob. positive dist. vs uniform dist.")
ks.test(subset(effortpropmean_stage1_zz, L1 %in% c("er41"))$value/100,subset(effortpropmean_stage1_zz, L1 %in% c("er11"))$value/100)

print("Kolmogorov-Smirnov test - Test: cumulative selection prob. uniform dist. vs positive dist.")
ks.test(subset(effortpropmean_stage1_zz, L1 %in% c("er11"))$value/100,subset(effortpropmean_stage1_zz, L1 %in% c("er14"))$value/100)

print("Kolmogorov-Smirnov test - Test: cumulative selection prob. positive dist. vs negative dist.")
ks.test(subset(effortpropmean_stage1_zz, L1 %in% c("er14"))$value/100,subset(effortpropmean_stage1_zz, L1 %in% c("er41"))$value/100)
