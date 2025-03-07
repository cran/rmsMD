# Snapshot: Feed non-rms modelfit

         variable                 coef_95CI Pvalue
    1 (Intercept) 18.672 (17.839 to 19.504) <0.001
    2         Age   0.009 (-0.029 to 0.046)  0.649
    3    ExerNone  -0.178 (-1.007 to 0.650)  0.673
    4    ExerSome  -0.391 (-0.899 to 0.118)  0.132

# Snapshot: Warning output for non-rms model without setting exp_coef

    The model fit does not belong to the 'rms' class. You must specify exp_coef argument to determine table output.

# Snapshot: OLS tests - simple model

         variable                 coef_95CI Pvalue
    1         Age   0.010 (-0.020 to 0.039)  0.523
    2   Exer=None   0.243 (-0.456 to 0.942)  0.495
    3   Exer=Some   0.192 (-0.231 to 0.615)  0.373
    4 Smoke=Never -0.962 (-1.875 to -0.050)  0.039
    5 Smoke=Occas -1.326 (-2.455 to -0.197)  0.021
    6 Smoke=Regul  -0.639 (-1.802 to 0.524)  0.282
    7      Height    0.069 (0.042 to 0.096) <0.001
    8    Sex=Male    1.418 (0.889 to 1.948) <0.001

# Snapshot: OLS tests - model with interactions

             variable                coef_95CI Pvalue
    1             Age  0.031 (-0.025 to 0.087)  0.284
    2       Exer=None  0.342 (-2.391 to 3.076)  0.806
    3       Exer=Some  0.484 (-1.200 to 2.168)  0.573
    4 Age * Exer=None -0.025 (-0.148 to 0.097)  0.685
    5 Age * Exer=Some -0.043 (-0.123 to 0.036)  0.285

# Snapshot: OLS tests - model with splines

    $simple
              variable                coef_95CI Pvalue
    1        Exer=None -0.277 (-1.098 to 0.544)  0.508
    2        Exer=Some -0.389 (-0.893 to 0.115)  0.130
    3 RCSoverallP: Age                RCS terms  0.048
    
    $full
       variable                   coef_95CI Pvalue
    1       Age      1.116 (0.311 to 1.921)  0.007
    2      Age' -35.856 (-64.042 to -7.670)  0.013
    3     Age''  64.472 (13.384 to 115.559)  0.013
    4 Exer=None    -0.277 (-1.098 to 0.544)  0.508
    5 Exer=Some    -0.389 (-0.893 to 0.115)  0.130
    

# Snapshot: OLS tests - model with splines and interactions

    $summary
                       variable                  coef_95CI Pvalue
    1                 Exer=None -3.885 (-50.286 to 42.515)  0.870
    2                 Exer=Some  23.159 (-1.786 to 48.104)  0.069
    3                  Sex=Male     2.120 (1.705 to 2.536) <0.001
    4        RCSoverallP: Age                    RCS terms  0.352
    5 RCSoverallP: Age * Exer                    RCS terms  0.251
    
    $full
                variable                     coef_95CI Pvalue
    1                Age        1.083 (0.031 to 2.135)  0.044
    2               Age'    -36.061 (-72.524 to 0.402)  0.053
    3              Age''    65.108 (-0.908 to 131.124)  0.053
    4          Exer=None    -3.885 (-50.286 to 42.515)  0.870
    5          Exer=Some     23.159 (-1.786 to 48.104)  0.069
    6           Sex=Male        2.120 (1.705 to 2.536) <0.001
    7    Age * Exer=None       0.173 (-2.441 to 2.788)  0.897
    8   Age' * Exer=None   11.499 (-81.943 to 104.941)  0.809
    9  Age'' * Exer=None -23.242 (-193.068 to 146.584)  0.789
    10   Age * Exer=Some      -1.305 (-2.716 to 0.107)  0.070
    11  Age' * Exer=Some     41.329 (-8.217 to 90.875)  0.102
    12 Age'' * Exer=Some  -74.331 (-164.140 to 15.477)  0.105
    

# Snapshot: LRM tests - simple model

    [[1]]
         variable                OR_95CI Pvalue
    1         Age 1.016 (0.968 to 1.066)  0.525
    2   Exer=None 1.780 (0.499 to 6.353)  0.374
    3   Exer=Some 1.094 (0.512 to 2.341)  0.816
    4 Smoke=Never 0.243 (0.046 to 1.278)  0.095
    5 Smoke=Occas 0.313 (0.042 to 2.332)  0.257
    6 Smoke=Regul 0.378 (0.040 to 3.596)  0.397
    7      Height 1.113 (1.053 to 1.176) <0.001
    8    Sex=Male 4.191 (1.812 to 9.693) <0.001
    
    [[2]]
         variable                OR_95CI Pvalue
    1         Age 1.016 (0.968 to 1.066)  0.525
    2   Exer=None 1.780 (0.499 to 6.353)  0.374
    3   Exer=Some 1.094 (0.512 to 2.341)  0.816
    4 Smoke=Never 0.243 (0.046 to 1.278)  0.095
    5 Smoke=Occas 0.313 (0.042 to 2.332)  0.257
    6 Smoke=Regul 0.378 (0.040 to 3.596)  0.397
    7      Height 1.113 (1.053 to 1.176) <0.001
    8    Sex=Male 4.191 (1.812 to 9.693) <0.001
    

# Snapshot: LRM tests - model with interactions

    [[1]]
             variable                 OR_95CI Pvalue
    1             Age  1.015 (0.954 to 1.081)  0.636
    2       Exer=None 1.203 (0.063 to 22.853)  0.902
    3       Exer=Some  0.795 (0.125 to 5.045)  0.807
    4 Age * Exer=None  0.979 (0.857 to 1.118)  0.753
    5 Age * Exer=Some  0.985 (0.903 to 1.076)  0.743
    
    [[2]]
             variable                 OR_95CI Pvalue
    1             Age  1.015 (0.954 to 1.081)  0.636
    2       Exer=None 1.203 (0.063 to 22.853)  0.902
    3       Exer=Some  0.795 (0.125 to 5.045)  0.807
    4 Age * Exer=None  0.979 (0.857 to 1.118)  0.753
    5 Age * Exer=Some  0.985 (0.903 to 1.076)  0.743
    

# Snapshot: LRM tests - model with splines

    [[1]]
              variable                OR_95CI Pvalue
    1        Exer=None 0.710 (0.290 to 1.735)  0.452
    2        Exer=Some 0.574 (0.329 to 1.001)  0.050
    3 RCSoverallP: Age              RCS terms  0.249
    
    [[2]]
              variable                OR_95CI Pvalue
    1        Exer=None 0.710 (0.290 to 1.735)  0.452
    2        Exer=Some 0.574 (0.329 to 1.001)  0.050
    3 RCSoverallP: Age              RCS terms  0.249
    

# Snapshot: CPH tests - simple model

    [[1]]
      variable                HR_95CI Pvalue
    1      age 1.017 (0.999 to 1.036)  0.065
    2      sex 0.599 (0.431 to 0.831)  0.002
    
    [[2]]
      variable                HR_95CI Pvalue
    1      age 1.017 (0.999 to 1.036)  0.065
    2      sex 0.599 (0.431 to 0.831)  0.002
    

# Snapshot: CPH tests - model with interactions

    [[1]]
       variable                 HR_95CI Pvalue
    1       age  1.030 (0.976 to 1.088)  0.279
    2       sex 1.100 (0.099 to 12.219)  0.938
    3 age * sex  0.990 (0.953 to 1.029)  0.618
    
    [[2]]
       variable                 HR_95CI Pvalue
    1       age  1.030 (0.976 to 1.088)  0.279
    2       sex 1.100 (0.099 to 12.219)  0.938
    3 age * sex  0.990 (0.953 to 1.029)  0.618
    

# Snapshot: CPH tests - model with splines

    [[1]]
              variable                HR_95CI Pvalue
    1              sex 0.603 (0.434 to 0.838)  0.003
    2 RCSoverallP: age              RCS terms  0.135
    
    [[2]]
              variable                HR_95CI Pvalue
    1              sex 0.603 (0.434 to 0.838)  0.003
    2 RCSoverallP: age              RCS terms  0.135
    

# Snapshot: CPH tests - model with splines and interactions

    [[1]]
                      variable                  HR_95CI Pvalue
    1                      sex 0.048 (0.000 to 131.415)  0.452
    2       RCSoverallP: age                  RCS terms  0.375
    3 RCSoverallP: age * sex                  RCS terms  0.839
    
    [[2]]
                      variable                  HR_95CI Pvalue
    1                      sex 0.048 (0.000 to 131.415)  0.452
    2       RCSoverallP: age                  RCS terms  0.375
    3 RCSoverallP: age * sex                  RCS terms  0.839
    

# Snapshot: Variables with labels and special names

    $summary
                       variable                  coef_95CI Pvalue
    1                 Exer=None -3.870 (-50.426 to 42.685)  0.871
    2                 Exer=Some  22.829 (-2.232 to 47.891)  0.074
    3                  Sex=Male     2.113 (1.696 to 2.530) <0.001
    4                   random1   -0.066 (-0.283 to 0.151)  0.549
    5                 "random2"   -0.063 (-0.264 to 0.137)  0.537
    6        RCSoverallP: Age                    RCS terms  0.372
    7 RCSoverallP: Age * Exer                    RCS terms  0.274
    
    $hidden
                       variable                  coef_95CI Pvalue
    1                 Exer=None -3.870 (-50.426 to 42.685)  0.871
    2                 Exer=Some  22.829 (-2.232 to 47.891)  0.074
    3                  Sex=Male     2.113 (1.696 to 2.530) <0.001
    4                   random1   -0.066 (-0.283 to 0.151)  0.549
    5                 "random2"   -0.063 (-0.264 to 0.137)  0.537
    6        RCSoverallP: Age                    RCS terms  0.372
    7 RCSoverallP: Age * Exer                    RCS terms  0.274
    

# Snapshot: Variables with reserved/special names

    $structure
     [1] "'data.frame':\t237 obs. of  17 variables:"                                       
     [2] " $ Sex   : Factor w/ 2 levels \"Female\",\"Male\": 1 2 2 2 2 1 2 1 2 2 ..."      
     [3] " $ Wr.Hnd: num  18.5 19.5 18 18.8 20 18 17.7 17 20 18.5 ..."                     
     [4] " $ NW.Hnd: num  18 20.5 13.3 18.9 20 17.7 17.7 17.3 19.5 18.5 ..."               
     [5] " $ W.Hnd : Factor w/ 2 levels \"Left\",\"Right\": 2 1 2 2 2 2 2 2 2 2 ..."       
     [6] " $ Fold  : Factor w/ 3 levels \"L on R\",\"Neither\",..: 3 3 1 3 2 1 1 3 3 3 ..."
     [7] " $ Pulse : int  92 104 87 NA 35 64 83 74 72 90 ..."                              
     [8] " $ Clap  : Factor w/ 3 levels \"Left\",\"Neither\",..: 1 1 2 2 3 3 3 3 3 3 ..."  
     [9] " $ Exer  : Factor w/ 3 levels \"Freq\",\"None\",..: 3 2 2 2 3 3 1 1 3 3 ..."     
    [10] " $ Smoke : Factor w/ 4 levels \"Heavy\",\"Never\",..: 2 4 3 2 2 2 2 2 2 2 ..."   
    [11] " $ Height: num  173 178 NA 160 165 ..."                                          
    [12] " $ M.I   : Factor w/ 2 levels \"Imperial\",\"Metric\": 2 1 NA 2 2 1 1 2 2 2 ..." 
    [13] " $ Age   : num  18.2 17.6 16.9 20.3 23.7 ..."                                    
    [14] " $ if    : num  -0.214 0.152 1.712 -0.326 0.373 ..."                             
    [15] "  ..- attr(*, \"label\")= chr \"Random variable with name 'if'\""                
    [16] " $ for   : num  -0.2128 -0.0936 -0.0867 1.4415 1.1251 ..."                       
    [17] "  ..- attr(*, \"label\")= chr \"Random variable with name 'for'\""               
    [18] " $ while : num  -0.272 -1.214 -0.141 -1.005 0.156 ..."                           
    [19] "  ..- attr(*, \"label\")= chr \"Random variable with name 'while'\""             
    [20] " $ TRUE  : num  1.168 -0.823 -0.307 1.44 -2.199 ..."                             
    [21] "  ..- attr(*, \"label\")= chr \"Random variable with name 'TRUE'\""              
    [22] " $ NULL  : num  0.29256 0.66875 -0.59418 1.58043 -0.00399 ..."                   
    [23] "  ..- attr(*, \"label\")= chr \"Random variable with name 'NULL'\""              
    
    $summary
                        variable                  coef_95CI Pvalue
    1                  Exer=None -1.015 (-48.194 to 46.163)  0.966
    2                  Exer=Some  24.099 (-1.229 to 49.426)  0.062
    3                   Sex=Male     2.127 (1.705 to 2.549) <0.001
    4                         if   -0.190 (-0.407 to 0.027)  0.085
    5                        for   -0.028 (-0.228 to 0.173)  0.787
    6                      while   -0.058 (-0.269 to 0.153)  0.592
    7                       TRUE   -0.028 (-0.248 to 0.191)  0.802
    8                       NULL    0.057 (-0.147 to 0.262)  0.582
    9         RCSoverallP: Age                    RCS terms  0.485
    10 RCSoverallP: Age * Exer                    RCS terms  0.340
    
    $hidden
                        variable                  coef_95CI Pvalue
    1                  Exer=None -1.015 (-48.194 to 46.163)  0.966
    2                  Exer=Some  24.099 (-1.229 to 49.426)  0.062
    3                   Sex=Male     2.127 (1.705 to 2.549) <0.001
    4                         if   -0.190 (-0.407 to 0.027)  0.085
    5                        for   -0.028 (-0.228 to 0.173)  0.787
    6                      while   -0.058 (-0.269 to 0.153)  0.592
    7                       TRUE   -0.028 (-0.248 to 0.191)  0.802
    8                       NULL    0.057 (-0.147 to 0.262)  0.582
    9         RCSoverallP: Age                    RCS terms  0.485
    10 RCSoverallP: Age * Exer                    RCS terms  0.340
    

