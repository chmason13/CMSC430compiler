; ModuleID = 'header.cpp'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%class.hamt = type { [7 x %class.KV], i64 }
%class.KV = type { %"union.KV<hKey, hKey, 0>::Key", %"union.KV<hKey, hKey, 0>::Val" }
%"union.KV<hKey, hKey, 0>::Key" = type { i64 }
%"union.KV<hKey, hKey, 0>::Val" = type { %class.KV.0* }
%class.KV.0 = type { %"union.KV<hKey, hKey, 1>::Key", %"union.KV<hKey, hKey, 1>::Val" }
%"union.KV<hKey, hKey, 1>::Key" = type { i64 }
%"union.KV<hKey, hKey, 1>::Val" = type { %class.KV.1* }
%class.KV.1 = type { %"union.KV<hKey, hKey, 2>::Key", %"union.KV<hKey, hKey, 2>::Val" }
%"union.KV<hKey, hKey, 2>::Key" = type { i64 }
%"union.KV<hKey, hKey, 2>::Val" = type { %class.KV.2* }
%class.KV.2 = type { %"union.KV<hKey, hKey, 3>::Key", %"union.KV<hKey, hKey, 3>::Val" }
%"union.KV<hKey, hKey, 3>::Key" = type { i64 }
%"union.KV<hKey, hKey, 3>::Val" = type { %class.KV.3* }
%class.KV.3 = type { %"union.KV<hKey, hKey, 4>::Key", %"union.KV<hKey, hKey, 4>::Val" }
%"union.KV<hKey, hKey, 4>::Key" = type { i64 }
%"union.KV<hKey, hKey, 4>::Val" = type { %class.KV.4* }
%class.KV.4 = type { %"union.KV<hKey, hKey, 5>::Key", %"union.KV<hKey, hKey, 5>::Val" }
%"union.KV<hKey, hKey, 5>::Key" = type { i64 }
%"union.KV<hKey, hKey, 5>::Val" = type { %class.KV.5* }
%class.KV.5 = type { %"union.KV<hKey, hKey, 6>::Key", %"union.KV<hKey, hKey, 6>::Val" }
%"union.KV<hKey, hKey, 6>::Key" = type { i64 }
%"union.KV<hKey, hKey, 6>::Val" = type { %class.KV.6* }
%class.KV.6 = type { %"union.KV<hKey, hKey, 7>::Key", %"union.KV<hKey, hKey, 7>::Val" }
%"union.KV<hKey, hKey, 7>::Key" = type { i64 }
%"union.KV<hKey, hKey, 7>::Val" = type { %class.KV.7* }
%class.KV.7 = type { %"union.KV<hKey, hKey, 8>::Key", %"union.KV<hKey, hKey, 8>::Val" }
%"union.KV<hKey, hKey, 8>::Key" = type { i64 }
%"union.KV<hKey, hKey, 8>::Val" = type { %class.KV.8* }
%class.KV.8 = type { %"union.KV<hKey, hKey, 9>::Key", %"union.KV<hKey, hKey, 9>::Val" }
%"union.KV<hKey, hKey, 9>::Key" = type { i64 }
%"union.KV<hKey, hKey, 9>::Val" = type { %class.KV.9* }
%class.KV.9 = type { %"union.KV<hKey, hKey, 10>::Key", %"union.KV<hKey, hKey, 10>::Val" }
%"union.KV<hKey, hKey, 10>::Key" = type { i64 }
%"union.KV<hKey, hKey, 10>::Val" = type { %class.LL* }
%class.LL = type { %class.hKey*, %class.hKey*, %class.LL* }
%class.hKey = type { i64 }

$_ZN4hKeyC2Em = comdat any

$_ZNK4hamtI4hKeyS0_E6insertEPKS0_S3_ = comdat any

$_ZN4hamtI4hKeyS0_EC2Ev = comdat any

$_ZNK4hamtI4hKeyS0_E3getEPKS0_ = comdat any

$_ZNK4hamtI4hKeyS0_E6removeEPKS0_ = comdat any

$_ZNK4hKey4hashEv = comdat any

$_ZN2KVI4hKeyS0_Lj0EEC2EPKS0_S3_ = comdat any

$_ZNK4hKeyeqERKS_ = comdat any

$_ZN2KVI4hKeyS0_Lj0EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj0EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI4hKeyS0_Lj0EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj0EE3ValC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj1EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj1EEC2ERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE = comdat any

$_ZN2KVI4hKeyS0_Lj1EEC2EPKS0_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj2EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj2EEC2ERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE = comdat any

$_ZN2KVI4hKeyS0_Lj2EEC2EPKS0_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj3EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj3EEC2ERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE = comdat any

$_ZN2KVI4hKeyS0_Lj3EEC2EPKS0_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj4EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj4EEC2ERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE = comdat any

$_ZN2KVI4hKeyS0_Lj4EEC2EPKS0_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj5EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj5EEC2ERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE = comdat any

$_ZN2KVI4hKeyS0_Lj5EEC2EPKS0_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj6EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj6EEC2ERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE = comdat any

$_ZN2KVI4hKeyS0_Lj6EEC2EPKS0_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj7EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj7EEC2ERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE = comdat any

$_ZN2KVI4hKeyS0_Lj7EEC2EPKS0_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj8EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj8EEC2ERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE = comdat any

$_ZN2KVI4hKeyS0_Lj8EEC2EPKS0_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj9EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj9EEC2ERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE = comdat any

$_ZN2KVI4hKeyS0_Lj9EEC2EPKS0_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj10EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI4hKeyS0_Lj10EEC2ERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE = comdat any

$_ZN2KVI4hKeyS0_Lj10EEC2EPKS0_S3_ = comdat any

$_ZN2LLI4hKeyS0_EC2EPKS0_S3_PKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj10EEC2EmPK2LLIS0_S0_E = comdat any

$_ZN2KVI4hKeyS0_Lj10EE3KeyC2Em = comdat any

$_ZN2KVI4hKeyS0_Lj10EE3ValC2EPK2LLIS0_S0_E = comdat any

$_ZN2KVI4hKeyS0_Lj9EE3KeyC2Em = comdat any

$_ZN2KVI4hKeyS0_Lj9EE3ValC2EPKS_IS0_S0_Lj10EE = comdat any

$_ZN2KVI4hKeyS0_Lj10EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj10EE3ValC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj8EE3KeyC2Em = comdat any

$_ZN2KVI4hKeyS0_Lj8EE3ValC2EPKS_IS0_S0_Lj9EE = comdat any

$_ZN2KVI4hKeyS0_Lj9EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj9EE3ValC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj7EE3KeyC2Em = comdat any

$_ZN2KVI4hKeyS0_Lj7EE3ValC2EPKS_IS0_S0_Lj8EE = comdat any

$_ZN2KVI4hKeyS0_Lj8EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj8EE3ValC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj6EE3KeyC2Em = comdat any

$_ZN2KVI4hKeyS0_Lj6EE3ValC2EPKS_IS0_S0_Lj7EE = comdat any

$_ZN2KVI4hKeyS0_Lj7EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj7EE3ValC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj5EE3KeyC2Em = comdat any

$_ZN2KVI4hKeyS0_Lj5EE3ValC2EPKS_IS0_S0_Lj6EE = comdat any

$_ZN2KVI4hKeyS0_Lj6EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj6EE3ValC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj4EE3KeyC2Em = comdat any

$_ZN2KVI4hKeyS0_Lj4EE3ValC2EPKS_IS0_S0_Lj5EE = comdat any

$_ZN2KVI4hKeyS0_Lj5EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj5EE3ValC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj3EE3KeyC2Em = comdat any

$_ZN2KVI4hKeyS0_Lj3EE3ValC2EPKS_IS0_S0_Lj4EE = comdat any

$_ZN2KVI4hKeyS0_Lj4EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj4EE3ValC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj2EE3KeyC2Em = comdat any

$_ZN2KVI4hKeyS0_Lj2EE3ValC2EPKS_IS0_S0_Lj3EE = comdat any

$_ZN2KVI4hKeyS0_Lj3EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj3EE3ValC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj1EE3KeyC2Em = comdat any

$_ZN2KVI4hKeyS0_Lj1EE3ValC2EPKS_IS0_S0_Lj2EE = comdat any

$_ZN2KVI4hKeyS0_Lj2EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj2EE3ValC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj0EE3KeyC2Em = comdat any

$_ZN2KVI4hKeyS0_Lj0EE3ValC2EPKS_IS0_S0_Lj1EE = comdat any

$_ZN2KVI4hKeyS0_Lj1EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj1EE3ValC2EPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj1EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI4hKeyS0_Lj1EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI4hKeyS0_Lj2EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI4hKeyS0_Lj2EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI4hKeyS0_Lj3EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI4hKeyS0_Lj3EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI4hKeyS0_Lj4EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI4hKeyS0_Lj4EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI4hKeyS0_Lj5EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI4hKeyS0_Lj5EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI4hKeyS0_Lj6EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI4hKeyS0_Lj6EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI4hKeyS0_Lj7EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI4hKeyS0_Lj7EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI4hKeyS0_Lj8EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI4hKeyS0_Lj8EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI4hKeyS0_Lj9EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI4hKeyS0_Lj9EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI4hKeyS0_Lj10EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI4hKeyS0_Lj10EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZNK2LLI4hKeyS0_E6insertEPKS0_S3_Pm = comdat any

$_ZN2KVI4hKeyS0_Lj0EEC2Ev = comdat any

$_ZN2KVI4hKeyS0_Lj0EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj1EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj2EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj3EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj4EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj5EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj6EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj7EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj8EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj9EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj10EE10inner_findERKS1_mPKS0_ = comdat any

$_ZNK2LLI4hKeyS0_E4findEPKS0_ = comdat any

$_ZN2KVI4hKeyS0_Lj0EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI4hKeyS0_Lj0EEeqERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj0EEC2ERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj1EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI4hKeyS0_Lj1EEeqERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj2EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI4hKeyS0_Lj2EEeqERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj3EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI4hKeyS0_Lj3EEeqERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj4EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI4hKeyS0_Lj4EEeqERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj5EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI4hKeyS0_Lj5EEeqERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj6EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI4hKeyS0_Lj6EEeqERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj7EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI4hKeyS0_Lj7EEeqERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj8EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI4hKeyS0_Lj8EEeqERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj9EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI4hKeyS0_Lj9EEeqERKS1_ = comdat any

$_ZN2KVI4hKeyS0_Lj10EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI4hKeyS0_Lj10EEeqERKS1_ = comdat any

$_ZNK2LLI4hKeyS0_E6removeEPKS0_Pm = comdat any

@.str = private unnamed_addr constant [25 x i8] c"library run-time error: \00", align 1
@.str.1 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.2 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.3 = private unnamed_addr constant [5 x i8] c"%lu\0A\00", align 1
@.str.4 = private unnamed_addr constant [68 x i8] c"Expected value: null (in expect_args0). Prim cannot take arguments.\00", align 1
@.str.5 = private unnamed_addr constant [79 x i8] c"Expected cons value (in expect_args1). Prim applied on an empty argument list.\00", align 1
@.str.6 = private unnamed_addr constant [70 x i8] c"Expected null value (in expect_args1). Prim can only take 1 argument.\00", align 1
@.str.7 = private unnamed_addr constant [37 x i8] c"Expected a cons value. (expect_cons)\00", align 1
@.str.8 = private unnamed_addr constant [51 x i8] c"Expected a vector or special value. (expect_other)\00", align 1
@.str.9 = private unnamed_addr constant [3 x i8] c"()\00", align 1
@.str.10 = private unnamed_addr constant [3 x i8] c"#t\00", align 1
@.str.11 = private unnamed_addr constant [3 x i8] c"#f\00", align 1
@.str.12 = private unnamed_addr constant [8 x i8] c"#<void>\00", align 1
@.str.13 = private unnamed_addr constant [13 x i8] c"#<procedure>\00", align 1
@.str.14 = private unnamed_addr constant [2 x i8] c"(\00", align 1
@.str.15 = private unnamed_addr constant [4 x i8] c" . \00", align 1
@.str.16 = private unnamed_addr constant [2 x i8] c")\00", align 1
@.str.17 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.18 = private unnamed_addr constant [5 x i8] c"\22%s\22\00", align 1
@.str.19 = private unnamed_addr constant [3 x i8] c"#(\00", align 1
@.str.20 = private unnamed_addr constant [2 x i8] c",\00", align 1
@.str.21 = private unnamed_addr constant [36 x i8] c"(print.. v); unrecognized value %lu\00", align 1
@.str.22 = private unnamed_addr constant [4 x i8] c"'()\00", align 1
@.str.23 = private unnamed_addr constant [3 x i8] c"'(\00", align 1
@.str.24 = private unnamed_addr constant [4 x i8] c"'%s\00", align 1
@.str.25 = private unnamed_addr constant [34 x i8] c"(print v); unrecognized value %lu\00", align 1
@.str.26 = private unnamed_addr constant [49 x i8] c"Tried to make hash with non list keys parameter.\00", align 1
@.str.27 = private unnamed_addr constant [51 x i8] c"Tried to make hash with non list values parameter.\00", align 1
@.str.28 = private unnamed_addr constant [39 x i8] c"prim applied on more than 2 arguments.\00", align 1
@.str.29 = private unnamed_addr constant [42 x i8] c"first argument to hash-ref must be a hash\00", align 1
@.str.30 = private unnamed_addr constant [44 x i8] c"hash-ref must be called with a key argument\00", align 1
@.str.31 = private unnamed_addr constant [42 x i8] c"hash-ref not given a properly formed hash\00", align 1
@.str.32 = private unnamed_addr constant [47 x i8] c"key value pair does not exist for provided key\00", align 1
@.str.33 = private unnamed_addr constant [42 x i8] c"first argument to hash-set must be a hash\00", align 1
@.str.34 = private unnamed_addr constant [53 x i8] c"hash-set must be called with key and value arguments\00", align 1
@.str.35 = private unnamed_addr constant [42 x i8] c"hash-set not given a properly formed hash\00", align 1
@.str.36 = private unnamed_addr constant [45 x i8] c"first argument to hash-remove must be a hash\00", align 1
@.str.37 = private unnamed_addr constant [47 x i8] c"hash-remove must be called with a key argument\00", align 1
@.str.38 = private unnamed_addr constant [49 x i8] c"first argument to make-vector must be an integer\00", align 1
@.str.39 = private unnamed_addr constant [49 x i8] c"second argument to vector-ref must be an integer\00", align 1
@.str.40 = private unnamed_addr constant [46 x i8] c"first argument to vector-ref must be a vector\00", align 1
@.str.41 = private unnamed_addr constant [46 x i8] c"vector-ref not given a properly formed vector\00", align 1
@.str.42 = private unnamed_addr constant [49 x i8] c"second argument to vector-set must be an integer\00", align 1
@.str.43 = private unnamed_addr constant [48 x i8] c"first argument to vector-set must be an integer\00", align 1
@.str.44 = private unnamed_addr constant [46 x i8] c"vector-set not given a properly formed vector\00", align 1
@.str.45 = private unnamed_addr constant [34 x i8] c"(prim + a b); a is not an integer\00", align 1
@.str.46 = private unnamed_addr constant [34 x i8] c"(prim + a b); b is not an integer\00", align 1
@.str.47 = private unnamed_addr constant [36 x i8] c"Tried to apply + on non list value.\00", align 1
@.str.48 = private unnamed_addr constant [34 x i8] c"(prim - a b); b is not an integer\00", align 1
@.str.49 = private unnamed_addr constant [34 x i8] c"(prim * a b); a is not an integer\00", align 1
@.str.50 = private unnamed_addr constant [34 x i8] c"(prim * a b); b is not an integer\00", align 1
@.str.51 = private unnamed_addr constant [34 x i8] c"(prim / a b); a is not an integer\00", align 1
@.str.52 = private unnamed_addr constant [34 x i8] c"(prim / a b); b is not an integer\00", align 1
@.str.53 = private unnamed_addr constant [34 x i8] c"(prim = a b); a is not an integer\00", align 1
@.str.54 = private unnamed_addr constant [34 x i8] c"(prim = a b); b is not an integer\00", align 1
@.str.55 = private unnamed_addr constant [34 x i8] c"(prim < a b); a is not an integer\00", align 1
@.str.56 = private unnamed_addr constant [34 x i8] c"(prim < a b); b is not an integer\00", align 1
@.str.57 = private unnamed_addr constant [35 x i8] c"(prim <= a b); a is not an integer\00", align 1
@.str.58 = private unnamed_addr constant [35 x i8] c"(prim <= a b); b is not an integer\00", align 1

; Function Attrs: nounwind uwtable
define i64* @alloc(i64 %m) #0 {
  %1 = alloca i64, align 8
  store i64 %m, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call noalias i8* @malloc(i64 %2) #8
  %4 = bitcast i8* %3 to i64*
  ret i64* %4
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #1

; Function Attrs: uwtable
define void @fatal_err(i8* %msg) #2 {
  %1 = alloca i8*, align 8
  store i8* %msg, i8** %1, align 8
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str, i32 0, i32 0))
  %3 = load i8*, i8** %1, align 8
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0), i8* %3)
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  call void @exit(i32 1) #9
  unreachable
                                                  ; No predecessors!
  ret void
}

declare i32 @printf(i8*, ...) #3

; Function Attrs: noreturn nounwind
declare void @exit(i32) #4

; Function Attrs: uwtable
define void @print_u64(i64 %i) #2 {
  %1 = alloca i64, align 8
  store i64 %i, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.3, i32 0, i32 0), i64 %2)
  call void @exit(i32 1) #9
  unreachable
                                                  ; No predecessors!
  ret void
}

; Function Attrs: uwtable
define i64 @expect_args0(i64 %args) #2 {
  %1 = alloca i64, align 8
  store i64 %args, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = icmp ne i64 %2, 0
  br i1 %3, label %4, label %5

; <label>:4                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([68 x i8], [68 x i8]* @.str.4, i32 0, i32 0))
  br label %5

; <label>:5                                       ; preds = %4, %0
  ret i64 0
}

; Function Attrs: uwtable
define i64 @expect_args1(i64 %args) #2 {
  %1 = alloca i64, align 8
  %p = alloca i64*, align 8
  store i64 %args, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = and i64 %2, 7
  %4 = icmp ne i64 %3, 1
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([79 x i8], [79 x i8]* @.str.5, i32 0, i32 0))
  br label %6

; <label>:6                                       ; preds = %5, %0
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, -8
  %9 = inttoptr i64 %8 to i64*
  store i64* %9, i64** %p, align 8
  %10 = load i64*, i64** %p, align 8
  %11 = getelementptr inbounds i64, i64* %10, i64 1
  %12 = load i64, i64* %11, align 8
  %13 = icmp ne i64 %12, 0
  br i1 %13, label %14, label %15

; <label>:14                                      ; preds = %6
  call void @fatal_err(i8* getelementptr inbounds ([70 x i8], [70 x i8]* @.str.6, i32 0, i32 0))
  br label %15

; <label>:15                                      ; preds = %14, %6
  %16 = load i64*, i64** %p, align 8
  %17 = getelementptr inbounds i64, i64* %16, i64 0
  %18 = load i64, i64* %17, align 8
  ret i64 %18
}

; Function Attrs: uwtable
define i64 @expect_cons(i64 %p, i64* %rest) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64*, align 8
  %pp = alloca i64*, align 8
  store i64 %p, i64* %1, align 8
  store i64* %rest, i64** %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 1
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.7, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %1, align 8
  %9 = and i64 %8, -8
  %10 = inttoptr i64 %9 to i64*
  store i64* %10, i64** %pp, align 8
  %11 = load i64*, i64** %pp, align 8
  %12 = getelementptr inbounds i64, i64* %11, i64 1
  %13 = load i64, i64* %12, align 8
  %14 = load i64*, i64** %2, align 8
  store i64 %13, i64* %14, align 8
  %15 = load i64*, i64** %pp, align 8
  %16 = getelementptr inbounds i64, i64* %15, i64 0
  %17 = load i64, i64* %16, align 8
  ret i64 %17
}

; Function Attrs: uwtable
define i64 @expect_other(i64 %v, i64* %rest) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64*, align 8
  %p = alloca i64*, align 8
  store i64 %v, i64* %1, align 8
  store i64* %rest, i64** %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 6
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([51 x i8], [51 x i8]* @.str.8, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %1, align 8
  %9 = and i64 %8, -8
  %10 = inttoptr i64 %9 to i64*
  store i64* %10, i64** %p, align 8
  %11 = load i64*, i64** %p, align 8
  %12 = getelementptr inbounds i64, i64* %11, i64 1
  %13 = load i64, i64* %12, align 8
  %14 = load i64*, i64** %2, align 8
  store i64 %13, i64* %14, align 8
  %15 = load i64*, i64** %p, align 8
  %16 = getelementptr inbounds i64, i64* %15, i64 0
  %17 = load i64, i64* %16, align 8
  ret i64 %17
}

; Function Attrs: nounwind uwtable
define i64 @const_init_int(i64 %i) #0 {
  %1 = alloca i64, align 8
  store i64 %i, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = trunc i64 %2 to i32
  %4 = zext i32 %3 to i64
  %5 = shl i64 %4, 32
  %6 = or i64 %5, 2
  ret i64 %6
}

; Function Attrs: nounwind uwtable
define i64 @const_init_void() #0 {
  ret i64 39
}

; Function Attrs: nounwind uwtable
define i64 @const_init_null() #0 {
  ret i64 0
}

; Function Attrs: nounwind uwtable
define i64 @const_init_true() #0 {
  ret i64 31
}

; Function Attrs: nounwind uwtable
define i64 @const_init_false() #0 {
  ret i64 15
}

; Function Attrs: nounwind uwtable
define i64 @const_init_string(i8* %s) #0 {
  %1 = alloca i8*, align 8
  store i8* %s, i8** %1, align 8
  %2 = load i8*, i8** %1, align 8
  %3 = ptrtoint i8* %2 to i64
  %4 = or i64 %3, 3
  ret i64 %4
}

; Function Attrs: nounwind uwtable
define i64 @const_init_symbol(i8* %s) #0 {
  %1 = alloca i8*, align 8
  store i8* %s, i8** %1, align 8
  %2 = load i8*, i8** %1, align 8
  %3 = ptrtoint i8* %2 to i64
  %4 = or i64 %3, 4
  ret i64 %4
}

; Function Attrs: uwtable
define i64 @prim_print_aux(i64 %v) #2 {
  %1 = alloca i64, align 8
  %p = alloca i64*, align 8
  %vec = alloca i64*, align 8
  %len = alloca i64, align 8
  %i = alloca i64, align 8
  store i64 %v, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = icmp eq i64 %2, 0
  br i1 %3, label %4, label %6

; <label>:4                                       ; preds = %0
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.9, i32 0, i32 0))
  br label %125

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %1, align 8
  %8 = icmp eq i64 %7, 31
  br i1 %8, label %9, label %11

; <label>:9                                       ; preds = %6
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.10, i32 0, i32 0))
  br label %124

; <label>:11                                      ; preds = %6
  %12 = load i64, i64* %1, align 8
  %13 = icmp eq i64 %12, 15
  br i1 %13, label %14, label %16

; <label>:14                                      ; preds = %11
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.11, i32 0, i32 0))
  br label %123

; <label>:16                                      ; preds = %11
  %17 = load i64, i64* %1, align 8
  %18 = icmp eq i64 %17, 39
  br i1 %18, label %19, label %21

; <label>:19                                      ; preds = %16
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @.str.12, i32 0, i32 0))
  br label %122

; <label>:21                                      ; preds = %16
  %22 = load i64, i64* %1, align 8
  %23 = and i64 %22, 7
  %24 = icmp eq i64 %23, 0
  br i1 %24, label %25, label %27

; <label>:25                                      ; preds = %21
  %26 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.13, i32 0, i32 0))
  br label %121

; <label>:27                                      ; preds = %21
  %28 = load i64, i64* %1, align 8
  %29 = and i64 %28, 7
  %30 = icmp eq i64 %29, 0
  br i1 %30, label %31, label %33

; <label>:31                                      ; preds = %27
  %32 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.13, i32 0, i32 0))
  br label %120

; <label>:33                                      ; preds = %27
  %34 = load i64, i64* %1, align 8
  %35 = and i64 %34, 7
  %36 = icmp eq i64 %35, 1
  br i1 %36, label %37, label %52

; <label>:37                                      ; preds = %33
  %38 = load i64, i64* %1, align 8
  %39 = and i64 %38, -8
  %40 = inttoptr i64 %39 to i64*
  store i64* %40, i64** %p, align 8
  %41 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.14, i32 0, i32 0))
  %42 = load i64*, i64** %p, align 8
  %43 = getelementptr inbounds i64, i64* %42, i64 0
  %44 = load i64, i64* %43, align 8
  %45 = call i64 @prim_print_aux(i64 %44)
  %46 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.15, i32 0, i32 0))
  %47 = load i64*, i64** %p, align 8
  %48 = getelementptr inbounds i64, i64* %47, i64 1
  %49 = load i64, i64* %48, align 8
  %50 = call i64 @prim_print_aux(i64 %49)
  %51 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.16, i32 0, i32 0))
  br label %119

; <label>:52                                      ; preds = %33
  %53 = load i64, i64* %1, align 8
  %54 = and i64 %53, 7
  %55 = icmp eq i64 %54, 2
  br i1 %55, label %56, label %61

; <label>:56                                      ; preds = %52
  %57 = load i64, i64* %1, align 8
  %58 = lshr i64 %57, 32
  %59 = trunc i64 %58 to i32
  %60 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.17, i32 0, i32 0), i32 %59)
  br label %118

; <label>:61                                      ; preds = %52
  %62 = load i64, i64* %1, align 8
  %63 = and i64 %62, 7
  %64 = icmp eq i64 %63, 3
  br i1 %64, label %65, label %70

; <label>:65                                      ; preds = %61
  %66 = load i64, i64* %1, align 8
  %67 = and i64 %66, -8
  %68 = inttoptr i64 %67 to i8*
  %69 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.18, i32 0, i32 0), i8* %68)
  br label %117

; <label>:70                                      ; preds = %61
  %71 = load i64, i64* %1, align 8
  %72 = and i64 %71, 7
  %73 = icmp eq i64 %72, 4
  br i1 %73, label %74, label %79

; <label>:74                                      ; preds = %70
  %75 = load i64, i64* %1, align 8
  %76 = and i64 %75, -8
  %77 = inttoptr i64 %76 to i8*
  %78 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0), i8* %77)
  br label %116

; <label>:79                                      ; preds = %70
  %80 = load i64, i64* %1, align 8
  %81 = and i64 %80, 7
  %82 = icmp eq i64 %81, 6
  br i1 %82, label %83, label %112

; <label>:83                                      ; preds = %79
  %84 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.19, i32 0, i32 0))
  %85 = load i64, i64* %1, align 8
  %86 = and i64 %85, -8
  %87 = inttoptr i64 %86 to i64*
  store i64* %87, i64** %vec, align 8
  %88 = load i64*, i64** %vec, align 8
  %89 = getelementptr inbounds i64, i64* %88, i64 0
  %90 = load i64, i64* %89, align 8
  %91 = lshr i64 %90, 3
  store i64 %91, i64* %len, align 8
  %92 = load i64*, i64** %vec, align 8
  %93 = getelementptr inbounds i64, i64* %92, i64 1
  %94 = load i64, i64* %93, align 8
  %95 = call i64 @prim_print_aux(i64 %94)
  store i64 2, i64* %i, align 8
  br label %96

; <label>:96                                      ; preds = %107, %83
  %97 = load i64, i64* %i, align 8
  %98 = load i64, i64* %len, align 8
  %99 = icmp ule i64 %97, %98
  br i1 %99, label %100, label %110

; <label>:100                                     ; preds = %96
  %101 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.20, i32 0, i32 0))
  %102 = load i64, i64* %i, align 8
  %103 = load i64*, i64** %vec, align 8
  %104 = getelementptr inbounds i64, i64* %103, i64 %102
  %105 = load i64, i64* %104, align 8
  %106 = call i64 @prim_print_aux(i64 %105)
  br label %107

; <label>:107                                     ; preds = %100
  %108 = load i64, i64* %i, align 8
  %109 = add i64 %108, 1
  store i64 %109, i64* %i, align 8
  br label %96

; <label>:110                                     ; preds = %96
  %111 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.16, i32 0, i32 0))
  br label %115

; <label>:112                                     ; preds = %79
  %113 = load i64, i64* %1, align 8
  %114 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.21, i32 0, i32 0), i64 %113)
  br label %115

; <label>:115                                     ; preds = %112, %110
  br label %116

; <label>:116                                     ; preds = %115, %74
  br label %117

; <label>:117                                     ; preds = %116, %65
  br label %118

; <label>:118                                     ; preds = %117, %56
  br label %119

; <label>:119                                     ; preds = %118, %37
  br label %120

; <label>:120                                     ; preds = %119, %31
  br label %121

; <label>:121                                     ; preds = %120, %25
  br label %122

; <label>:122                                     ; preds = %121, %19
  br label %123

; <label>:123                                     ; preds = %122, %14
  br label %124

; <label>:124                                     ; preds = %123, %9
  br label %125

; <label>:125                                     ; preds = %124, %4
  ret i64 39
}

; Function Attrs: uwtable
define i64 @prim_print(i64 %v) #2 {
  %1 = alloca i64, align 8
  %p = alloca i64*, align 8
  %vec = alloca i64*, align 8
  %len = alloca i64, align 8
  %i = alloca i64, align 8
  store i64 %v, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = icmp eq i64 %2, 0
  br i1 %3, label %4, label %6

; <label>:4                                       ; preds = %0
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.22, i32 0, i32 0))
  br label %126

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %1, align 8
  %8 = icmp eq i64 %7, 31
  br i1 %8, label %9, label %11

; <label>:9                                       ; preds = %6
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.10, i32 0, i32 0))
  br label %125

; <label>:11                                      ; preds = %6
  %12 = load i64, i64* %1, align 8
  %13 = icmp eq i64 %12, 15
  br i1 %13, label %14, label %16

; <label>:14                                      ; preds = %11
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.11, i32 0, i32 0))
  br label %124

; <label>:16                                      ; preds = %11
  %17 = load i64, i64* %1, align 8
  %18 = icmp eq i64 %17, 39
  br i1 %18, label %19, label %21

; <label>:19                                      ; preds = %16
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @.str.12, i32 0, i32 0))
  br label %123

; <label>:21                                      ; preds = %16
  %22 = load i64, i64* %1, align 8
  %23 = and i64 %22, 7
  %24 = icmp eq i64 %23, 0
  br i1 %24, label %25, label %27

; <label>:25                                      ; preds = %21
  %26 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.13, i32 0, i32 0))
  br label %122

; <label>:27                                      ; preds = %21
  %28 = load i64, i64* %1, align 8
  %29 = and i64 %28, 7
  %30 = icmp eq i64 %29, 1
  br i1 %30, label %31, label %46

; <label>:31                                      ; preds = %27
  %32 = load i64, i64* %1, align 8
  %33 = and i64 %32, -8
  %34 = inttoptr i64 %33 to i64*
  store i64* %34, i64** %p, align 8
  %35 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.23, i32 0, i32 0))
  %36 = load i64*, i64** %p, align 8
  %37 = getelementptr inbounds i64, i64* %36, i64 0
  %38 = load i64, i64* %37, align 8
  %39 = call i64 @prim_print_aux(i64 %38)
  %40 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.15, i32 0, i32 0))
  %41 = load i64*, i64** %p, align 8
  %42 = getelementptr inbounds i64, i64* %41, i64 1
  %43 = load i64, i64* %42, align 8
  %44 = call i64 @prim_print_aux(i64 %43)
  %45 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.16, i32 0, i32 0))
  br label %121

; <label>:46                                      ; preds = %27
  %47 = load i64, i64* %1, align 8
  %48 = and i64 %47, 7
  %49 = icmp eq i64 %48, 2
  br i1 %49, label %50, label %55

; <label>:50                                      ; preds = %46
  %51 = load i64, i64* %1, align 8
  %52 = lshr i64 %51, 32
  %53 = trunc i64 %52 to i32
  %54 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.17, i32 0, i32 0), i32 %53)
  br label %120

; <label>:55                                      ; preds = %46
  %56 = load i64, i64* %1, align 8
  %57 = and i64 %56, 7
  %58 = icmp eq i64 %57, 3
  br i1 %58, label %59, label %64

; <label>:59                                      ; preds = %55
  %60 = load i64, i64* %1, align 8
  %61 = and i64 %60, -8
  %62 = inttoptr i64 %61 to i8*
  %63 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.18, i32 0, i32 0), i8* %62)
  br label %119

; <label>:64                                      ; preds = %55
  %65 = load i64, i64* %1, align 8
  %66 = and i64 %65, 7
  %67 = icmp eq i64 %66, 4
  br i1 %67, label %68, label %73

; <label>:68                                      ; preds = %64
  %69 = load i64, i64* %1, align 8
  %70 = and i64 %69, -8
  %71 = inttoptr i64 %70 to i8*
  %72 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.24, i32 0, i32 0), i8* %71)
  br label %118

; <label>:73                                      ; preds = %64
  %74 = load i64, i64* %1, align 8
  %75 = and i64 %74, 7
  %76 = icmp eq i64 %75, 6
  br i1 %76, label %77, label %114

; <label>:77                                      ; preds = %73
  %78 = load i64, i64* %1, align 8
  %79 = and i64 %78, -8
  %80 = inttoptr i64 %79 to i64*
  %81 = getelementptr inbounds i64, i64* %80, i64 0
  %82 = load i64, i64* %81, align 8
  %83 = and i64 %82, 7
  %84 = icmp eq i64 1, %83
  br i1 %84, label %85, label %114

; <label>:85                                      ; preds = %77
  %86 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.19, i32 0, i32 0))
  %87 = load i64, i64* %1, align 8
  %88 = and i64 %87, -8
  %89 = inttoptr i64 %88 to i64*
  store i64* %89, i64** %vec, align 8
  %90 = load i64*, i64** %vec, align 8
  %91 = getelementptr inbounds i64, i64* %90, i64 0
  %92 = load i64, i64* %91, align 8
  %93 = lshr i64 %92, 3
  store i64 %93, i64* %len, align 8
  %94 = load i64*, i64** %vec, align 8
  %95 = getelementptr inbounds i64, i64* %94, i64 1
  %96 = load i64, i64* %95, align 8
  %97 = call i64 @prim_print(i64 %96)
  store i64 2, i64* %i, align 8
  br label %98

; <label>:98                                      ; preds = %109, %85
  %99 = load i64, i64* %i, align 8
  %100 = load i64, i64* %len, align 8
  %101 = icmp ule i64 %99, %100
  br i1 %101, label %102, label %112

; <label>:102                                     ; preds = %98
  %103 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.20, i32 0, i32 0))
  %104 = load i64, i64* %i, align 8
  %105 = load i64*, i64** %vec, align 8
  %106 = getelementptr inbounds i64, i64* %105, i64 %104
  %107 = load i64, i64* %106, align 8
  %108 = call i64 @prim_print(i64 %107)
  br label %109

; <label>:109                                     ; preds = %102
  %110 = load i64, i64* %i, align 8
  %111 = add i64 %110, 1
  store i64 %111, i64* %i, align 8
  br label %98

; <label>:112                                     ; preds = %98
  %113 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.16, i32 0, i32 0))
  br label %117

; <label>:114                                     ; preds = %77, %73
  %115 = load i64, i64* %1, align 8
  %116 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.25, i32 0, i32 0), i64 %115)
  br label %117

; <label>:117                                     ; preds = %114, %112
  br label %118

; <label>:118                                     ; preds = %117, %68
  br label %119

; <label>:119                                     ; preds = %118, %59
  br label %120

; <label>:120                                     ; preds = %119, %50
  br label %121

; <label>:121                                     ; preds = %120, %31
  br label %122

; <label>:122                                     ; preds = %121, %25
  br label %123

; <label>:123                                     ; preds = %122, %19
  br label %124

; <label>:124                                     ; preds = %123, %14
  br label %125

; <label>:125                                     ; preds = %124, %9
  br label %126

; <label>:126                                     ; preds = %125, %4
  ret i64 39
}

; Function Attrs: uwtable
define i64 @applyprim_print(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_print(i64 %4)
  ret i64 %5
}

; Function Attrs: uwtable
define i64 @prim_halt(i64 %v) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %v, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = call i64 @prim_print(i64 %3)
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  call void @exit(i32 0) #9
  unreachable
                                                  ; No predecessors!
  %7 = load i64, i64* %1, align 8
  ret i64 %7
}

; Function Attrs: uwtable
define %class.hamt* @prim_hash_aux(%class.hamt* %h, i64 %k, i64 %v) #2 {
  %1 = alloca %class.hamt*, align 8
  %2 = alloca %class.hamt*, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %kp = alloca i64*, align 8
  %vp = alloca i64*, align 8
  %key = alloca %class.hKey*, align 8
  %val = alloca %class.hKey*, align 8
  store %class.hamt* %h, %class.hamt** %2, align 8
  store i64 %k, i64* %3, align 8
  store i64 %v, i64* %4, align 8
  %5 = load i64, i64* %3, align 8
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %10, label %7

; <label>:7                                       ; preds = %0
  %8 = load i64, i64* %4, align 8
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %10, label %12

; <label>:10                                      ; preds = %7, %0
  %11 = load %class.hamt*, %class.hamt** %2, align 8
  store %class.hamt* %11, %class.hamt** %1, align 8
  br label %44

; <label>:12                                      ; preds = %7
  %13 = load i64, i64* %3, align 8
  %14 = and i64 %13, -8
  %15 = inttoptr i64 %14 to i64*
  store i64* %15, i64** %kp, align 8
  %16 = load i64, i64* %4, align 8
  %17 = and i64 %16, -8
  %18 = inttoptr i64 %17 to i64*
  store i64* %18, i64** %vp, align 8
  %19 = call i64* @alloc(i64 8)
  %20 = bitcast i64* %19 to %class.hKey*
  %21 = bitcast %class.hKey* %20 to i8*
  %22 = bitcast i8* %21 to %class.hKey*
  %23 = load i64*, i64** %kp, align 8
  %24 = getelementptr inbounds i64, i64* %23, i64 0
  %25 = load i64, i64* %24, align 8
  call void @_ZN4hKeyC2Em(%class.hKey* %22, i64 %25)
  store %class.hKey* %22, %class.hKey** %key, align 8
  %26 = call i64* @alloc(i64 8)
  %27 = bitcast i64* %26 to %class.hKey*
  %28 = bitcast %class.hKey* %27 to i8*
  %29 = bitcast i8* %28 to %class.hKey*
  %30 = load i64*, i64** %vp, align 8
  %31 = getelementptr inbounds i64, i64* %30, i64 0
  %32 = load i64, i64* %31, align 8
  call void @_ZN4hKeyC2Em(%class.hKey* %29, i64 %32)
  store %class.hKey* %29, %class.hKey** %val, align 8
  %33 = load %class.hamt*, %class.hamt** %2, align 8
  %34 = load %class.hKey*, %class.hKey** %key, align 8
  %35 = load %class.hKey*, %class.hKey** %val, align 8
  %36 = call %class.hamt* @_ZNK4hamtI4hKeyS0_E6insertEPKS0_S3_(%class.hamt* %33, %class.hKey* %34, %class.hKey* %35)
  %37 = load i64*, i64** %kp, align 8
  %38 = getelementptr inbounds i64, i64* %37, i64 1
  %39 = load i64, i64* %38, align 8
  %40 = load i64*, i64** %vp, align 8
  %41 = getelementptr inbounds i64, i64* %40, i64 1
  %42 = load i64, i64* %41, align 8
  %43 = call %class.hamt* @prim_hash_aux(%class.hamt* %36, i64 %39, i64 %42)
  store %class.hamt* %43, %class.hamt** %1, align 8
  br label %44

; <label>:44                                      ; preds = %12, %10
  %45 = load %class.hamt*, %class.hamt** %1, align 8
  ret %class.hamt* %45
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN4hKeyC2Em(%class.hKey* %this, i64 %key) unnamed_addr #0 comdat align 2 {
  %1 = alloca %class.hKey*, align 8
  %2 = alloca i64, align 8
  store %class.hKey* %this, %class.hKey** %1, align 8
  store i64 %key, i64* %2, align 8
  %3 = load %class.hKey*, %class.hKey** %1, align 8
  %4 = getelementptr inbounds %class.hKey, %class.hKey* %3, i32 0, i32 0
  %5 = load i64, i64* %2, align 8
  store i64 %5, i64* %4, align 8
  ret void
}

; Function Attrs: uwtable
define linkonce_odr %class.hamt* @_ZNK4hamtI4hKeyS0_E6insertEPKS0_S3_(%class.hamt* %this, %class.hKey* %key, %class.hKey* %val) #2 comdat align 2 {
  %1 = alloca %class.hamt*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %h = alloca i64, align 8
  %hpiece = alloca i64, align 8
  %new_root = alloca %class.hamt*, align 8
  store %class.hamt* %this, %class.hamt** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  store %class.hKey* %val, %class.hKey** %3, align 8
  %4 = load %class.hamt*, %class.hamt** %1, align 8
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  %6 = call i64 @_ZNK4hKey4hashEv(%class.hKey* %5)
  store i64 %6, i64* %h, align 8
  %7 = load i64, i64* %h, align 8
  %8 = and i64 %7, 15
  %9 = urem i64 %8, 7
  store i64 %9, i64* %hpiece, align 8
  %10 = call noalias i8* @malloc(i64 120) #8
  %11 = bitcast i8* %10 to %class.hamt*
  store %class.hamt* %11, %class.hamt** %new_root, align 8
  %12 = load %class.hamt*, %class.hamt** %new_root, align 8
  %13 = bitcast %class.hamt* %12 to i8*
  %14 = bitcast %class.hamt* %4 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %13, i8* %14, i64 120, i32 8, i1 false)
  %15 = load i64, i64* %hpiece, align 8
  %16 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %17 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %16, i64 0, i64 %15
  %18 = getelementptr inbounds %class.KV, %class.KV* %17, i32 0, i32 0
  %19 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %18 to i64*
  %20 = load i64, i64* %19, align 8
  %21 = icmp eq i64 %20, 0
  br i1 %21, label %22, label %35

; <label>:22                                      ; preds = %0
  %23 = load i64, i64* %hpiece, align 8
  %24 = load %class.hamt*, %class.hamt** %new_root, align 8
  %25 = getelementptr inbounds %class.hamt, %class.hamt* %24, i32 0, i32 0
  %26 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %25, i64 0, i64 %23
  %27 = bitcast %class.KV* %26 to i8*
  %28 = bitcast i8* %27 to %class.KV*
  %29 = load %class.hKey*, %class.hKey** %2, align 8
  %30 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EEC2EPKS0_S3_(%class.KV* %28, %class.hKey* %29, %class.hKey* %30)
  %31 = load %class.hamt*, %class.hamt** %new_root, align 8
  %32 = getelementptr inbounds %class.hamt, %class.hamt* %31, i32 0, i32 1
  %33 = load i64, i64* %32, align 8
  %34 = add i64 %33, 1
  store i64 %34, i64* %32, align 8
  br label %115

; <label>:35                                      ; preds = %0
  %36 = load i64, i64* %hpiece, align 8
  %37 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %38 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %37, i64 0, i64 %36
  %39 = getelementptr inbounds %class.KV, %class.KV* %38, i32 0, i32 0
  %40 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %39 to i64*
  %41 = load i64, i64* %40, align 8
  %42 = and i64 %41, 1
  %43 = icmp eq i64 %42, 0
  br i1 %43, label %44, label %98

; <label>:44                                      ; preds = %35
  %45 = load i64, i64* %hpiece, align 8
  %46 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %47 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %46, i64 0, i64 %45
  %48 = getelementptr inbounds %class.KV, %class.KV* %47, i32 0, i32 0
  %49 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %48 to %class.hKey**
  %50 = load %class.hKey*, %class.hKey** %49, align 8
  %51 = load %class.hKey*, %class.hKey** %2, align 8
  %52 = call zeroext i1 @_ZNK4hKeyeqERKS_(%class.hKey* %50, %class.hKey* dereferenceable(8) %51)
  br i1 %52, label %53, label %62

; <label>:53                                      ; preds = %44
  %54 = load i64, i64* %hpiece, align 8
  %55 = load %class.hamt*, %class.hamt** %new_root, align 8
  %56 = getelementptr inbounds %class.hamt, %class.hamt* %55, i32 0, i32 0
  %57 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %56, i64 0, i64 %54
  %58 = bitcast %class.KV* %57 to i8*
  %59 = bitcast i8* %58 to %class.KV*
  %60 = load %class.hKey*, %class.hKey** %2, align 8
  %61 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EEC2EPKS0_S3_(%class.KV* %59, %class.hKey* %60, %class.hKey* %61)
  br label %97

; <label>:62                                      ; preds = %44
  %63 = load %class.hamt*, %class.hamt** %new_root, align 8
  %64 = getelementptr inbounds %class.hamt, %class.hamt* %63, i32 0, i32 1
  %65 = load i64, i64* %64, align 8
  %66 = add i64 %65, 1
  store i64 %66, i64* %64, align 8
  %67 = load i64, i64* %hpiece, align 8
  %68 = load %class.hamt*, %class.hamt** %new_root, align 8
  %69 = getelementptr inbounds %class.hamt, %class.hamt* %68, i32 0, i32 0
  %70 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %69, i64 0, i64 %67
  %71 = bitcast %class.KV* %70 to i8*
  %72 = bitcast i8* %71 to %class.KV*
  %73 = load i64, i64* %hpiece, align 8
  %74 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %75 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %74, i64 0, i64 %73
  %76 = getelementptr inbounds %class.KV, %class.KV* %75, i32 0, i32 0
  %77 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %76 to %class.hKey**
  %78 = load %class.hKey*, %class.hKey** %77, align 8
  %79 = call i64 @_ZNK4hKey4hashEv(%class.hKey* %78)
  %80 = lshr i64 %79, 4
  %81 = load i64, i64* %hpiece, align 8
  %82 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %83 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %82, i64 0, i64 %81
  %84 = getelementptr inbounds %class.KV, %class.KV* %83, i32 0, i32 0
  %85 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %84 to %class.hKey**
  %86 = load %class.hKey*, %class.hKey** %85, align 8
  %87 = load i64, i64* %hpiece, align 8
  %88 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %89 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %88, i64 0, i64 %87
  %90 = getelementptr inbounds %class.KV, %class.KV* %89, i32 0, i32 1
  %91 = bitcast %"union.KV<hKey, hKey, 0>::Val"* %90 to %class.hKey**
  %92 = load %class.hKey*, %class.hKey** %91, align 8
  %93 = load i64, i64* %h, align 8
  %94 = lshr i64 %93, 4
  %95 = load %class.hKey*, %class.hKey** %2, align 8
  %96 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV* sret %72, i64 %80, %class.hKey* %86, %class.hKey* %92, i64 %94, %class.hKey* %95, %class.hKey* %96)
  br label %97

; <label>:97                                      ; preds = %62, %53
  br label %114

; <label>:98                                      ; preds = %35
  %99 = load i64, i64* %hpiece, align 8
  %100 = load %class.hamt*, %class.hamt** %new_root, align 8
  %101 = getelementptr inbounds %class.hamt, %class.hamt* %100, i32 0, i32 0
  %102 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %101, i64 0, i64 %99
  %103 = bitcast %class.KV* %102 to i8*
  %104 = bitcast i8* %103 to %class.KV*
  %105 = load i64, i64* %hpiece, align 8
  %106 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %107 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %106, i64 0, i64 %105
  %108 = load i64, i64* %h, align 8
  %109 = lshr i64 %108, 4
  %110 = load %class.hKey*, %class.hKey** %2, align 8
  %111 = load %class.hKey*, %class.hKey** %3, align 8
  %112 = load %class.hamt*, %class.hamt** %new_root, align 8
  %113 = getelementptr inbounds %class.hamt, %class.hamt* %112, i32 0, i32 1
  call void @_ZN2KVI4hKeyS0_Lj0EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV* sret %104, %class.KV* dereferenceable(16) %107, i64 %109, %class.hKey* %110, %class.hKey* %111, i64* %113)
  br label %114

; <label>:114                                     ; preds = %98, %97
  br label %115

; <label>:115                                     ; preds = %114, %22
  %116 = load %class.hamt*, %class.hamt** %new_root, align 8
  ret %class.hamt* %116
}

; Function Attrs: uwtable
define i64 @prim_hash(i64 %klst, i64 %vlst) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %h = alloca %class.hamt*, align 8
  %hp = alloca i64*, align 8
  %hash = alloca i64*, align 8
  store i64 %klst, i64* %1, align 8
  store i64 %vlst, i64* %2, align 8
  %3 = call i64* @alloc(i64 120)
  %4 = bitcast i64* %3 to %class.hamt*
  %5 = bitcast %class.hamt* %4 to i8*
  %6 = bitcast i8* %5 to %class.hamt*
  call void @_ZN4hamtI4hKeyS0_EC2Ev(%class.hamt* %6)
  store %class.hamt* %6, %class.hamt** %h, align 8
  %7 = load i64, i64* %1, align 8
  %8 = icmp ne i64 %7, 0
  br i1 %8, label %9, label %27

; <label>:9                                       ; preds = %0
  %10 = load i64, i64* %2, align 8
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %12, label %27

; <label>:12                                      ; preds = %9
  %13 = load i64, i64* %1, align 8
  %14 = and i64 %13, 7
  %15 = icmp ne i64 %14, 1
  br i1 %15, label %16, label %17

; <label>:16                                      ; preds = %12
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.26, i32 0, i32 0))
  br label %17

; <label>:17                                      ; preds = %16, %12
  %18 = load i64, i64* %2, align 8
  %19 = and i64 %18, 7
  %20 = icmp ne i64 %19, 1
  br i1 %20, label %21, label %22

; <label>:21                                      ; preds = %17
  call void @fatal_err(i8* getelementptr inbounds ([51 x i8], [51 x i8]* @.str.27, i32 0, i32 0))
  br label %22

; <label>:22                                      ; preds = %21, %17
  %23 = load %class.hamt*, %class.hamt** %h, align 8
  %24 = load i64, i64* %1, align 8
  %25 = load i64, i64* %2, align 8
  %26 = call %class.hamt* @prim_hash_aux(%class.hamt* %23, i64 %24, i64 %25)
  store %class.hamt* %26, %class.hamt** %h, align 8
  br label %27

; <label>:27                                      ; preds = %22, %9, %0
  %28 = load %class.hamt*, %class.hamt** %h, align 8
  %29 = bitcast %class.hamt* %28 to i64*
  store i64* %29, i64** %hp, align 8
  %30 = call i64* @alloc(i64 16)
  store i64* %30, i64** %hash, align 8
  %31 = load i64*, i64** %hash, align 8
  %32 = getelementptr inbounds i64, i64* %31, i64 0
  store i64 10, i64* %32, align 8
  %33 = load i64*, i64** %hp, align 8
  %34 = load i64, i64* %33, align 8
  %35 = load i64*, i64** %hash, align 8
  %36 = getelementptr inbounds i64, i64* %35, i64 1
  store i64 %34, i64* %36, align 8
  %37 = load i64*, i64** %hash, align 8
  %38 = ptrtoint i64* %37 to i64
  %39 = or i64 %38, 6
  ret i64 %39
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN4hamtI4hKeyS0_EC2Ev(%class.hamt* %this) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.hamt*, align 8
  store %class.hamt* %this, %class.hamt** %1, align 8
  %2 = load %class.hamt*, %class.hamt** %1, align 8
  %3 = getelementptr inbounds %class.hamt, %class.hamt* %2, i32 0, i32 0
  %4 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %3, i64 0, i64 0
  %5 = getelementptr inbounds %class.KV, %class.KV* %4, i64 7
  br label %6

; <label>:6                                       ; preds = %6, %0
  %7 = phi %class.KV* [ %4, %0 ], [ %8, %6 ]
  call void @_ZN2KVI4hKeyS0_Lj0EEC2Ev(%class.KV* %7)
  %8 = getelementptr inbounds %class.KV, %class.KV* %7, i64 1
  %9 = icmp eq %class.KV* %8, %5
  br i1 %9, label %10, label %6

; <label>:10                                      ; preds = %6
  %11 = getelementptr inbounds %class.hamt, %class.hamt* %2, i32 0, i32 1
  store i64 0, i64* %11, align 8
  ret void
}

; Function Attrs: uwtable
define i64 @applyprim_hash(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_hash(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: uwtable
define i64 @prim_hash_ref(i64 %h, i64 %k) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %hash = alloca %class.hamt*, align 8
  %key = alloca %class.hKey*, align 8
  %val = alloca %class.hKey*, align 8
  store i64 %h, i64* %1, align 8
  store i64 %k, i64* %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 6
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([42 x i8], [42 x i8]* @.str.29, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([44 x i8], [44 x i8]* @.str.30, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %7
  %12 = load i64, i64* %1, align 8
  %13 = and i64 %12, -8
  %14 = inttoptr i64 %13 to i64*
  %15 = getelementptr inbounds i64, i64* %14, i64 0
  %16 = load i64, i64* %15, align 8
  %17 = and i64 %16, 7
  %18 = icmp ne i64 %17, 2
  br i1 %18, label %19, label %20

; <label>:19                                      ; preds = %11
  call void @fatal_err(i8* getelementptr inbounds ([42 x i8], [42 x i8]* @.str.31, i32 0, i32 0))
  br label %20

; <label>:20                                      ; preds = %19, %11
  %21 = load i64, i64* %1, align 8
  %22 = and i64 %21, -8
  %23 = inttoptr i64 %22 to i64*
  %24 = getelementptr inbounds i64, i64* %23, i64 1
  %25 = load i64, i64* %24, align 8
  %26 = inttoptr i64 %25 to %class.hamt*
  store %class.hamt* %26, %class.hamt** %hash, align 8
  %27 = call i64* @alloc(i64 8)
  %28 = bitcast i64* %27 to %class.hKey*
  %29 = bitcast %class.hKey* %28 to i8*
  %30 = bitcast i8* %29 to %class.hKey*
  %31 = load i64, i64* %2, align 8
  call void @_ZN4hKeyC2Em(%class.hKey* %30, i64 %31)
  store %class.hKey* %30, %class.hKey** %key, align 8
  %32 = load %class.hamt*, %class.hamt** %hash, align 8
  %33 = load %class.hKey*, %class.hKey** %key, align 8
  %34 = call %class.hKey* @_ZNK4hamtI4hKeyS0_E3getEPKS0_(%class.hamt* %32, %class.hKey* %33)
  store %class.hKey* %34, %class.hKey** %val, align 8
  %35 = load %class.hKey*, %class.hKey** %val, align 8
  %36 = icmp eq %class.hKey* %35, null
  br i1 %36, label %37, label %38

; <label>:37                                      ; preds = %20
  call void @fatal_err(i8* getelementptr inbounds ([47 x i8], [47 x i8]* @.str.32, i32 0, i32 0))
  br label %38

; <label>:38                                      ; preds = %37, %20
  %39 = load %class.hKey*, %class.hKey** %val, align 8
  %40 = getelementptr inbounds %class.hKey, %class.hKey* %39, i32 0, i32 0
  %41 = load i64, i64* %40, align 8
  ret i64 %41
}

; Function Attrs: uwtable
define linkonce_odr %class.hKey* @_ZNK4hamtI4hKeyS0_E3getEPKS0_(%class.hamt* %this, %class.hKey* %key) #2 comdat align 2 {
  %1 = alloca %class.hKey*, align 8
  %2 = alloca %class.hamt*, align 8
  %3 = alloca %class.hKey*, align 8
  %h = alloca i64, align 8
  %hpiece = alloca i64, align 8
  store %class.hamt* %this, %class.hamt** %2, align 8
  store %class.hKey* %key, %class.hKey** %3, align 8
  %4 = load %class.hamt*, %class.hamt** %2, align 8
  %5 = load %class.hKey*, %class.hKey** %3, align 8
  %6 = call i64 @_ZNK4hKey4hashEv(%class.hKey* %5)
  store i64 %6, i64* %h, align 8
  %7 = load i64, i64* %h, align 8
  %8 = and i64 %7, 15
  %9 = urem i64 %8, 7
  store i64 %9, i64* %hpiece, align 8
  %10 = load i64, i64* %hpiece, align 8
  %11 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %12 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %11, i64 0, i64 %10
  %13 = getelementptr inbounds %class.KV, %class.KV* %12, i32 0, i32 0
  %14 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %13 to i64*
  %15 = load i64, i64* %14, align 8
  %16 = icmp eq i64 %15, 0
  br i1 %16, label %17, label %18

; <label>:17                                      ; preds = %0
  store %class.hKey* null, %class.hKey** %1, align 8
  br label %52

; <label>:18                                      ; preds = %0
  %19 = load i64, i64* %hpiece, align 8
  %20 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %21 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %20, i64 0, i64 %19
  %22 = getelementptr inbounds %class.KV, %class.KV* %21, i32 0, i32 0
  %23 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %22 to i64*
  %24 = load i64, i64* %23, align 8
  %25 = and i64 %24, 1
  %26 = icmp eq i64 %25, 0
  br i1 %26, label %27, label %44

; <label>:27                                      ; preds = %18
  %28 = load i64, i64* %hpiece, align 8
  %29 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %30 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %29, i64 0, i64 %28
  %31 = getelementptr inbounds %class.KV, %class.KV* %30, i32 0, i32 0
  %32 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %31 to %class.hKey**
  %33 = load %class.hKey*, %class.hKey** %32, align 8
  %34 = load %class.hKey*, %class.hKey** %3, align 8
  %35 = call zeroext i1 @_ZNK4hKeyeqERKS_(%class.hKey* %33, %class.hKey* dereferenceable(8) %34)
  br i1 %35, label %36, label %43

; <label>:36                                      ; preds = %27
  %37 = load i64, i64* %hpiece, align 8
  %38 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %39 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %38, i64 0, i64 %37
  %40 = getelementptr inbounds %class.KV, %class.KV* %39, i32 0, i32 1
  %41 = bitcast %"union.KV<hKey, hKey, 0>::Val"* %40 to %class.hKey**
  %42 = load %class.hKey*, %class.hKey** %41, align 8
  store %class.hKey* %42, %class.hKey** %1, align 8
  br label %52

; <label>:43                                      ; preds = %27
  store %class.hKey* null, %class.hKey** %1, align 8
  br label %52

; <label>:44                                      ; preds = %18
  %45 = load i64, i64* %hpiece, align 8
  %46 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %47 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %46, i64 0, i64 %45
  %48 = load i64, i64* %h, align 8
  %49 = lshr i64 %48, 4
  %50 = load %class.hKey*, %class.hKey** %3, align 8
  %51 = call %class.hKey* @_ZN2KVI4hKeyS0_Lj0EE10inner_findERKS1_mPKS0_(%class.KV* dereferenceable(16) %47, i64 %49, %class.hKey* %50)
  store %class.hKey* %51, %class.hKey** %1, align 8
  br label %52

; <label>:52                                      ; preds = %44, %43, %36, %17
  %53 = load %class.hKey*, %class.hKey** %1, align 8
  ret %class.hKey* %53
}

; Function Attrs: uwtable
define i64 @applyprim_hash_ref(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_hash_ref(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: uwtable
define i64 @prim_hash_set(i64 %h, i64 %k, i64 %v) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %hash = alloca %class.hamt*, align 8
  %key = alloca %class.hKey*, align 8
  %val = alloca %class.hKey*, align 8
  %hp = alloca i64*, align 8
  store i64 %h, i64* %1, align 8
  store i64 %k, i64* %2, align 8
  store i64 %v, i64* %3, align 8
  %4 = load i64, i64* %1, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 6
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([42 x i8], [42 x i8]* @.str.33, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %2, align 8
  %10 = icmp eq i64 %9, 0
  br i1 %10, label %14, label %11

; <label>:11                                      ; preds = %8
  %12 = load i64, i64* %3, align 8
  %13 = icmp eq i64 %12, 0
  br i1 %13, label %14, label %15

; <label>:14                                      ; preds = %11, %8
  call void @fatal_err(i8* getelementptr inbounds ([53 x i8], [53 x i8]* @.str.34, i32 0, i32 0))
  br label %15

; <label>:15                                      ; preds = %14, %11
  %16 = load i64, i64* %1, align 8
  %17 = and i64 %16, -8
  %18 = inttoptr i64 %17 to i64*
  %19 = getelementptr inbounds i64, i64* %18, i64 0
  %20 = load i64, i64* %19, align 8
  %21 = and i64 %20, 7
  %22 = icmp ne i64 %21, 2
  br i1 %22, label %23, label %24

; <label>:23                                      ; preds = %15
  call void @fatal_err(i8* getelementptr inbounds ([42 x i8], [42 x i8]* @.str.35, i32 0, i32 0))
  br label %24

; <label>:24                                      ; preds = %23, %15
  %25 = load i64, i64* %1, align 8
  %26 = and i64 %25, -8
  %27 = inttoptr i64 %26 to i64*
  %28 = getelementptr inbounds i64, i64* %27, i64 1
  %29 = load i64, i64* %28, align 8
  %30 = inttoptr i64 %29 to %class.hamt*
  store %class.hamt* %30, %class.hamt** %hash, align 8
  %31 = call i64* @alloc(i64 8)
  %32 = bitcast i64* %31 to %class.hKey*
  %33 = bitcast %class.hKey* %32 to i8*
  %34 = bitcast i8* %33 to %class.hKey*
  %35 = load i64, i64* %2, align 8
  call void @_ZN4hKeyC2Em(%class.hKey* %34, i64 %35)
  store %class.hKey* %34, %class.hKey** %key, align 8
  %36 = call i64* @alloc(i64 8)
  %37 = bitcast i64* %36 to %class.hKey*
  %38 = bitcast %class.hKey* %37 to i8*
  %39 = bitcast i8* %38 to %class.hKey*
  %40 = load i64, i64* %3, align 8
  call void @_ZN4hKeyC2Em(%class.hKey* %39, i64 %40)
  store %class.hKey* %39, %class.hKey** %val, align 8
  %41 = load %class.hamt*, %class.hamt** %hash, align 8
  %42 = load %class.hKey*, %class.hKey** %key, align 8
  %43 = load %class.hKey*, %class.hKey** %val, align 8
  %44 = call %class.hamt* @_ZNK4hamtI4hKeyS0_E6insertEPKS0_S3_(%class.hamt* %41, %class.hKey* %42, %class.hKey* %43)
  store %class.hamt* %44, %class.hamt** %hash, align 8
  %45 = load %class.hamt*, %class.hamt** %hash, align 8
  %46 = bitcast %class.hamt* %45 to i64*
  store i64* %46, i64** %hp, align 8
  %47 = load i64*, i64** %hp, align 8
  %48 = load i64, i64* %47, align 8
  %49 = load i64, i64* %1, align 8
  %50 = and i64 %49, -8
  %51 = inttoptr i64 %50 to i64*
  %52 = getelementptr inbounds i64, i64* %51, i64 1
  store i64 %48, i64* %52, align 8
  %53 = load i64, i64* %1, align 8
  ret i64 %53
}

; Function Attrs: uwtable
define i64 @applyprim_hash_set(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  %v2 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %rest)
  store i64 %7, i64* %v2, align 8
  %8 = load i64, i64* %rest, align 8
  %9 = icmp ne i64 %8, 0
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %0
  %12 = load i64, i64* %v0, align 8
  %13 = load i64, i64* %v1, align 8
  %14 = load i64, i64* %v2, align 8
  %15 = call i64 @prim_hash_set(i64 %12, i64 %13, i64 %14)
  ret i64 %15
}

; Function Attrs: uwtable
define i64 @prim_hash_remove(i64 %h, i64 %k) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %hash = alloca %class.hamt*, align 8
  %key = alloca %class.hKey*, align 8
  %hp = alloca i64*, align 8
  store i64 %h, i64* %1, align 8
  store i64 %k, i64* %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 6
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([45 x i8], [45 x i8]* @.str.36, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([47 x i8], [47 x i8]* @.str.37, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %7
  %12 = load i64, i64* %1, align 8
  %13 = and i64 %12, -8
  %14 = inttoptr i64 %13 to i64*
  %15 = getelementptr inbounds i64, i64* %14, i64 0
  %16 = load i64, i64* %15, align 8
  %17 = and i64 %16, 7
  %18 = icmp ne i64 %17, 2
  br i1 %18, label %19, label %20

; <label>:19                                      ; preds = %11
  call void @fatal_err(i8* getelementptr inbounds ([42 x i8], [42 x i8]* @.str.35, i32 0, i32 0))
  br label %20

; <label>:20                                      ; preds = %19, %11
  %21 = load i64, i64* %1, align 8
  %22 = and i64 %21, -8
  %23 = inttoptr i64 %22 to i64*
  %24 = getelementptr inbounds i64, i64* %23, i64 1
  %25 = load i64, i64* %24, align 8
  %26 = inttoptr i64 %25 to %class.hamt*
  store %class.hamt* %26, %class.hamt** %hash, align 8
  %27 = call i64* @alloc(i64 8)
  %28 = bitcast i64* %27 to %class.hKey*
  %29 = bitcast %class.hKey* %28 to i8*
  %30 = bitcast i8* %29 to %class.hKey*
  %31 = load i64, i64* %2, align 8
  call void @_ZN4hKeyC2Em(%class.hKey* %30, i64 %31)
  store %class.hKey* %30, %class.hKey** %key, align 8
  %32 = load %class.hamt*, %class.hamt** %hash, align 8
  %33 = load %class.hKey*, %class.hKey** %key, align 8
  %34 = call %class.hamt* @_ZNK4hamtI4hKeyS0_E6removeEPKS0_(%class.hamt* %32, %class.hKey* %33)
  store %class.hamt* %34, %class.hamt** %hash, align 8
  %35 = load %class.hamt*, %class.hamt** %hash, align 8
  %36 = bitcast %class.hamt* %35 to i64*
  store i64* %36, i64** %hp, align 8
  %37 = load i64*, i64** %hp, align 8
  %38 = load i64, i64* %37, align 8
  %39 = load i64, i64* %1, align 8
  %40 = and i64 %39, -8
  %41 = inttoptr i64 %40 to i64*
  %42 = getelementptr inbounds i64, i64* %41, i64 1
  store i64 %38, i64* %42, align 8
  %43 = load i64, i64* %1, align 8
  ret i64 %43
}

; Function Attrs: uwtable
define linkonce_odr %class.hamt* @_ZNK4hamtI4hKeyS0_E6removeEPKS0_(%class.hamt* %this, %class.hKey* %key) #2 comdat align 2 {
  %1 = alloca %class.hamt*, align 8
  %2 = alloca %class.hamt*, align 8
  %3 = alloca %class.hKey*, align 8
  %h = alloca i64, align 8
  %hpiece = alloca i64, align 8
  %new_root = alloca %class.hamt*, align 8
  %temp_count = alloca i64, align 8
  %kv = alloca %class.KV, align 8
  %new_root1 = alloca %class.hamt*, align 8
  store %class.hamt* %this, %class.hamt** %2, align 8
  store %class.hKey* %key, %class.hKey** %3, align 8
  %4 = load %class.hamt*, %class.hamt** %2, align 8
  %5 = load %class.hKey*, %class.hKey** %3, align 8
  %6 = call i64 @_ZNK4hKey4hashEv(%class.hKey* %5)
  store i64 %6, i64* %h, align 8
  %7 = load i64, i64* %h, align 8
  %8 = and i64 %7, 15
  %9 = urem i64 %8, 7
  store i64 %9, i64* %hpiece, align 8
  %10 = load i64, i64* %hpiece, align 8
  %11 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %12 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %11, i64 0, i64 %10
  %13 = getelementptr inbounds %class.KV, %class.KV* %12, i32 0, i32 0
  %14 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %13 to i64*
  %15 = load i64, i64* %14, align 8
  %16 = icmp eq i64 %15, 0
  br i1 %16, label %17, label %18

; <label>:17                                      ; preds = %0
  store %class.hamt* %4, %class.hamt** %1, align 8
  br label %83

; <label>:18                                      ; preds = %0
  %19 = load i64, i64* %hpiece, align 8
  %20 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %21 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %20, i64 0, i64 %19
  %22 = getelementptr inbounds %class.KV, %class.KV* %21, i32 0, i32 0
  %23 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %22 to i64*
  %24 = load i64, i64* %23, align 8
  %25 = and i64 %24, 1
  %26 = icmp eq i64 %25, 0
  br i1 %26, label %27, label %53

; <label>:27                                      ; preds = %18
  %28 = load i64, i64* %hpiece, align 8
  %29 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %30 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %29, i64 0, i64 %28
  %31 = getelementptr inbounds %class.KV, %class.KV* %30, i32 0, i32 0
  %32 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %31 to %class.hKey**
  %33 = load %class.hKey*, %class.hKey** %32, align 8
  %34 = load %class.hKey*, %class.hKey** %3, align 8
  %35 = call zeroext i1 @_ZNK4hKeyeqERKS_(%class.hKey* %33, %class.hKey* dereferenceable(8) %34)
  br i1 %35, label %36, label %52

; <label>:36                                      ; preds = %27
  %37 = call noalias i8* @malloc(i64 120) #8
  %38 = bitcast i8* %37 to %class.hamt*
  store %class.hamt* %38, %class.hamt** %new_root, align 8
  %39 = load %class.hamt*, %class.hamt** %new_root, align 8
  %40 = bitcast %class.hamt* %39 to i8*
  %41 = bitcast %class.hamt* %4 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %40, i8* %41, i64 120, i32 8, i1 false)
  %42 = load i64, i64* %hpiece, align 8
  %43 = load %class.hamt*, %class.hamt** %new_root, align 8
  %44 = getelementptr inbounds %class.hamt, %class.hamt* %43, i64 %42
  %45 = bitcast %class.hamt* %44 to i8*
  %46 = bitcast i8* %45 to %class.KV*
  call void @_ZN2KVI4hKeyS0_Lj0EEC2EPKS0_S3_(%class.KV* %46, %class.hKey* null, %class.hKey* null)
  %47 = load %class.hamt*, %class.hamt** %new_root, align 8
  %48 = getelementptr inbounds %class.hamt, %class.hamt* %47, i32 0, i32 1
  %49 = load i64, i64* %48, align 8
  %50 = add i64 %49, -1
  store i64 %50, i64* %48, align 8
  %51 = load %class.hamt*, %class.hamt** %new_root, align 8
  store %class.hamt* %51, %class.hamt** %1, align 8
  br label %83

; <label>:52                                      ; preds = %27
  store %class.hamt* %4, %class.hamt** %1, align 8
  br label %83

; <label>:53                                      ; preds = %18
  %54 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 1
  %55 = load i64, i64* %54, align 8
  store i64 %55, i64* %temp_count, align 8
  %56 = load i64, i64* %hpiece, align 8
  %57 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %58 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %57, i64 0, i64 %56
  %59 = load i64, i64* %h, align 8
  %60 = lshr i64 %59, 4
  %61 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EE12remove_innerERKS1_mPKS0_Pm(%class.KV* sret %kv, %class.KV* dereferenceable(16) %58, i64 %60, %class.hKey* %61, i64* %temp_count)
  %62 = load i64, i64* %hpiece, align 8
  %63 = getelementptr inbounds %class.hamt, %class.hamt* %4, i32 0, i32 0
  %64 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %63, i64 0, i64 %62
  %65 = call zeroext i1 @_ZNK2KVI4hKeyS0_Lj0EEeqERKS1_(%class.KV* %kv, %class.KV* dereferenceable(16) %64)
  br i1 %65, label %66, label %67

; <label>:66                                      ; preds = %53
  store %class.hamt* %4, %class.hamt** %1, align 8
  br label %83

; <label>:67                                      ; preds = %53
  %68 = call noalias i8* @malloc(i64 120) #8
  %69 = bitcast i8* %68 to %class.hamt*
  store %class.hamt* %69, %class.hamt** %new_root1, align 8
  %70 = load %class.hamt*, %class.hamt** %new_root1, align 8
  %71 = bitcast %class.hamt* %70 to i8*
  %72 = bitcast %class.hamt* %4 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %71, i8* %72, i64 120, i32 8, i1 false)
  %73 = load i64, i64* %hpiece, align 8
  %74 = load %class.hamt*, %class.hamt** %new_root1, align 8
  %75 = getelementptr inbounds %class.hamt, %class.hamt* %74, i32 0, i32 0
  %76 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %75, i64 0, i64 %73
  %77 = bitcast %class.KV* %76 to i8*
  %78 = bitcast i8* %77 to %class.KV*
  call void @_ZN2KVI4hKeyS0_Lj0EEC2ERKS1_(%class.KV* %78, %class.KV* dereferenceable(16) %kv)
  %79 = load i64, i64* %temp_count, align 8
  %80 = load %class.hamt*, %class.hamt** %new_root1, align 8
  %81 = getelementptr inbounds %class.hamt, %class.hamt* %80, i32 0, i32 1
  store i64 %79, i64* %81, align 8
  %82 = load %class.hamt*, %class.hamt** %new_root1, align 8
  store %class.hamt* %82, %class.hamt** %1, align 8
  br label %83

; <label>:83                                      ; preds = %67, %66, %52, %36, %17
  %84 = load %class.hamt*, %class.hamt** %1, align 8
  ret %class.hamt* %84
}

; Function Attrs: uwtable
define i64 @applyprim_hash_remove(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_hash_remove(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: uwtable
define i64 @applyprim_vector(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %buffer = alloca i64*, align 8
  %l = alloca i64, align 8
  %mem = alloca i64*, align 8
  %i = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = call noalias i8* @malloc(i64 4096) #8
  %3 = bitcast i8* %2 to i64*
  store i64* %3, i64** %buffer, align 8
  store i64 0, i64* %l, align 8
  br label %4

; <label>:4                                       ; preds = %13, %0
  %5 = load i64, i64* %1, align 8
  %6 = and i64 %5, 7
  %7 = icmp eq i64 %6, 1
  br i1 %7, label %8, label %11

; <label>:8                                       ; preds = %4
  %9 = load i64, i64* %l, align 8
  %10 = icmp ult i64 %9, 512
  br label %11

; <label>:11                                      ; preds = %8, %4
  %12 = phi i1 [ false, %4 ], [ %10, %8 ]
  br i1 %12, label %13, label %20

; <label>:13                                      ; preds = %11
  %14 = load i64, i64* %1, align 8
  %15 = call i64 @expect_cons(i64 %14, i64* %1)
  %16 = load i64, i64* %l, align 8
  %17 = add i64 %16, 1
  store i64 %17, i64* %l, align 8
  %18 = load i64*, i64** %buffer, align 8
  %19 = getelementptr inbounds i64, i64* %18, i64 %16
  store i64 %15, i64* %19, align 8
  br label %4

; <label>:20                                      ; preds = %11
  %21 = load i64, i64* %l, align 8
  %22 = add i64 %21, 1
  %23 = mul i64 %22, 8
  %24 = call i64* @alloc(i64 %23)
  store i64* %24, i64** %mem, align 8
  %25 = load i64, i64* %l, align 8
  %26 = shl i64 %25, 3
  %27 = or i64 %26, 1
  %28 = load i64*, i64** %mem, align 8
  %29 = getelementptr inbounds i64, i64* %28, i64 0
  store i64 %27, i64* %29, align 8
  store i64 0, i64* %i, align 8
  br label %30

; <label>:30                                      ; preds = %43, %20
  %31 = load i64, i64* %i, align 8
  %32 = load i64, i64* %l, align 8
  %33 = icmp ult i64 %31, %32
  br i1 %33, label %34, label %46

; <label>:34                                      ; preds = %30
  %35 = load i64, i64* %i, align 8
  %36 = load i64*, i64** %buffer, align 8
  %37 = getelementptr inbounds i64, i64* %36, i64 %35
  %38 = load i64, i64* %37, align 8
  %39 = load i64, i64* %i, align 8
  %40 = add i64 %39, 1
  %41 = load i64*, i64** %mem, align 8
  %42 = getelementptr inbounds i64, i64* %41, i64 %40
  store i64 %38, i64* %42, align 8
  br label %43

; <label>:43                                      ; preds = %34
  %44 = load i64, i64* %i, align 8
  %45 = add i64 %44, 1
  store i64 %45, i64* %i, align 8
  br label %30

; <label>:46                                      ; preds = %30
  %47 = load i64*, i64** %buffer, align 8
  %48 = icmp eq i64* %47, null
  br i1 %48, label %51, label %49

; <label>:49                                      ; preds = %46
  %50 = bitcast i64* %47 to i8*
  call void @_ZdaPv(i8* %50) #10
  br label %51

; <label>:51                                      ; preds = %49, %46
  %52 = load i64*, i64** %mem, align 8
  %53 = ptrtoint i64* %52 to i64
  %54 = or i64 %53, 6
  ret i64 %54
}

; Function Attrs: nobuiltin nounwind
declare void @_ZdaPv(i8*) #5

; Function Attrs: uwtable
define i64 @prim_make_45vector(i64 %lenv, i64 %iv) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %l = alloca i64, align 8
  %vec = alloca i64*, align 8
  %i = alloca i64, align 8
  store i64 %lenv, i64* %1, align 8
  store i64 %iv, i64* %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.38, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %1, align 8
  %9 = and i64 %8, -8
  %10 = lshr i64 %9, 32
  %11 = trunc i64 %10 to i32
  %12 = sext i32 %11 to i64
  store i64 %12, i64* %l, align 8
  %13 = load i64, i64* %l, align 8
  %14 = add i64 %13, 1
  %15 = mul i64 %14, 8
  %16 = call i64* @alloc(i64 %15)
  store i64* %16, i64** %vec, align 8
  %17 = load i64, i64* %l, align 8
  %18 = shl i64 %17, 3
  %19 = or i64 %18, 1
  %20 = load i64*, i64** %vec, align 8
  %21 = getelementptr inbounds i64, i64* %20, i64 0
  store i64 %19, i64* %21, align 8
  store i64 1, i64* %i, align 8
  br label %22

; <label>:22                                      ; preds = %31, %7
  %23 = load i64, i64* %i, align 8
  %24 = load i64, i64* %l, align 8
  %25 = icmp ule i64 %23, %24
  br i1 %25, label %26, label %34

; <label>:26                                      ; preds = %22
  %27 = load i64, i64* %2, align 8
  %28 = load i64, i64* %i, align 8
  %29 = load i64*, i64** %vec, align 8
  %30 = getelementptr inbounds i64, i64* %29, i64 %28
  store i64 %27, i64* %30, align 8
  br label %31

; <label>:31                                      ; preds = %26
  %32 = load i64, i64* %i, align 8
  %33 = add i64 %32, 1
  store i64 %33, i64* %i, align 8
  br label %22

; <label>:34                                      ; preds = %22
  %35 = load i64*, i64** %vec, align 8
  %36 = ptrtoint i64* %35 to i64
  %37 = or i64 %36, 6
  ret i64 %37
}

; Function Attrs: uwtable
define i64 @applyprim_make_45vector(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_make_45vector(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: uwtable
define i64 @prim_vector_45ref(i64 %v, i64 %i) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %v, i64* %1, align 8
  store i64 %i, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.39, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %1, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 6
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.40, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %7
  %13 = load i64, i64* %1, align 8
  %14 = and i64 %13, -8
  %15 = inttoptr i64 %14 to i64*
  %16 = getelementptr inbounds i64, i64* %15, i64 0
  %17 = load i64, i64* %16, align 8
  %18 = and i64 %17, 7
  %19 = icmp ne i64 %18, 1
  br i1 %19, label %20, label %21

; <label>:20                                      ; preds = %12
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.41, i32 0, i32 0))
  br label %21

; <label>:21                                      ; preds = %20, %12
  %22 = load i64, i64* %2, align 8
  %23 = and i64 %22, -8
  %24 = lshr i64 %23, 32
  %25 = trunc i64 %24 to i32
  %26 = add nsw i32 1, %25
  %27 = sext i32 %26 to i64
  %28 = load i64, i64* %1, align 8
  %29 = and i64 %28, -8
  %30 = inttoptr i64 %29 to i64*
  %31 = getelementptr inbounds i64, i64* %30, i64 %27
  %32 = load i64, i64* %31, align 8
  ret i64 %32
}

; Function Attrs: uwtable
define i64 @applyprim_vector_45ref(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_vector_45ref(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: uwtable
define i64 @prim_vector_45set_33(i64 %a, i64 %i, i64 %v) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %1, align 8
  store i64 %i, i64* %2, align 8
  store i64 %v, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.42, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %1, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 6
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([48 x i8], [48 x i8]* @.str.43, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %1, align 8
  %15 = and i64 %14, -8
  %16 = inttoptr i64 %15 to i64*
  %17 = getelementptr inbounds i64, i64* %16, i64 0
  %18 = load i64, i64* %17, align 8
  %19 = and i64 %18, 7
  %20 = icmp ne i64 %19, 1
  br i1 %20, label %21, label %22

; <label>:21                                      ; preds = %13
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.44, i32 0, i32 0))
  br label %22

; <label>:22                                      ; preds = %21, %13
  %23 = load i64, i64* %3, align 8
  %24 = load i64, i64* %2, align 8
  %25 = and i64 %24, -8
  %26 = lshr i64 %25, 32
  %27 = trunc i64 %26 to i32
  %28 = add nsw i32 1, %27
  %29 = sext i32 %28 to i64
  %30 = load i64, i64* %1, align 8
  %31 = and i64 %30, -8
  %32 = inttoptr i64 %31 to i64*
  %33 = getelementptr inbounds i64, i64* %32, i64 %29
  store i64 %23, i64* %33, align 8
  ret i64 39
}

; Function Attrs: uwtable
define i64 @applyprim_vector_45set_33(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  %v2 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %rest)
  store i64 %7, i64* %v2, align 8
  %8 = load i64, i64* %rest, align 8
  %9 = icmp ne i64 %8, 0
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %0
  %12 = load i64, i64* %v0, align 8
  %13 = load i64, i64* %v1, align 8
  %14 = load i64, i64* %v2, align 8
  %15 = call i64 @prim_vector_45set_33(i64 %12, i64 %13, i64 %14)
  ret i64 %15
}

; Function Attrs: nounwind uwtable
define i64 @prim_void() #0 {
  ret i64 39
}

; Function Attrs: nounwind uwtable
define i64 @prim_eq_63(i64 %a, i64 %b) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  store i64 %b, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = load i64, i64* %3, align 8
  %6 = icmp eq i64 %4, %5
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %9

; <label>:8                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %9

; <label>:9                                       ; preds = %8, %7
  %10 = load i64, i64* %1, align 8
  ret i64 %10
}

; Function Attrs: uwtable
define i64 @applyprim_eq_63(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_eq_63(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: nounwind uwtable
define i64 @prim_eqv_63(i64 %a, i64 %b) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  store i64 %b, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = load i64, i64* %3, align 8
  %6 = icmp eq i64 %4, %5
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %9

; <label>:8                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %9

; <label>:9                                       ; preds = %8, %7
  %10 = load i64, i64* %1, align 8
  ret i64 %10
}

; Function Attrs: uwtable
define i64 @applyprim_eqv_63(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_eqv_63(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: nounwind uwtable
define i64 @prim_number_63(i64 %a) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = and i64 %3, 7
  %5 = icmp eq i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %8

; <label>:7                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %8

; <label>:8                                       ; preds = %7, %6
  %9 = load i64, i64* %1, align 8
  ret i64 %9
}

; Function Attrs: uwtable
define i64 @applyprim_number_63(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_number_63(i64 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @prim_integer_63(i64 %a) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = and i64 %3, 7
  %5 = icmp eq i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %8

; <label>:7                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %8

; <label>:8                                       ; preds = %7, %6
  %9 = load i64, i64* %1, align 8
  ret i64 %9
}

; Function Attrs: uwtable
define i64 @applyprim_integer_63(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_integer_63(i64 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @prim_void_63(i64 %a) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp eq i64 %3, 39
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %7

; <label>:6                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %7

; <label>:7                                       ; preds = %6, %5
  %8 = load i64, i64* %1, align 8
  ret i64 %8
}

; Function Attrs: uwtable
define i64 @applyprim_void_63(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_void_63(i64 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @prim_procedure_63(i64 %a) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = and i64 %3, 7
  %5 = icmp eq i64 %4, 0
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %8

; <label>:7                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %8

; <label>:8                                       ; preds = %7, %6
  %9 = load i64, i64* %1, align 8
  ret i64 %9
}

; Function Attrs: uwtable
define i64 @applyprim_procedure_63(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_procedure_63(i64 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @prim_null_63(i64 %p) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %p, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp eq i64 %3, 0
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %7

; <label>:6                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %7

; <label>:7                                       ; preds = %6, %5
  %8 = load i64, i64* %1, align 8
  ret i64 %8
}

; Function Attrs: uwtable
define i64 @applyprim_null_63(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_null_63(i64 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @prim_cons_63(i64 %p) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %p, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = and i64 %3, 7
  %5 = icmp eq i64 %4, 1
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %8

; <label>:7                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %8

; <label>:8                                       ; preds = %7, %6
  %9 = load i64, i64* %1, align 8
  ret i64 %9
}

; Function Attrs: uwtable
define i64 @applyprim_cons_63(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_cons_63(i64 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @prim_cons(i64 %a, i64 %b) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %p = alloca i64*, align 8
  store i64 %a, i64* %1, align 8
  store i64 %b, i64* %2, align 8
  %3 = call i64* @alloc(i64 16)
  store i64* %3, i64** %p, align 8
  %4 = load i64, i64* %1, align 8
  %5 = load i64*, i64** %p, align 8
  %6 = getelementptr inbounds i64, i64* %5, i64 0
  store i64 %4, i64* %6, align 8
  %7 = load i64, i64* %2, align 8
  %8 = load i64*, i64** %p, align 8
  %9 = getelementptr inbounds i64, i64* %8, i64 1
  store i64 %7, i64* %9, align 8
  %10 = load i64*, i64** %p, align 8
  %11 = ptrtoint i64* %10 to i64
  %12 = or i64 %11, 1
  ret i64 %12
}

; Function Attrs: uwtable
define i64 @applyprim_cons(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_cons(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: uwtable
define i64 @prim_car(i64 %p) #2 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %p, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  ret i64 %4
}

; Function Attrs: uwtable
define i64 @applyprim_car(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_car(i64 %4)
  ret i64 %5
}

; Function Attrs: uwtable
define i64 @prim_cdr(i64 %p) #2 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %p, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  ret i64 %4
}

; Function Attrs: uwtable
define i64 @applyprim_cdr(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_cdr(i64 %4)
  ret i64 %5
}

; Function Attrs: uwtable
define i64 @prim__43(i64 %a, i64 %b) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %1, align 8
  store i64 %b, i64* %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.45, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.46, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %7
  %13 = load i64, i64* %1, align 8
  %14 = and i64 %13, -8
  %15 = lshr i64 %14, 32
  %16 = trunc i64 %15 to i32
  %17 = load i64, i64* %2, align 8
  %18 = and i64 %17, -8
  %19 = lshr i64 %18, 32
  %20 = trunc i64 %19 to i32
  %21 = add nsw i32 %16, %20
  %22 = zext i32 %21 to i64
  %23 = shl i64 %22, 32
  %24 = or i64 %23, 2
  ret i64 %24
}

; Function Attrs: uwtable
define i64 @applyprim__43(i64 %p) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %pp = alloca i64*, align 8
  store i64 %p, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp eq i64 %3, 0
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i64 2, i64* %1, align 8
  br label %32

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %2, align 8
  %8 = and i64 %7, 7
  %9 = icmp ne i64 %8, 1
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %6
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.47, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %6
  %12 = load i64, i64* %2, align 8
  %13 = and i64 %12, -8
  %14 = inttoptr i64 %13 to i64*
  store i64* %14, i64** %pp, align 8
  %15 = load i64*, i64** %pp, align 8
  %16 = getelementptr inbounds i64, i64* %15, i64 0
  %17 = load i64, i64* %16, align 8
  %18 = and i64 %17, -8
  %19 = lshr i64 %18, 32
  %20 = trunc i64 %19 to i32
  %21 = load i64*, i64** %pp, align 8
  %22 = getelementptr inbounds i64, i64* %21, i64 1
  %23 = load i64, i64* %22, align 8
  %24 = call i64 @applyprim__43(i64 %23)
  %25 = and i64 %24, -8
  %26 = lshr i64 %25, 32
  %27 = trunc i64 %26 to i32
  %28 = add nsw i32 %20, %27
  %29 = zext i32 %28 to i64
  %30 = shl i64 %29, 32
  %31 = or i64 %30, 2
  store i64 %31, i64* %1, align 8
  br label %32

; <label>:32                                      ; preds = %11, %5
  %33 = load i64, i64* %1, align 8
  ret i64 %33
}

; Function Attrs: uwtable
define i64 @prim__45(i64 %a, i64 %b) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %1, align 8
  store i64 %b, i64* %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.45, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.48, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %7
  %13 = load i64, i64* %1, align 8
  %14 = and i64 %13, -8
  %15 = lshr i64 %14, 32
  %16 = trunc i64 %15 to i32
  %17 = load i64, i64* %2, align 8
  %18 = and i64 %17, -8
  %19 = lshr i64 %18, 32
  %20 = trunc i64 %19 to i32
  %21 = sub nsw i32 %16, %20
  %22 = zext i32 %21 to i64
  %23 = shl i64 %22, 32
  %24 = or i64 %23, 2
  ret i64 %24
}

; Function Attrs: uwtable
define i64 @applyprim__45(i64 %p) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %pp = alloca i64*, align 8
  store i64 %p, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp eq i64 %3, 0
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i64 2, i64* %1, align 8
  br label %48

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %2, align 8
  %8 = and i64 %7, 7
  %9 = icmp ne i64 %8, 1
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %6
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.47, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %6
  %12 = load i64, i64* %2, align 8
  %13 = and i64 %12, -8
  %14 = inttoptr i64 %13 to i64*
  store i64* %14, i64** %pp, align 8
  %15 = load i64*, i64** %pp, align 8
  %16 = getelementptr inbounds i64, i64* %15, i64 1
  %17 = load i64, i64* %16, align 8
  %18 = icmp eq i64 %17, 0
  br i1 %18, label %19, label %30

; <label>:19                                      ; preds = %11
  %20 = load i64*, i64** %pp, align 8
  %21 = getelementptr inbounds i64, i64* %20, i64 0
  %22 = load i64, i64* %21, align 8
  %23 = and i64 %22, -8
  %24 = lshr i64 %23, 32
  %25 = trunc i64 %24 to i32
  %26 = sub nsw i32 0, %25
  %27 = zext i32 %26 to i64
  %28 = shl i64 %27, 32
  %29 = or i64 %28, 2
  store i64 %29, i64* %1, align 8
  br label %48

; <label>:30                                      ; preds = %11
  %31 = load i64*, i64** %pp, align 8
  %32 = getelementptr inbounds i64, i64* %31, i64 0
  %33 = load i64, i64* %32, align 8
  %34 = and i64 %33, -8
  %35 = lshr i64 %34, 32
  %36 = trunc i64 %35 to i32
  %37 = load i64*, i64** %pp, align 8
  %38 = getelementptr inbounds i64, i64* %37, i64 1
  %39 = load i64, i64* %38, align 8
  %40 = call i64 @applyprim__43(i64 %39)
  %41 = and i64 %40, -8
  %42 = lshr i64 %41, 32
  %43 = trunc i64 %42 to i32
  %44 = sub nsw i32 %36, %43
  %45 = zext i32 %44 to i64
  %46 = shl i64 %45, 32
  %47 = or i64 %46, 2
  store i64 %47, i64* %1, align 8
  br label %48

; <label>:48                                      ; preds = %30, %19, %5
  %49 = load i64, i64* %1, align 8
  ret i64 %49
}

; Function Attrs: uwtable
define i64 @prim__42(i64 %a, i64 %b) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %1, align 8
  store i64 %b, i64* %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.49, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.50, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %7
  %13 = load i64, i64* %1, align 8
  %14 = and i64 %13, -8
  %15 = lshr i64 %14, 32
  %16 = trunc i64 %15 to i32
  %17 = load i64, i64* %2, align 8
  %18 = and i64 %17, -8
  %19 = lshr i64 %18, 32
  %20 = trunc i64 %19 to i32
  %21 = mul nsw i32 %16, %20
  %22 = zext i32 %21 to i64
  %23 = shl i64 %22, 32
  %24 = or i64 %23, 2
  ret i64 %24
}

; Function Attrs: uwtable
define i64 @applyprim__42(i64 %p) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %pp = alloca i64*, align 8
  store i64 %p, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp eq i64 %3, 0
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i64 4294967298, i64* %1, align 8
  br label %32

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %2, align 8
  %8 = and i64 %7, 7
  %9 = icmp ne i64 %8, 1
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %6
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.47, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %6
  %12 = load i64, i64* %2, align 8
  %13 = and i64 %12, -8
  %14 = inttoptr i64 %13 to i64*
  store i64* %14, i64** %pp, align 8
  %15 = load i64*, i64** %pp, align 8
  %16 = getelementptr inbounds i64, i64* %15, i64 0
  %17 = load i64, i64* %16, align 8
  %18 = and i64 %17, -8
  %19 = lshr i64 %18, 32
  %20 = trunc i64 %19 to i32
  %21 = load i64*, i64** %pp, align 8
  %22 = getelementptr inbounds i64, i64* %21, i64 1
  %23 = load i64, i64* %22, align 8
  %24 = call i64 @applyprim__42(i64 %23)
  %25 = and i64 %24, -8
  %26 = lshr i64 %25, 32
  %27 = trunc i64 %26 to i32
  %28 = mul nsw i32 %20, %27
  %29 = zext i32 %28 to i64
  %30 = shl i64 %29, 32
  %31 = or i64 %30, 2
  store i64 %31, i64* %1, align 8
  br label %32

; <label>:32                                      ; preds = %11, %5
  %33 = load i64, i64* %1, align 8
  ret i64 %33
}

; Function Attrs: uwtable
define i64 @prim__47(i64 %a, i64 %b) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %1, align 8
  store i64 %b, i64* %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.51, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.52, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %7
  %13 = load i64, i64* %1, align 8
  %14 = and i64 %13, -8
  %15 = lshr i64 %14, 32
  %16 = trunc i64 %15 to i32
  %17 = load i64, i64* %2, align 8
  %18 = and i64 %17, -8
  %19 = lshr i64 %18, 32
  %20 = trunc i64 %19 to i32
  %21 = sdiv i32 %16, %20
  %22 = zext i32 %21 to i64
  %23 = shl i64 %22, 32
  %24 = or i64 %23, 2
  ret i64 %24
}

; Function Attrs: uwtable
define i64 @prim__61(i64 %a, i64 %b) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  store i64 %b, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.53, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.54, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %2, align 8
  %15 = and i64 %14, -8
  %16 = lshr i64 %15, 32
  %17 = trunc i64 %16 to i32
  %18 = load i64, i64* %3, align 8
  %19 = and i64 %18, -8
  %20 = lshr i64 %19, 32
  %21 = trunc i64 %20 to i32
  %22 = icmp eq i32 %17, %21
  br i1 %22, label %23, label %24

; <label>:23                                      ; preds = %13
  store i64 31, i64* %1, align 8
  br label %25

; <label>:24                                      ; preds = %13
  store i64 15, i64* %1, align 8
  br label %25

; <label>:25                                      ; preds = %24, %23
  %26 = load i64, i64* %1, align 8
  ret i64 %26
}

; Function Attrs: uwtable
define i64 @prim__60(i64 %a, i64 %b) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  store i64 %b, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.55, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.56, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %2, align 8
  %15 = and i64 %14, -8
  %16 = lshr i64 %15, 32
  %17 = trunc i64 %16 to i32
  %18 = load i64, i64* %3, align 8
  %19 = and i64 %18, -8
  %20 = lshr i64 %19, 32
  %21 = trunc i64 %20 to i32
  %22 = icmp slt i32 %17, %21
  br i1 %22, label %23, label %24

; <label>:23                                      ; preds = %13
  store i64 31, i64* %1, align 8
  br label %25

; <label>:24                                      ; preds = %13
  store i64 15, i64* %1, align 8
  br label %25

; <label>:25                                      ; preds = %24, %23
  %26 = load i64, i64* %1, align 8
  ret i64 %26
}

; Function Attrs: uwtable
define i64 @prim__60_61(i64 %a, i64 %b) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  store i64 %b, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.57, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.58, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %2, align 8
  %15 = and i64 %14, -8
  %16 = lshr i64 %15, 32
  %17 = trunc i64 %16 to i32
  %18 = load i64, i64* %3, align 8
  %19 = and i64 %18, -8
  %20 = lshr i64 %19, 32
  %21 = trunc i64 %20 to i32
  %22 = icmp sle i32 %17, %21
  br i1 %22, label %23, label %24

; <label>:23                                      ; preds = %13
  store i64 31, i64* %1, align 8
  br label %25

; <label>:24                                      ; preds = %13
  store i64 15, i64* %1, align 8
  br label %25

; <label>:25                                      ; preds = %24, %23
  %26 = load i64, i64* %1, align 8
  ret i64 %26
}

; Function Attrs: uwtable
define i64 @prim__62(i64 %a, i64 %b) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  store i64 %b, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.57, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.58, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %2, align 8
  %15 = and i64 %14, -8
  %16 = lshr i64 %15, 32
  %17 = trunc i64 %16 to i32
  %18 = load i64, i64* %3, align 8
  %19 = and i64 %18, -8
  %20 = lshr i64 %19, 32
  %21 = trunc i64 %20 to i32
  %22 = icmp sgt i32 %17, %21
  br i1 %22, label %23, label %24

; <label>:23                                      ; preds = %13
  store i64 31, i64* %1, align 8
  br label %25

; <label>:24                                      ; preds = %13
  store i64 15, i64* %1, align 8
  br label %25

; <label>:25                                      ; preds = %24, %23
  %26 = load i64, i64* %1, align 8
  ret i64 %26
}

; Function Attrs: uwtable
define i64 @prim__62_61(i64 %a, i64 %b) #2 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  store i64 %b, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.57, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.58, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %2, align 8
  %15 = and i64 %14, -8
  %16 = lshr i64 %15, 32
  %17 = trunc i64 %16 to i32
  %18 = load i64, i64* %3, align 8
  %19 = and i64 %18, -8
  %20 = lshr i64 %19, 32
  %21 = trunc i64 %20 to i32
  %22 = icmp sge i32 %17, %21
  br i1 %22, label %23, label %24

; <label>:23                                      ; preds = %13
  store i64 31, i64* %1, align 8
  br label %25

; <label>:24                                      ; preds = %13
  store i64 15, i64* %1, align 8
  br label %25

; <label>:25                                      ; preds = %24, %23
  %26 = load i64, i64* %1, align 8
  ret i64 %26
}

; Function Attrs: nounwind uwtable
define i64 @prim_not(i64 %a) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp eq i64 %3, 15
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %7

; <label>:6                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %7

; <label>:7                                       ; preds = %6, %5
  %8 = load i64, i64* %1, align 8
  ret i64 %8
}

; Function Attrs: uwtable
define i64 @applyprim_not(i64 %lst) #2 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_not(i64 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define linkonce_odr i64 @_ZNK4hKey4hashEv(%class.hKey* %this) #0 comdat align 2 {
  %1 = alloca %class.hKey*, align 8
  %data = alloca i8*, align 8
  %h = alloca i64, align 8
  %i = alloca i32, align 4
  store %class.hKey* %this, %class.hKey** %1, align 8
  %2 = load %class.hKey*, %class.hKey** %1, align 8
  %3 = bitcast %class.hKey* %2 to i8*
  store i8* %3, i8** %data, align 8
  store i64 -3750763034362895579, i64* %h, align 8
  store i32 0, i32* %i, align 4
  br label %4

; <label>:4                                       ; preds = %24, %0
  %5 = load i32, i32* %i, align 4
  %6 = zext i32 %5 to i64
  %7 = icmp ult i64 %6, 8
  br i1 %7, label %8, label %26

; <label>:8                                       ; preds = %4
  %9 = load i64, i64* %h, align 8
  %10 = load i8*, i8** %data, align 8
  %11 = load i8, i8* %10, align 1
  %12 = zext i8 %11 to i64
  %13 = xor i64 %9, %12
  store i64 %13, i64* %h, align 8
  %14 = load i64, i64* %h, align 8
  %15 = mul i64 %14, 1099511628211
  store i64 %15, i64* %h, align 8
  br label %16

; <label>:16                                      ; preds = %8
  %17 = load i32, i32* %i, align 4
  %18 = add i32 %17, 1
  store i32 %18, i32* %i, align 4
  %19 = icmp ne i32 %18, 0
  br i1 %19, label %20, label %24

; <label>:20                                      ; preds = %16
  %21 = load i8*, i8** %data, align 8
  %22 = getelementptr inbounds i8, i8* %21, i32 1
  store i8* %22, i8** %data, align 8
  %23 = icmp ne i8* %22, null
  br label %24

; <label>:24                                      ; preds = %20, %16
  %25 = phi i1 [ false, %16 ], [ %23, %20 ]
  br label %4

; <label>:26                                      ; preds = %4
  %27 = load i64, i64* %h, align 8
  ret i64 %27
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture readonly, i64, i32, i1) #6

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj0EEC2EPKS0_S3_(%class.KV* %this, %class.hKey* %key, %class.hKey* %val) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  store %class.KV* %this, %class.KV** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  store %class.hKey* %val, %class.hKey** %3, align 8
  %4 = load %class.KV*, %class.KV** %1, align 8
  %5 = getelementptr inbounds %class.KV, %class.KV* %4, i32 0, i32 0
  %6 = load %class.hKey*, %class.hKey** %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 0>::Key"* %5, %class.hKey* %6)
  %7 = getelementptr inbounds %class.KV, %class.KV* %4, i32 0, i32 1
  %8 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 0>::Val"* %7, %class.hKey* %8)
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr zeroext i1 @_ZNK4hKeyeqERKS_(%class.hKey* %this, %class.hKey* dereferenceable(8) %k) #0 comdat align 2 {
  %1 = alloca %class.hKey*, align 8
  %2 = alloca %class.hKey*, align 8
  store %class.hKey* %this, %class.hKey** %1, align 8
  store %class.hKey* %k, %class.hKey** %2, align 8
  %3 = load %class.hKey*, %class.hKey** %1, align 8
  %4 = load %class.hKey*, %class.hKey** %2, align 8
  %5 = getelementptr inbounds %class.hKey, %class.hKey* %4, i32 0, i32 0
  %6 = load i64, i64* %5, align 8
  %7 = getelementptr inbounds %class.hKey, %class.hKey* %3, i32 0, i32 0
  %8 = load i64, i64* %7, align 8
  %9 = icmp eq i64 %6, %8
  ret i1 %9
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj0EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV* noalias sret %agg.result, i64 %h0, %class.hKey* %k0, %class.hKey* %v0, i64 %h1, %class.hKey* %k1, %class.hKey* %v1) #2 comdat align 2 {
  %1 = alloca i64, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hKey*, align 8
  %6 = alloca %class.hKey*, align 8
  %h0piece = alloca i32, align 4
  %h1piece = alloca i32, align 4
  %childkv = alloca %class.KV.0, align 8
  %node = alloca %class.KV.0*, align 8
  %node1 = alloca %class.KV.0*, align 8
  store i64 %h0, i64* %1, align 8
  store %class.hKey* %k0, %class.hKey** %2, align 8
  store %class.hKey* %v0, %class.hKey** %3, align 8
  store i64 %h1, i64* %4, align 8
  store %class.hKey* %k1, %class.hKey** %5, align 8
  store %class.hKey* %v1, %class.hKey** %6, align 8
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 63
  %9 = urem i64 %8, 63
  %10 = trunc i64 %9 to i32
  store i32 %10, i32* %h0piece, align 4
  %11 = load i64, i64* %4, align 8
  %12 = and i64 %11, 63
  %13 = urem i64 %12, 63
  %14 = trunc i64 %13 to i32
  store i32 %14, i32* %h1piece, align 4
  %15 = load i32, i32* %h0piece, align 4
  %16 = load i32, i32* %h1piece, align 4
  %17 = icmp eq i32 %15, %16
  br i1 %17, label %18, label %39

; <label>:18                                      ; preds = %0
  %19 = load i64, i64* %1, align 8
  %20 = lshr i64 %19, 6
  %21 = load %class.hKey*, %class.hKey** %2, align 8
  %22 = load %class.hKey*, %class.hKey** %3, align 8
  %23 = load i64, i64* %4, align 8
  %24 = lshr i64 %23, 6
  %25 = load %class.hKey*, %class.hKey** %5, align 8
  %26 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.0* sret %childkv, i64 %20, %class.hKey* %21, %class.hKey* %22, i64 %24, %class.hKey* %25, %class.hKey* %26)
  %27 = call noalias i8* @malloc(i64 16) #8
  %28 = bitcast i8* %27 to %class.KV.0*
  store %class.KV.0* %28, %class.KV.0** %node, align 8
  %29 = load %class.KV.0*, %class.KV.0** %node, align 8
  %30 = getelementptr inbounds %class.KV.0, %class.KV.0* %29, i64 0
  %31 = bitcast %class.KV.0* %30 to i8*
  %32 = bitcast i8* %31 to %class.KV.0*
  call void @_ZN2KVI4hKeyS0_Lj1EEC2ERKS1_(%class.KV.0* %32, %class.KV.0* dereferenceable(16) %childkv)
  %33 = load i32, i32* %h0piece, align 4
  %34 = zext i32 %33 to i64
  %35 = shl i64 1, %34
  %36 = shl i64 %35, 1
  %37 = or i64 %36, 1
  %38 = load %class.KV.0*, %class.KV.0** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %agg.result, i64 %37, %class.KV.0* %38)
  br label %82

; <label>:39                                      ; preds = %0
  %40 = call noalias i8* @malloc(i64 32) #8
  %41 = bitcast i8* %40 to %class.KV.0*
  store %class.KV.0* %41, %class.KV.0** %node1, align 8
  %42 = load i32, i32* %h1piece, align 4
  %43 = load i32, i32* %h0piece, align 4
  %44 = icmp ult i32 %42, %43
  br i1 %44, label %45, label %58

; <label>:45                                      ; preds = %39
  %46 = load %class.KV.0*, %class.KV.0** %node1, align 8
  %47 = getelementptr inbounds %class.KV.0, %class.KV.0* %46, i64 0
  %48 = bitcast %class.KV.0* %47 to i8*
  %49 = bitcast i8* %48 to %class.KV.0*
  %50 = load %class.hKey*, %class.hKey** %5, align 8
  %51 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %49, %class.hKey* %50, %class.hKey* %51)
  %52 = load %class.KV.0*, %class.KV.0** %node1, align 8
  %53 = getelementptr inbounds %class.KV.0, %class.KV.0* %52, i64 1
  %54 = bitcast %class.KV.0* %53 to i8*
  %55 = bitcast i8* %54 to %class.KV.0*
  %56 = load %class.hKey*, %class.hKey** %2, align 8
  %57 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %55, %class.hKey* %56, %class.hKey* %57)
  br label %71

; <label>:58                                      ; preds = %39
  %59 = load %class.KV.0*, %class.KV.0** %node1, align 8
  %60 = getelementptr inbounds %class.KV.0, %class.KV.0* %59, i64 0
  %61 = bitcast %class.KV.0* %60 to i8*
  %62 = bitcast i8* %61 to %class.KV.0*
  %63 = load %class.hKey*, %class.hKey** %2, align 8
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %62, %class.hKey* %63, %class.hKey* %64)
  %65 = load %class.KV.0*, %class.KV.0** %node1, align 8
  %66 = getelementptr inbounds %class.KV.0, %class.KV.0* %65, i64 1
  %67 = bitcast %class.KV.0* %66 to i8*
  %68 = bitcast i8* %67 to %class.KV.0*
  %69 = load %class.hKey*, %class.hKey** %5, align 8
  %70 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %68, %class.hKey* %69, %class.hKey* %70)
  br label %71

; <label>:71                                      ; preds = %58, %45
  %72 = load i32, i32* %h0piece, align 4
  %73 = zext i32 %72 to i64
  %74 = shl i64 1, %73
  %75 = load i32, i32* %h1piece, align 4
  %76 = zext i32 %75 to i64
  %77 = shl i64 1, %76
  %78 = or i64 %74, %77
  %79 = shl i64 %78, 1
  %80 = or i64 %79, 1
  %81 = load %class.KV.0*, %class.KV.0** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %agg.result, i64 %80, %class.KV.0* %81)
  br label %82

; <label>:82                                      ; preds = %71, %18
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj0EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV* noalias sret %agg.result, %class.KV* dereferenceable(16) %kv, i64 %h, %class.hKey* %key, %class.hKey* %val, i64* %cptr) #2 comdat align 2 {
  %1 = alloca %class.KV*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca %class.hKey*, align 8
  %5 = alloca i64*, align 8
  %data = alloca %class.KV.0*, align 8
  %bm = alloca i64, align 8
  %hpiece = alloca i32, align 4
  %count = alloca i32, align 4
  %i = alloca i32, align 4
  %exists = alloca i8, align 1
  %node = alloca %class.KV.0*, align 8
  %6 = alloca %class.KV.0, align 8
  %childkv = alloca %class.KV.0, align 8
  %node1 = alloca %class.KV.0*, align 8
  %childkv2 = alloca %class.KV.0, align 8
  %node3 = alloca %class.KV.0*, align 8
  %node4 = alloca %class.KV.0*, align 8
  store %class.KV* %kv, %class.KV** %1, align 8
  store i64 %h, i64* %2, align 8
  store %class.hKey* %key, %class.hKey** %3, align 8
  store %class.hKey* %val, %class.hKey** %4, align 8
  store i64* %cptr, i64** %5, align 8
  %7 = load %class.KV*, %class.KV** %1, align 8
  %8 = getelementptr inbounds %class.KV, %class.KV* %7, i32 0, i32 1
  %9 = bitcast %"union.KV<hKey, hKey, 0>::Val"* %8 to %class.KV.0**
  %10 = load %class.KV.0*, %class.KV.0** %9, align 8
  store %class.KV.0* %10, %class.KV.0** %data, align 8
  %11 = load %class.KV*, %class.KV** %1, align 8
  %12 = getelementptr inbounds %class.KV, %class.KV* %11, i32 0, i32 0
  %13 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %12 to i64*
  %14 = load i64, i64* %13, align 8
  %15 = lshr i64 %14, 1
  store i64 %15, i64* %bm, align 8
  %16 = load i64, i64* %2, align 8
  %17 = and i64 %16, 63
  %18 = urem i64 %17, 63
  %19 = trunc i64 %18 to i32
  store i32 %19, i32* %hpiece, align 4
  %20 = load i64, i64* %bm, align 8
  %21 = call i64 @llvm.ctpop.i64(i64 %20)
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %count, align 4
  %23 = load i64, i64* %bm, align 8
  %24 = shl i64 %23, 1
  %25 = load i32, i32* %hpiece, align 4
  %26 = sub i32 63, %25
  %27 = zext i32 %26 to i64
  %28 = shl i64 %24, %27
  %29 = call i64 @llvm.ctpop.i64(i64 %28)
  %30 = trunc i64 %29 to i32
  store i32 %30, i32* %i, align 4
  %31 = load i64, i64* %bm, align 8
  %32 = load i32, i32* %hpiece, align 4
  %33 = zext i32 %32 to i64
  %34 = shl i64 1, %33
  %35 = and i64 %31, %34
  %36 = icmp ne i64 %35, 0
  %37 = zext i1 %36 to i8
  store i8 %37, i8* %exists, align 1
  %38 = load i8, i8* %exists, align 1
  %39 = trunc i8 %38 to i1
  br i1 %39, label %40, label %131

; <label>:40                                      ; preds = %0
  %41 = load i32, i32* %i, align 4
  %42 = zext i32 %41 to i64
  %43 = load %class.KV.0*, %class.KV.0** %data, align 8
  %44 = getelementptr inbounds %class.KV.0, %class.KV.0* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.0, %class.KV.0* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<hKey, hKey, 1>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %112

; <label>:50                                      ; preds = %40
  %51 = load i32, i32* %i, align 4
  %52 = zext i32 %51 to i64
  %53 = load %class.KV.0*, %class.KV.0** %data, align 8
  %54 = getelementptr inbounds %class.KV.0, %class.KV.0* %53, i64 %52
  %55 = getelementptr inbounds %class.KV.0, %class.KV.0* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<hKey, hKey, 1>::Key"* %55 to %class.hKey**
  %57 = load %class.hKey*, %class.hKey** %56, align 8
  %58 = load %class.hKey*, %class.hKey** %3, align 8
  %59 = call zeroext i1 @_ZNK4hKeyeqERKS_(%class.hKey* %57, %class.hKey* dereferenceable(8) %58)
  br i1 %59, label %60, label %72

; <label>:60                                      ; preds = %50
  %61 = load %class.KV.0*, %class.KV.0** %data, align 8
  %62 = load i32, i32* %count, align 4
  %63 = load i32, i32* %i, align 4
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  %65 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %6, %class.hKey* %64, %class.hKey* %65)
  %66 = call %class.KV.0* @_ZN2KVI4hKeyS0_Lj1EE11update_nodeEPKS1_jjRS2_(%class.KV.0* %61, i32 %62, i32 %63, %class.KV.0* dereferenceable(16) %6)
  store %class.KV.0* %66, %class.KV.0** %node, align 8
  %67 = load %class.KV*, %class.KV** %1, align 8
  %68 = getelementptr inbounds %class.KV, %class.KV* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %68 to i64*
  %70 = load i64, i64* %69, align 8
  %71 = load %class.KV.0*, %class.KV.0** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %agg.result, i64 %70, %class.KV.0* %71)
  br label %180

; <label>:72                                      ; preds = %50
  %73 = load i64*, i64** %5, align 8
  %74 = load i64, i64* %73, align 8
  %75 = add i64 %74, 1
  store i64 %75, i64* %73, align 8
  %76 = load i32, i32* %i, align 4
  %77 = zext i32 %76 to i64
  %78 = load %class.KV.0*, %class.KV.0** %data, align 8
  %79 = getelementptr inbounds %class.KV.0, %class.KV.0* %78, i64 %77
  %80 = getelementptr inbounds %class.KV.0, %class.KV.0* %79, i32 0, i32 0
  %81 = bitcast %"union.KV<hKey, hKey, 1>::Key"* %80 to %class.hKey**
  %82 = load %class.hKey*, %class.hKey** %81, align 8
  %83 = call i64 @_ZNK4hKey4hashEv(%class.hKey* %82)
  %84 = lshr i64 %83, 10
  %85 = load i32, i32* %i, align 4
  %86 = zext i32 %85 to i64
  %87 = load %class.KV.0*, %class.KV.0** %data, align 8
  %88 = getelementptr inbounds %class.KV.0, %class.KV.0* %87, i64 %86
  %89 = getelementptr inbounds %class.KV.0, %class.KV.0* %88, i32 0, i32 0
  %90 = bitcast %"union.KV<hKey, hKey, 1>::Key"* %89 to %class.hKey**
  %91 = load %class.hKey*, %class.hKey** %90, align 8
  %92 = load i32, i32* %i, align 4
  %93 = zext i32 %92 to i64
  %94 = load %class.KV.0*, %class.KV.0** %data, align 8
  %95 = getelementptr inbounds %class.KV.0, %class.KV.0* %94, i64 %93
  %96 = getelementptr inbounds %class.KV.0, %class.KV.0* %95, i32 0, i32 1
  %97 = bitcast %"union.KV<hKey, hKey, 1>::Val"* %96 to %class.hKey**
  %98 = load %class.hKey*, %class.hKey** %97, align 8
  %99 = load i64, i64* %2, align 8
  %100 = lshr i64 %99, 6
  %101 = load %class.hKey*, %class.hKey** %3, align 8
  %102 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.0* sret %childkv, i64 %84, %class.hKey* %91, %class.hKey* %98, i64 %100, %class.hKey* %101, %class.hKey* %102)
  %103 = load %class.KV.0*, %class.KV.0** %data, align 8
  %104 = load i32, i32* %count, align 4
  %105 = load i32, i32* %i, align 4
  %106 = call %class.KV.0* @_ZN2KVI4hKeyS0_Lj1EE11update_nodeEPKS1_jjRS2_(%class.KV.0* %103, i32 %104, i32 %105, %class.KV.0* dereferenceable(16) %childkv)
  store %class.KV.0* %106, %class.KV.0** %node1, align 8
  %107 = load %class.KV*, %class.KV** %1, align 8
  %108 = getelementptr inbounds %class.KV, %class.KV* %107, i32 0, i32 0
  %109 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %108 to i64*
  %110 = load i64, i64* %109, align 8
  %111 = load %class.KV.0*, %class.KV.0** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %agg.result, i64 %110, %class.KV.0* %111)
  br label %180

; <label>:112                                     ; preds = %40
  %113 = load i32, i32* %i, align 4
  %114 = zext i32 %113 to i64
  %115 = load %class.KV.0*, %class.KV.0** %data, align 8
  %116 = getelementptr inbounds %class.KV.0, %class.KV.0* %115, i64 %114
  %117 = load i64, i64* %2, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.hKey*, %class.hKey** %3, align 8
  %120 = load %class.hKey*, %class.hKey** %4, align 8
  %121 = load i64*, i64** %5, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.0* sret %childkv2, %class.KV.0* dereferenceable(16) %116, i64 %118, %class.hKey* %119, %class.hKey* %120, i64* %121)
  %122 = load %class.KV.0*, %class.KV.0** %data, align 8
  %123 = load i32, i32* %count, align 4
  %124 = load i32, i32* %i, align 4
  %125 = call %class.KV.0* @_ZN2KVI4hKeyS0_Lj1EE11update_nodeEPKS1_jjRS2_(%class.KV.0* %122, i32 %123, i32 %124, %class.KV.0* dereferenceable(16) %childkv2)
  store %class.KV.0* %125, %class.KV.0** %node3, align 8
  %126 = load %class.KV*, %class.KV** %1, align 8
  %127 = getelementptr inbounds %class.KV, %class.KV* %126, i32 0, i32 0
  %128 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %127 to i64*
  %129 = load i64, i64* %128, align 8
  %130 = load %class.KV.0*, %class.KV.0** %node3, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %agg.result, i64 %129, %class.KV.0* %130)
  br label %180

; <label>:131                                     ; preds = %0
  %132 = load i64*, i64** %5, align 8
  %133 = load i64, i64* %132, align 8
  %134 = add i64 %133, 1
  store i64 %134, i64* %132, align 8
  %135 = load i32, i32* %count, align 4
  %136 = add i32 %135, 1
  %137 = zext i32 %136 to i64
  %138 = mul i64 %137, 16
  %139 = call noalias i8* @malloc(i64 %138) #8
  %140 = bitcast i8* %139 to %class.KV.0*
  store %class.KV.0* %140, %class.KV.0** %node4, align 8
  %141 = load %class.KV.0*, %class.KV.0** %node4, align 8
  %142 = bitcast %class.KV.0* %141 to i8*
  %143 = load %class.KV.0*, %class.KV.0** %data, align 8
  %144 = bitcast %class.KV.0* %143 to i8*
  %145 = load i32, i32* %i, align 4
  %146 = zext i32 %145 to i64
  %147 = mul i64 %146, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %142, i8* %144, i64 %147, i32 8, i1 false)
  %148 = load i32, i32* %i, align 4
  %149 = add i32 %148, 1
  %150 = zext i32 %149 to i64
  %151 = load %class.KV.0*, %class.KV.0** %node4, align 8
  %152 = getelementptr inbounds %class.KV.0, %class.KV.0* %151, i64 %150
  %153 = bitcast %class.KV.0* %152 to i8*
  %154 = load i32, i32* %i, align 4
  %155 = zext i32 %154 to i64
  %156 = load %class.KV.0*, %class.KV.0** %data, align 8
  %157 = getelementptr inbounds %class.KV.0, %class.KV.0* %156, i64 %155
  %158 = bitcast %class.KV.0* %157 to i8*
  %159 = load i32, i32* %count, align 4
  %160 = load i32, i32* %i, align 4
  %161 = sub i32 %159, %160
  %162 = zext i32 %161 to i64
  %163 = mul i64 %162, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %153, i8* %158, i64 %163, i32 8, i1 false)
  %164 = load %class.KV.0*, %class.KV.0** %node4, align 8
  %165 = load i32, i32* %i, align 4
  %166 = zext i32 %165 to i64
  %167 = getelementptr inbounds %class.KV.0, %class.KV.0* %164, i64 %166
  %168 = bitcast %class.KV.0* %167 to i8*
  %169 = bitcast i8* %168 to %class.KV.0*
  %170 = load %class.hKey*, %class.hKey** %3, align 8
  %171 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %169, %class.hKey* %170, %class.hKey* %171)
  %172 = load i64, i64* %bm, align 8
  %173 = load i32, i32* %hpiece, align 4
  %174 = zext i32 %173 to i64
  %175 = shl i64 1, %174
  %176 = or i64 %172, %175
  %177 = shl i64 %176, 1
  %178 = or i64 %177, 1
  %179 = load %class.KV.0*, %class.KV.0** %node4, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %agg.result, i64 %178, %class.KV.0* %179)
  br label %180

; <label>:180                                     ; preds = %131, %112, %72, %60
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj0EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 0>::Key"* %this, %class.hKey* %key) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 0>::Key"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 0>::Key"* %this, %"union.KV<hKey, hKey, 0>::Key"** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 0>::Key"*, %"union.KV<hKey, hKey, 0>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj0EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 0>::Val"* %this, %class.hKey* %val) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 0>::Val"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 0>::Val"* %this, %"union.KV<hKey, hKey, 0>::Val"** %1, align 8
  store %class.hKey* %val, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 0>::Val"*, %"union.KV<hKey, hKey, 0>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 0>::Val"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj1EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.0* noalias sret %agg.result, i64 %h0, %class.hKey* %k0, %class.hKey* %v0, i64 %h1, %class.hKey* %k1, %class.hKey* %v1) #2 comdat align 2 {
  %1 = alloca i64, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hKey*, align 8
  %6 = alloca %class.hKey*, align 8
  %h0piece = alloca i32, align 4
  %h1piece = alloca i32, align 4
  %childkv = alloca %class.KV.1, align 8
  %node = alloca %class.KV.1*, align 8
  %node1 = alloca %class.KV.1*, align 8
  store i64 %h0, i64* %1, align 8
  store %class.hKey* %k0, %class.hKey** %2, align 8
  store %class.hKey* %v0, %class.hKey** %3, align 8
  store i64 %h1, i64* %4, align 8
  store %class.hKey* %k1, %class.hKey** %5, align 8
  store %class.hKey* %v1, %class.hKey** %6, align 8
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 63
  %9 = urem i64 %8, 63
  %10 = trunc i64 %9 to i32
  store i32 %10, i32* %h0piece, align 4
  %11 = load i64, i64* %4, align 8
  %12 = and i64 %11, 63
  %13 = urem i64 %12, 63
  %14 = trunc i64 %13 to i32
  store i32 %14, i32* %h1piece, align 4
  %15 = load i32, i32* %h0piece, align 4
  %16 = load i32, i32* %h1piece, align 4
  %17 = icmp eq i32 %15, %16
  br i1 %17, label %18, label %39

; <label>:18                                      ; preds = %0
  %19 = load i64, i64* %1, align 8
  %20 = lshr i64 %19, 6
  %21 = load %class.hKey*, %class.hKey** %2, align 8
  %22 = load %class.hKey*, %class.hKey** %3, align 8
  %23 = load i64, i64* %4, align 8
  %24 = lshr i64 %23, 6
  %25 = load %class.hKey*, %class.hKey** %5, align 8
  %26 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.1* sret %childkv, i64 %20, %class.hKey* %21, %class.hKey* %22, i64 %24, %class.hKey* %25, %class.hKey* %26)
  %27 = call noalias i8* @malloc(i64 16) #8
  %28 = bitcast i8* %27 to %class.KV.1*
  store %class.KV.1* %28, %class.KV.1** %node, align 8
  %29 = load %class.KV.1*, %class.KV.1** %node, align 8
  %30 = getelementptr inbounds %class.KV.1, %class.KV.1* %29, i64 0
  %31 = bitcast %class.KV.1* %30 to i8*
  %32 = bitcast i8* %31 to %class.KV.1*
  call void @_ZN2KVI4hKeyS0_Lj2EEC2ERKS1_(%class.KV.1* %32, %class.KV.1* dereferenceable(16) %childkv)
  %33 = load i32, i32* %h0piece, align 4
  %34 = zext i32 %33 to i64
  %35 = shl i64 1, %34
  %36 = shl i64 %35, 1
  %37 = or i64 %36, 1
  %38 = load %class.KV.1*, %class.KV.1** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %agg.result, i64 %37, %class.KV.1* %38)
  br label %82

; <label>:39                                      ; preds = %0
  %40 = call noalias i8* @malloc(i64 32) #8
  %41 = bitcast i8* %40 to %class.KV.1*
  store %class.KV.1* %41, %class.KV.1** %node1, align 8
  %42 = load i32, i32* %h1piece, align 4
  %43 = load i32, i32* %h0piece, align 4
  %44 = icmp ult i32 %42, %43
  br i1 %44, label %45, label %58

; <label>:45                                      ; preds = %39
  %46 = load %class.KV.1*, %class.KV.1** %node1, align 8
  %47 = getelementptr inbounds %class.KV.1, %class.KV.1* %46, i64 0
  %48 = bitcast %class.KV.1* %47 to i8*
  %49 = bitcast i8* %48 to %class.KV.1*
  %50 = load %class.hKey*, %class.hKey** %5, align 8
  %51 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %49, %class.hKey* %50, %class.hKey* %51)
  %52 = load %class.KV.1*, %class.KV.1** %node1, align 8
  %53 = getelementptr inbounds %class.KV.1, %class.KV.1* %52, i64 1
  %54 = bitcast %class.KV.1* %53 to i8*
  %55 = bitcast i8* %54 to %class.KV.1*
  %56 = load %class.hKey*, %class.hKey** %2, align 8
  %57 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %55, %class.hKey* %56, %class.hKey* %57)
  br label %71

; <label>:58                                      ; preds = %39
  %59 = load %class.KV.1*, %class.KV.1** %node1, align 8
  %60 = getelementptr inbounds %class.KV.1, %class.KV.1* %59, i64 0
  %61 = bitcast %class.KV.1* %60 to i8*
  %62 = bitcast i8* %61 to %class.KV.1*
  %63 = load %class.hKey*, %class.hKey** %2, align 8
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %62, %class.hKey* %63, %class.hKey* %64)
  %65 = load %class.KV.1*, %class.KV.1** %node1, align 8
  %66 = getelementptr inbounds %class.KV.1, %class.KV.1* %65, i64 1
  %67 = bitcast %class.KV.1* %66 to i8*
  %68 = bitcast i8* %67 to %class.KV.1*
  %69 = load %class.hKey*, %class.hKey** %5, align 8
  %70 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %68, %class.hKey* %69, %class.hKey* %70)
  br label %71

; <label>:71                                      ; preds = %58, %45
  %72 = load i32, i32* %h0piece, align 4
  %73 = zext i32 %72 to i64
  %74 = shl i64 1, %73
  %75 = load i32, i32* %h1piece, align 4
  %76 = zext i32 %75 to i64
  %77 = shl i64 1, %76
  %78 = or i64 %74, %77
  %79 = shl i64 %78, 1
  %80 = or i64 %79, 1
  %81 = load %class.KV.1*, %class.KV.1** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %agg.result, i64 %80, %class.KV.1* %81)
  br label %82

; <label>:82                                      ; preds = %71, %18
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj1EEC2ERKS1_(%class.KV.0* %this, %class.KV.0* dereferenceable(16) %o) unnamed_addr #0 comdat align 2 {
  %1 = alloca %class.KV.0*, align 8
  %2 = alloca %class.KV.0*, align 8
  store %class.KV.0* %this, %class.KV.0** %1, align 8
  store %class.KV.0* %o, %class.KV.0** %2, align 8
  %3 = load %class.KV.0*, %class.KV.0** %1, align 8
  %4 = getelementptr inbounds %class.KV.0, %class.KV.0* %3, i32 0, i32 0
  %5 = load %class.KV.0*, %class.KV.0** %2, align 8
  %6 = getelementptr inbounds %class.KV.0, %class.KV.0* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<hKey, hKey, 1>::Key"* %4 to i8*
  %8 = bitcast %"union.KV<hKey, hKey, 1>::Key"* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %7, i8* %8, i64 8, i32 8, i1 false)
  %9 = getelementptr inbounds %class.KV.0, %class.KV.0* %3, i32 0, i32 1
  %10 = load %class.KV.0*, %class.KV.0** %2, align 8
  %11 = getelementptr inbounds %class.KV.0, %class.KV.0* %10, i32 0, i32 1
  %12 = bitcast %"union.KV<hKey, hKey, 1>::Val"* %9 to i8*
  %13 = bitcast %"union.KV<hKey, hKey, 1>::Val"* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %13, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %this, i64 %bm, %class.KV.0* %kv) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.KV.0*, align 8
  store %class.KV* %this, %class.KV** %1, align 8
  store i64 %bm, i64* %2, align 8
  store %class.KV.0* %kv, %class.KV.0** %3, align 8
  %4 = load %class.KV*, %class.KV** %1, align 8
  %5 = getelementptr inbounds %class.KV, %class.KV* %4, i32 0, i32 0
  %6 = load i64, i64* %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EE3KeyC2Em(%"union.KV<hKey, hKey, 0>::Key"* %5, i64 %6)
  %7 = getelementptr inbounds %class.KV, %class.KV* %4, i32 0, i32 1
  %8 = load %class.KV.0*, %class.KV.0** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj0EE3ValC2EPKS_IS0_S0_Lj1EE(%"union.KV<hKey, hKey, 0>::Val"* %7, %class.KV.0* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %this, %class.hKey* %key, %class.hKey* %val) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.0*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  store %class.KV.0* %this, %class.KV.0** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  store %class.hKey* %val, %class.hKey** %3, align 8
  %4 = load %class.KV.0*, %class.KV.0** %1, align 8
  %5 = getelementptr inbounds %class.KV.0, %class.KV.0* %4, i32 0, i32 0
  %6 = load %class.hKey*, %class.hKey** %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 1>::Key"* %5, %class.hKey* %6)
  %7 = getelementptr inbounds %class.KV.0, %class.KV.0* %4, i32 0, i32 1
  %8 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 1>::Val"* %7, %class.hKey* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj2EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.1* noalias sret %agg.result, i64 %h0, %class.hKey* %k0, %class.hKey* %v0, i64 %h1, %class.hKey* %k1, %class.hKey* %v1) #2 comdat align 2 {
  %1 = alloca i64, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hKey*, align 8
  %6 = alloca %class.hKey*, align 8
  %h0piece = alloca i32, align 4
  %h1piece = alloca i32, align 4
  %childkv = alloca %class.KV.2, align 8
  %node = alloca %class.KV.2*, align 8
  %node1 = alloca %class.KV.2*, align 8
  store i64 %h0, i64* %1, align 8
  store %class.hKey* %k0, %class.hKey** %2, align 8
  store %class.hKey* %v0, %class.hKey** %3, align 8
  store i64 %h1, i64* %4, align 8
  store %class.hKey* %k1, %class.hKey** %5, align 8
  store %class.hKey* %v1, %class.hKey** %6, align 8
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 63
  %9 = urem i64 %8, 63
  %10 = trunc i64 %9 to i32
  store i32 %10, i32* %h0piece, align 4
  %11 = load i64, i64* %4, align 8
  %12 = and i64 %11, 63
  %13 = urem i64 %12, 63
  %14 = trunc i64 %13 to i32
  store i32 %14, i32* %h1piece, align 4
  %15 = load i32, i32* %h0piece, align 4
  %16 = load i32, i32* %h1piece, align 4
  %17 = icmp eq i32 %15, %16
  br i1 %17, label %18, label %39

; <label>:18                                      ; preds = %0
  %19 = load i64, i64* %1, align 8
  %20 = lshr i64 %19, 6
  %21 = load %class.hKey*, %class.hKey** %2, align 8
  %22 = load %class.hKey*, %class.hKey** %3, align 8
  %23 = load i64, i64* %4, align 8
  %24 = lshr i64 %23, 6
  %25 = load %class.hKey*, %class.hKey** %5, align 8
  %26 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.2* sret %childkv, i64 %20, %class.hKey* %21, %class.hKey* %22, i64 %24, %class.hKey* %25, %class.hKey* %26)
  %27 = call noalias i8* @malloc(i64 16) #8
  %28 = bitcast i8* %27 to %class.KV.2*
  store %class.KV.2* %28, %class.KV.2** %node, align 8
  %29 = load %class.KV.2*, %class.KV.2** %node, align 8
  %30 = getelementptr inbounds %class.KV.2, %class.KV.2* %29, i64 0
  %31 = bitcast %class.KV.2* %30 to i8*
  %32 = bitcast i8* %31 to %class.KV.2*
  call void @_ZN2KVI4hKeyS0_Lj3EEC2ERKS1_(%class.KV.2* %32, %class.KV.2* dereferenceable(16) %childkv)
  %33 = load i32, i32* %h0piece, align 4
  %34 = zext i32 %33 to i64
  %35 = shl i64 1, %34
  %36 = shl i64 %35, 1
  %37 = or i64 %36, 1
  %38 = load %class.KV.2*, %class.KV.2** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %agg.result, i64 %37, %class.KV.2* %38)
  br label %82

; <label>:39                                      ; preds = %0
  %40 = call noalias i8* @malloc(i64 32) #8
  %41 = bitcast i8* %40 to %class.KV.2*
  store %class.KV.2* %41, %class.KV.2** %node1, align 8
  %42 = load i32, i32* %h1piece, align 4
  %43 = load i32, i32* %h0piece, align 4
  %44 = icmp ult i32 %42, %43
  br i1 %44, label %45, label %58

; <label>:45                                      ; preds = %39
  %46 = load %class.KV.2*, %class.KV.2** %node1, align 8
  %47 = getelementptr inbounds %class.KV.2, %class.KV.2* %46, i64 0
  %48 = bitcast %class.KV.2* %47 to i8*
  %49 = bitcast i8* %48 to %class.KV.2*
  %50 = load %class.hKey*, %class.hKey** %5, align 8
  %51 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %49, %class.hKey* %50, %class.hKey* %51)
  %52 = load %class.KV.2*, %class.KV.2** %node1, align 8
  %53 = getelementptr inbounds %class.KV.2, %class.KV.2* %52, i64 1
  %54 = bitcast %class.KV.2* %53 to i8*
  %55 = bitcast i8* %54 to %class.KV.2*
  %56 = load %class.hKey*, %class.hKey** %2, align 8
  %57 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %55, %class.hKey* %56, %class.hKey* %57)
  br label %71

; <label>:58                                      ; preds = %39
  %59 = load %class.KV.2*, %class.KV.2** %node1, align 8
  %60 = getelementptr inbounds %class.KV.2, %class.KV.2* %59, i64 0
  %61 = bitcast %class.KV.2* %60 to i8*
  %62 = bitcast i8* %61 to %class.KV.2*
  %63 = load %class.hKey*, %class.hKey** %2, align 8
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %62, %class.hKey* %63, %class.hKey* %64)
  %65 = load %class.KV.2*, %class.KV.2** %node1, align 8
  %66 = getelementptr inbounds %class.KV.2, %class.KV.2* %65, i64 1
  %67 = bitcast %class.KV.2* %66 to i8*
  %68 = bitcast i8* %67 to %class.KV.2*
  %69 = load %class.hKey*, %class.hKey** %5, align 8
  %70 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %68, %class.hKey* %69, %class.hKey* %70)
  br label %71

; <label>:71                                      ; preds = %58, %45
  %72 = load i32, i32* %h0piece, align 4
  %73 = zext i32 %72 to i64
  %74 = shl i64 1, %73
  %75 = load i32, i32* %h1piece, align 4
  %76 = zext i32 %75 to i64
  %77 = shl i64 1, %76
  %78 = or i64 %74, %77
  %79 = shl i64 %78, 1
  %80 = or i64 %79, 1
  %81 = load %class.KV.2*, %class.KV.2** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %agg.result, i64 %80, %class.KV.2* %81)
  br label %82

; <label>:82                                      ; preds = %71, %18
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj2EEC2ERKS1_(%class.KV.1* %this, %class.KV.1* dereferenceable(16) %o) unnamed_addr #0 comdat align 2 {
  %1 = alloca %class.KV.1*, align 8
  %2 = alloca %class.KV.1*, align 8
  store %class.KV.1* %this, %class.KV.1** %1, align 8
  store %class.KV.1* %o, %class.KV.1** %2, align 8
  %3 = load %class.KV.1*, %class.KV.1** %1, align 8
  %4 = getelementptr inbounds %class.KV.1, %class.KV.1* %3, i32 0, i32 0
  %5 = load %class.KV.1*, %class.KV.1** %2, align 8
  %6 = getelementptr inbounds %class.KV.1, %class.KV.1* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<hKey, hKey, 2>::Key"* %4 to i8*
  %8 = bitcast %"union.KV<hKey, hKey, 2>::Key"* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %7, i8* %8, i64 8, i32 8, i1 false)
  %9 = getelementptr inbounds %class.KV.1, %class.KV.1* %3, i32 0, i32 1
  %10 = load %class.KV.1*, %class.KV.1** %2, align 8
  %11 = getelementptr inbounds %class.KV.1, %class.KV.1* %10, i32 0, i32 1
  %12 = bitcast %"union.KV<hKey, hKey, 2>::Val"* %9 to i8*
  %13 = bitcast %"union.KV<hKey, hKey, 2>::Val"* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %13, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %this, i64 %bm, %class.KV.1* %kv) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.0*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.KV.1*, align 8
  store %class.KV.0* %this, %class.KV.0** %1, align 8
  store i64 %bm, i64* %2, align 8
  store %class.KV.1* %kv, %class.KV.1** %3, align 8
  %4 = load %class.KV.0*, %class.KV.0** %1, align 8
  %5 = getelementptr inbounds %class.KV.0, %class.KV.0* %4, i32 0, i32 0
  %6 = load i64, i64* %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EE3KeyC2Em(%"union.KV<hKey, hKey, 1>::Key"* %5, i64 %6)
  %7 = getelementptr inbounds %class.KV.0, %class.KV.0* %4, i32 0, i32 1
  %8 = load %class.KV.1*, %class.KV.1** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EE3ValC2EPKS_IS0_S0_Lj2EE(%"union.KV<hKey, hKey, 1>::Val"* %7, %class.KV.1* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %this, %class.hKey* %key, %class.hKey* %val) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.1*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  store %class.KV.1* %this, %class.KV.1** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  store %class.hKey* %val, %class.hKey** %3, align 8
  %4 = load %class.KV.1*, %class.KV.1** %1, align 8
  %5 = getelementptr inbounds %class.KV.1, %class.KV.1* %4, i32 0, i32 0
  %6 = load %class.hKey*, %class.hKey** %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 2>::Key"* %5, %class.hKey* %6)
  %7 = getelementptr inbounds %class.KV.1, %class.KV.1* %4, i32 0, i32 1
  %8 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 2>::Val"* %7, %class.hKey* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj3EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.2* noalias sret %agg.result, i64 %h0, %class.hKey* %k0, %class.hKey* %v0, i64 %h1, %class.hKey* %k1, %class.hKey* %v1) #2 comdat align 2 {
  %1 = alloca i64, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hKey*, align 8
  %6 = alloca %class.hKey*, align 8
  %h0piece = alloca i32, align 4
  %h1piece = alloca i32, align 4
  %childkv = alloca %class.KV.3, align 8
  %node = alloca %class.KV.3*, align 8
  %node1 = alloca %class.KV.3*, align 8
  store i64 %h0, i64* %1, align 8
  store %class.hKey* %k0, %class.hKey** %2, align 8
  store %class.hKey* %v0, %class.hKey** %3, align 8
  store i64 %h1, i64* %4, align 8
  store %class.hKey* %k1, %class.hKey** %5, align 8
  store %class.hKey* %v1, %class.hKey** %6, align 8
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 63
  %9 = urem i64 %8, 63
  %10 = trunc i64 %9 to i32
  store i32 %10, i32* %h0piece, align 4
  %11 = load i64, i64* %4, align 8
  %12 = and i64 %11, 63
  %13 = urem i64 %12, 63
  %14 = trunc i64 %13 to i32
  store i32 %14, i32* %h1piece, align 4
  %15 = load i32, i32* %h0piece, align 4
  %16 = load i32, i32* %h1piece, align 4
  %17 = icmp eq i32 %15, %16
  br i1 %17, label %18, label %39

; <label>:18                                      ; preds = %0
  %19 = load i64, i64* %1, align 8
  %20 = lshr i64 %19, 6
  %21 = load %class.hKey*, %class.hKey** %2, align 8
  %22 = load %class.hKey*, %class.hKey** %3, align 8
  %23 = load i64, i64* %4, align 8
  %24 = lshr i64 %23, 6
  %25 = load %class.hKey*, %class.hKey** %5, align 8
  %26 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.3* sret %childkv, i64 %20, %class.hKey* %21, %class.hKey* %22, i64 %24, %class.hKey* %25, %class.hKey* %26)
  %27 = call noalias i8* @malloc(i64 16) #8
  %28 = bitcast i8* %27 to %class.KV.3*
  store %class.KV.3* %28, %class.KV.3** %node, align 8
  %29 = load %class.KV.3*, %class.KV.3** %node, align 8
  %30 = getelementptr inbounds %class.KV.3, %class.KV.3* %29, i64 0
  %31 = bitcast %class.KV.3* %30 to i8*
  %32 = bitcast i8* %31 to %class.KV.3*
  call void @_ZN2KVI4hKeyS0_Lj4EEC2ERKS1_(%class.KV.3* %32, %class.KV.3* dereferenceable(16) %childkv)
  %33 = load i32, i32* %h0piece, align 4
  %34 = zext i32 %33 to i64
  %35 = shl i64 1, %34
  %36 = shl i64 %35, 1
  %37 = or i64 %36, 1
  %38 = load %class.KV.3*, %class.KV.3** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %agg.result, i64 %37, %class.KV.3* %38)
  br label %82

; <label>:39                                      ; preds = %0
  %40 = call noalias i8* @malloc(i64 32) #8
  %41 = bitcast i8* %40 to %class.KV.3*
  store %class.KV.3* %41, %class.KV.3** %node1, align 8
  %42 = load i32, i32* %h1piece, align 4
  %43 = load i32, i32* %h0piece, align 4
  %44 = icmp ult i32 %42, %43
  br i1 %44, label %45, label %58

; <label>:45                                      ; preds = %39
  %46 = load %class.KV.3*, %class.KV.3** %node1, align 8
  %47 = getelementptr inbounds %class.KV.3, %class.KV.3* %46, i64 0
  %48 = bitcast %class.KV.3* %47 to i8*
  %49 = bitcast i8* %48 to %class.KV.3*
  %50 = load %class.hKey*, %class.hKey** %5, align 8
  %51 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %49, %class.hKey* %50, %class.hKey* %51)
  %52 = load %class.KV.3*, %class.KV.3** %node1, align 8
  %53 = getelementptr inbounds %class.KV.3, %class.KV.3* %52, i64 1
  %54 = bitcast %class.KV.3* %53 to i8*
  %55 = bitcast i8* %54 to %class.KV.3*
  %56 = load %class.hKey*, %class.hKey** %2, align 8
  %57 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %55, %class.hKey* %56, %class.hKey* %57)
  br label %71

; <label>:58                                      ; preds = %39
  %59 = load %class.KV.3*, %class.KV.3** %node1, align 8
  %60 = getelementptr inbounds %class.KV.3, %class.KV.3* %59, i64 0
  %61 = bitcast %class.KV.3* %60 to i8*
  %62 = bitcast i8* %61 to %class.KV.3*
  %63 = load %class.hKey*, %class.hKey** %2, align 8
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %62, %class.hKey* %63, %class.hKey* %64)
  %65 = load %class.KV.3*, %class.KV.3** %node1, align 8
  %66 = getelementptr inbounds %class.KV.3, %class.KV.3* %65, i64 1
  %67 = bitcast %class.KV.3* %66 to i8*
  %68 = bitcast i8* %67 to %class.KV.3*
  %69 = load %class.hKey*, %class.hKey** %5, align 8
  %70 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %68, %class.hKey* %69, %class.hKey* %70)
  br label %71

; <label>:71                                      ; preds = %58, %45
  %72 = load i32, i32* %h0piece, align 4
  %73 = zext i32 %72 to i64
  %74 = shl i64 1, %73
  %75 = load i32, i32* %h1piece, align 4
  %76 = zext i32 %75 to i64
  %77 = shl i64 1, %76
  %78 = or i64 %74, %77
  %79 = shl i64 %78, 1
  %80 = or i64 %79, 1
  %81 = load %class.KV.3*, %class.KV.3** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %agg.result, i64 %80, %class.KV.3* %81)
  br label %82

; <label>:82                                      ; preds = %71, %18
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj3EEC2ERKS1_(%class.KV.2* %this, %class.KV.2* dereferenceable(16) %o) unnamed_addr #0 comdat align 2 {
  %1 = alloca %class.KV.2*, align 8
  %2 = alloca %class.KV.2*, align 8
  store %class.KV.2* %this, %class.KV.2** %1, align 8
  store %class.KV.2* %o, %class.KV.2** %2, align 8
  %3 = load %class.KV.2*, %class.KV.2** %1, align 8
  %4 = getelementptr inbounds %class.KV.2, %class.KV.2* %3, i32 0, i32 0
  %5 = load %class.KV.2*, %class.KV.2** %2, align 8
  %6 = getelementptr inbounds %class.KV.2, %class.KV.2* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<hKey, hKey, 3>::Key"* %4 to i8*
  %8 = bitcast %"union.KV<hKey, hKey, 3>::Key"* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %7, i8* %8, i64 8, i32 8, i1 false)
  %9 = getelementptr inbounds %class.KV.2, %class.KV.2* %3, i32 0, i32 1
  %10 = load %class.KV.2*, %class.KV.2** %2, align 8
  %11 = getelementptr inbounds %class.KV.2, %class.KV.2* %10, i32 0, i32 1
  %12 = bitcast %"union.KV<hKey, hKey, 3>::Val"* %9 to i8*
  %13 = bitcast %"union.KV<hKey, hKey, 3>::Val"* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %13, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %this, i64 %bm, %class.KV.2* %kv) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.1*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.KV.2*, align 8
  store %class.KV.1* %this, %class.KV.1** %1, align 8
  store i64 %bm, i64* %2, align 8
  store %class.KV.2* %kv, %class.KV.2** %3, align 8
  %4 = load %class.KV.1*, %class.KV.1** %1, align 8
  %5 = getelementptr inbounds %class.KV.1, %class.KV.1* %4, i32 0, i32 0
  %6 = load i64, i64* %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EE3KeyC2Em(%"union.KV<hKey, hKey, 2>::Key"* %5, i64 %6)
  %7 = getelementptr inbounds %class.KV.1, %class.KV.1* %4, i32 0, i32 1
  %8 = load %class.KV.2*, %class.KV.2** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EE3ValC2EPKS_IS0_S0_Lj3EE(%"union.KV<hKey, hKey, 2>::Val"* %7, %class.KV.2* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %this, %class.hKey* %key, %class.hKey* %val) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.2*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  store %class.KV.2* %this, %class.KV.2** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  store %class.hKey* %val, %class.hKey** %3, align 8
  %4 = load %class.KV.2*, %class.KV.2** %1, align 8
  %5 = getelementptr inbounds %class.KV.2, %class.KV.2* %4, i32 0, i32 0
  %6 = load %class.hKey*, %class.hKey** %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 3>::Key"* %5, %class.hKey* %6)
  %7 = getelementptr inbounds %class.KV.2, %class.KV.2* %4, i32 0, i32 1
  %8 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 3>::Val"* %7, %class.hKey* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj4EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.3* noalias sret %agg.result, i64 %h0, %class.hKey* %k0, %class.hKey* %v0, i64 %h1, %class.hKey* %k1, %class.hKey* %v1) #2 comdat align 2 {
  %1 = alloca i64, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hKey*, align 8
  %6 = alloca %class.hKey*, align 8
  %h0piece = alloca i32, align 4
  %h1piece = alloca i32, align 4
  %childkv = alloca %class.KV.4, align 8
  %node = alloca %class.KV.4*, align 8
  %node1 = alloca %class.KV.4*, align 8
  store i64 %h0, i64* %1, align 8
  store %class.hKey* %k0, %class.hKey** %2, align 8
  store %class.hKey* %v0, %class.hKey** %3, align 8
  store i64 %h1, i64* %4, align 8
  store %class.hKey* %k1, %class.hKey** %5, align 8
  store %class.hKey* %v1, %class.hKey** %6, align 8
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 63
  %9 = urem i64 %8, 63
  %10 = trunc i64 %9 to i32
  store i32 %10, i32* %h0piece, align 4
  %11 = load i64, i64* %4, align 8
  %12 = and i64 %11, 63
  %13 = urem i64 %12, 63
  %14 = trunc i64 %13 to i32
  store i32 %14, i32* %h1piece, align 4
  %15 = load i32, i32* %h0piece, align 4
  %16 = load i32, i32* %h1piece, align 4
  %17 = icmp eq i32 %15, %16
  br i1 %17, label %18, label %39

; <label>:18                                      ; preds = %0
  %19 = load i64, i64* %1, align 8
  %20 = lshr i64 %19, 6
  %21 = load %class.hKey*, %class.hKey** %2, align 8
  %22 = load %class.hKey*, %class.hKey** %3, align 8
  %23 = load i64, i64* %4, align 8
  %24 = lshr i64 %23, 6
  %25 = load %class.hKey*, %class.hKey** %5, align 8
  %26 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj5EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.4* sret %childkv, i64 %20, %class.hKey* %21, %class.hKey* %22, i64 %24, %class.hKey* %25, %class.hKey* %26)
  %27 = call noalias i8* @malloc(i64 16) #8
  %28 = bitcast i8* %27 to %class.KV.4*
  store %class.KV.4* %28, %class.KV.4** %node, align 8
  %29 = load %class.KV.4*, %class.KV.4** %node, align 8
  %30 = getelementptr inbounds %class.KV.4, %class.KV.4* %29, i64 0
  %31 = bitcast %class.KV.4* %30 to i8*
  %32 = bitcast i8* %31 to %class.KV.4*
  call void @_ZN2KVI4hKeyS0_Lj5EEC2ERKS1_(%class.KV.4* %32, %class.KV.4* dereferenceable(16) %childkv)
  %33 = load i32, i32* %h0piece, align 4
  %34 = zext i32 %33 to i64
  %35 = shl i64 1, %34
  %36 = shl i64 %35, 1
  %37 = or i64 %36, 1
  %38 = load %class.KV.4*, %class.KV.4** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3* %agg.result, i64 %37, %class.KV.4* %38)
  br label %82

; <label>:39                                      ; preds = %0
  %40 = call noalias i8* @malloc(i64 32) #8
  %41 = bitcast i8* %40 to %class.KV.4*
  store %class.KV.4* %41, %class.KV.4** %node1, align 8
  %42 = load i32, i32* %h1piece, align 4
  %43 = load i32, i32* %h0piece, align 4
  %44 = icmp ult i32 %42, %43
  br i1 %44, label %45, label %58

; <label>:45                                      ; preds = %39
  %46 = load %class.KV.4*, %class.KV.4** %node1, align 8
  %47 = getelementptr inbounds %class.KV.4, %class.KV.4* %46, i64 0
  %48 = bitcast %class.KV.4* %47 to i8*
  %49 = bitcast i8* %48 to %class.KV.4*
  %50 = load %class.hKey*, %class.hKey** %5, align 8
  %51 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj5EEC2EPKS0_S3_(%class.KV.4* %49, %class.hKey* %50, %class.hKey* %51)
  %52 = load %class.KV.4*, %class.KV.4** %node1, align 8
  %53 = getelementptr inbounds %class.KV.4, %class.KV.4* %52, i64 1
  %54 = bitcast %class.KV.4* %53 to i8*
  %55 = bitcast i8* %54 to %class.KV.4*
  %56 = load %class.hKey*, %class.hKey** %2, align 8
  %57 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj5EEC2EPKS0_S3_(%class.KV.4* %55, %class.hKey* %56, %class.hKey* %57)
  br label %71

; <label>:58                                      ; preds = %39
  %59 = load %class.KV.4*, %class.KV.4** %node1, align 8
  %60 = getelementptr inbounds %class.KV.4, %class.KV.4* %59, i64 0
  %61 = bitcast %class.KV.4* %60 to i8*
  %62 = bitcast i8* %61 to %class.KV.4*
  %63 = load %class.hKey*, %class.hKey** %2, align 8
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj5EEC2EPKS0_S3_(%class.KV.4* %62, %class.hKey* %63, %class.hKey* %64)
  %65 = load %class.KV.4*, %class.KV.4** %node1, align 8
  %66 = getelementptr inbounds %class.KV.4, %class.KV.4* %65, i64 1
  %67 = bitcast %class.KV.4* %66 to i8*
  %68 = bitcast i8* %67 to %class.KV.4*
  %69 = load %class.hKey*, %class.hKey** %5, align 8
  %70 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj5EEC2EPKS0_S3_(%class.KV.4* %68, %class.hKey* %69, %class.hKey* %70)
  br label %71

; <label>:71                                      ; preds = %58, %45
  %72 = load i32, i32* %h0piece, align 4
  %73 = zext i32 %72 to i64
  %74 = shl i64 1, %73
  %75 = load i32, i32* %h1piece, align 4
  %76 = zext i32 %75 to i64
  %77 = shl i64 1, %76
  %78 = or i64 %74, %77
  %79 = shl i64 %78, 1
  %80 = or i64 %79, 1
  %81 = load %class.KV.4*, %class.KV.4** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3* %agg.result, i64 %80, %class.KV.4* %81)
  br label %82

; <label>:82                                      ; preds = %71, %18
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj4EEC2ERKS1_(%class.KV.3* %this, %class.KV.3* dereferenceable(16) %o) unnamed_addr #0 comdat align 2 {
  %1 = alloca %class.KV.3*, align 8
  %2 = alloca %class.KV.3*, align 8
  store %class.KV.3* %this, %class.KV.3** %1, align 8
  store %class.KV.3* %o, %class.KV.3** %2, align 8
  %3 = load %class.KV.3*, %class.KV.3** %1, align 8
  %4 = getelementptr inbounds %class.KV.3, %class.KV.3* %3, i32 0, i32 0
  %5 = load %class.KV.3*, %class.KV.3** %2, align 8
  %6 = getelementptr inbounds %class.KV.3, %class.KV.3* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<hKey, hKey, 4>::Key"* %4 to i8*
  %8 = bitcast %"union.KV<hKey, hKey, 4>::Key"* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %7, i8* %8, i64 8, i32 8, i1 false)
  %9 = getelementptr inbounds %class.KV.3, %class.KV.3* %3, i32 0, i32 1
  %10 = load %class.KV.3*, %class.KV.3** %2, align 8
  %11 = getelementptr inbounds %class.KV.3, %class.KV.3* %10, i32 0, i32 1
  %12 = bitcast %"union.KV<hKey, hKey, 4>::Val"* %9 to i8*
  %13 = bitcast %"union.KV<hKey, hKey, 4>::Val"* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %13, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %this, i64 %bm, %class.KV.3* %kv) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.2*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.KV.3*, align 8
  store %class.KV.2* %this, %class.KV.2** %1, align 8
  store i64 %bm, i64* %2, align 8
  store %class.KV.3* %kv, %class.KV.3** %3, align 8
  %4 = load %class.KV.2*, %class.KV.2** %1, align 8
  %5 = getelementptr inbounds %class.KV.2, %class.KV.2* %4, i32 0, i32 0
  %6 = load i64, i64* %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EE3KeyC2Em(%"union.KV<hKey, hKey, 3>::Key"* %5, i64 %6)
  %7 = getelementptr inbounds %class.KV.2, %class.KV.2* %4, i32 0, i32 1
  %8 = load %class.KV.3*, %class.KV.3** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EE3ValC2EPKS_IS0_S0_Lj4EE(%"union.KV<hKey, hKey, 3>::Val"* %7, %class.KV.3* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %this, %class.hKey* %key, %class.hKey* %val) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.3*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  store %class.KV.3* %this, %class.KV.3** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  store %class.hKey* %val, %class.hKey** %3, align 8
  %4 = load %class.KV.3*, %class.KV.3** %1, align 8
  %5 = getelementptr inbounds %class.KV.3, %class.KV.3* %4, i32 0, i32 0
  %6 = load %class.hKey*, %class.hKey** %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 4>::Key"* %5, %class.hKey* %6)
  %7 = getelementptr inbounds %class.KV.3, %class.KV.3* %4, i32 0, i32 1
  %8 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 4>::Val"* %7, %class.hKey* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj5EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.4* noalias sret %agg.result, i64 %h0, %class.hKey* %k0, %class.hKey* %v0, i64 %h1, %class.hKey* %k1, %class.hKey* %v1) #2 comdat align 2 {
  %1 = alloca i64, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hKey*, align 8
  %6 = alloca %class.hKey*, align 8
  %h0piece = alloca i32, align 4
  %h1piece = alloca i32, align 4
  %childkv = alloca %class.KV.5, align 8
  %node = alloca %class.KV.5*, align 8
  %node1 = alloca %class.KV.5*, align 8
  store i64 %h0, i64* %1, align 8
  store %class.hKey* %k0, %class.hKey** %2, align 8
  store %class.hKey* %v0, %class.hKey** %3, align 8
  store i64 %h1, i64* %4, align 8
  store %class.hKey* %k1, %class.hKey** %5, align 8
  store %class.hKey* %v1, %class.hKey** %6, align 8
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 63
  %9 = urem i64 %8, 63
  %10 = trunc i64 %9 to i32
  store i32 %10, i32* %h0piece, align 4
  %11 = load i64, i64* %4, align 8
  %12 = and i64 %11, 63
  %13 = urem i64 %12, 63
  %14 = trunc i64 %13 to i32
  store i32 %14, i32* %h1piece, align 4
  %15 = load i32, i32* %h0piece, align 4
  %16 = load i32, i32* %h1piece, align 4
  %17 = icmp eq i32 %15, %16
  br i1 %17, label %18, label %39

; <label>:18                                      ; preds = %0
  %19 = load i64, i64* %1, align 8
  %20 = lshr i64 %19, 6
  %21 = load %class.hKey*, %class.hKey** %2, align 8
  %22 = load %class.hKey*, %class.hKey** %3, align 8
  %23 = load i64, i64* %4, align 8
  %24 = lshr i64 %23, 6
  %25 = load %class.hKey*, %class.hKey** %5, align 8
  %26 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj6EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.5* sret %childkv, i64 %20, %class.hKey* %21, %class.hKey* %22, i64 %24, %class.hKey* %25, %class.hKey* %26)
  %27 = call noalias i8* @malloc(i64 16) #8
  %28 = bitcast i8* %27 to %class.KV.5*
  store %class.KV.5* %28, %class.KV.5** %node, align 8
  %29 = load %class.KV.5*, %class.KV.5** %node, align 8
  %30 = getelementptr inbounds %class.KV.5, %class.KV.5* %29, i64 0
  %31 = bitcast %class.KV.5* %30 to i8*
  %32 = bitcast i8* %31 to %class.KV.5*
  call void @_ZN2KVI4hKeyS0_Lj6EEC2ERKS1_(%class.KV.5* %32, %class.KV.5* dereferenceable(16) %childkv)
  %33 = load i32, i32* %h0piece, align 4
  %34 = zext i32 %33 to i64
  %35 = shl i64 1, %34
  %36 = shl i64 %35, 1
  %37 = or i64 %36, 1
  %38 = load %class.KV.5*, %class.KV.5** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE(%class.KV.4* %agg.result, i64 %37, %class.KV.5* %38)
  br label %82

; <label>:39                                      ; preds = %0
  %40 = call noalias i8* @malloc(i64 32) #8
  %41 = bitcast i8* %40 to %class.KV.5*
  store %class.KV.5* %41, %class.KV.5** %node1, align 8
  %42 = load i32, i32* %h1piece, align 4
  %43 = load i32, i32* %h0piece, align 4
  %44 = icmp ult i32 %42, %43
  br i1 %44, label %45, label %58

; <label>:45                                      ; preds = %39
  %46 = load %class.KV.5*, %class.KV.5** %node1, align 8
  %47 = getelementptr inbounds %class.KV.5, %class.KV.5* %46, i64 0
  %48 = bitcast %class.KV.5* %47 to i8*
  %49 = bitcast i8* %48 to %class.KV.5*
  %50 = load %class.hKey*, %class.hKey** %5, align 8
  %51 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj6EEC2EPKS0_S3_(%class.KV.5* %49, %class.hKey* %50, %class.hKey* %51)
  %52 = load %class.KV.5*, %class.KV.5** %node1, align 8
  %53 = getelementptr inbounds %class.KV.5, %class.KV.5* %52, i64 1
  %54 = bitcast %class.KV.5* %53 to i8*
  %55 = bitcast i8* %54 to %class.KV.5*
  %56 = load %class.hKey*, %class.hKey** %2, align 8
  %57 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj6EEC2EPKS0_S3_(%class.KV.5* %55, %class.hKey* %56, %class.hKey* %57)
  br label %71

; <label>:58                                      ; preds = %39
  %59 = load %class.KV.5*, %class.KV.5** %node1, align 8
  %60 = getelementptr inbounds %class.KV.5, %class.KV.5* %59, i64 0
  %61 = bitcast %class.KV.5* %60 to i8*
  %62 = bitcast i8* %61 to %class.KV.5*
  %63 = load %class.hKey*, %class.hKey** %2, align 8
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj6EEC2EPKS0_S3_(%class.KV.5* %62, %class.hKey* %63, %class.hKey* %64)
  %65 = load %class.KV.5*, %class.KV.5** %node1, align 8
  %66 = getelementptr inbounds %class.KV.5, %class.KV.5* %65, i64 1
  %67 = bitcast %class.KV.5* %66 to i8*
  %68 = bitcast i8* %67 to %class.KV.5*
  %69 = load %class.hKey*, %class.hKey** %5, align 8
  %70 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj6EEC2EPKS0_S3_(%class.KV.5* %68, %class.hKey* %69, %class.hKey* %70)
  br label %71

; <label>:71                                      ; preds = %58, %45
  %72 = load i32, i32* %h0piece, align 4
  %73 = zext i32 %72 to i64
  %74 = shl i64 1, %73
  %75 = load i32, i32* %h1piece, align 4
  %76 = zext i32 %75 to i64
  %77 = shl i64 1, %76
  %78 = or i64 %74, %77
  %79 = shl i64 %78, 1
  %80 = or i64 %79, 1
  %81 = load %class.KV.5*, %class.KV.5** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE(%class.KV.4* %agg.result, i64 %80, %class.KV.5* %81)
  br label %82

; <label>:82                                      ; preds = %71, %18
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj5EEC2ERKS1_(%class.KV.4* %this, %class.KV.4* dereferenceable(16) %o) unnamed_addr #0 comdat align 2 {
  %1 = alloca %class.KV.4*, align 8
  %2 = alloca %class.KV.4*, align 8
  store %class.KV.4* %this, %class.KV.4** %1, align 8
  store %class.KV.4* %o, %class.KV.4** %2, align 8
  %3 = load %class.KV.4*, %class.KV.4** %1, align 8
  %4 = getelementptr inbounds %class.KV.4, %class.KV.4* %3, i32 0, i32 0
  %5 = load %class.KV.4*, %class.KV.4** %2, align 8
  %6 = getelementptr inbounds %class.KV.4, %class.KV.4* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<hKey, hKey, 5>::Key"* %4 to i8*
  %8 = bitcast %"union.KV<hKey, hKey, 5>::Key"* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %7, i8* %8, i64 8, i32 8, i1 false)
  %9 = getelementptr inbounds %class.KV.4, %class.KV.4* %3, i32 0, i32 1
  %10 = load %class.KV.4*, %class.KV.4** %2, align 8
  %11 = getelementptr inbounds %class.KV.4, %class.KV.4* %10, i32 0, i32 1
  %12 = bitcast %"union.KV<hKey, hKey, 5>::Val"* %9 to i8*
  %13 = bitcast %"union.KV<hKey, hKey, 5>::Val"* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %13, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3* %this, i64 %bm, %class.KV.4* %kv) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.3*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.KV.4*, align 8
  store %class.KV.3* %this, %class.KV.3** %1, align 8
  store i64 %bm, i64* %2, align 8
  store %class.KV.4* %kv, %class.KV.4** %3, align 8
  %4 = load %class.KV.3*, %class.KV.3** %1, align 8
  %5 = getelementptr inbounds %class.KV.3, %class.KV.3* %4, i32 0, i32 0
  %6 = load i64, i64* %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EE3KeyC2Em(%"union.KV<hKey, hKey, 4>::Key"* %5, i64 %6)
  %7 = getelementptr inbounds %class.KV.3, %class.KV.3* %4, i32 0, i32 1
  %8 = load %class.KV.4*, %class.KV.4** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EE3ValC2EPKS_IS0_S0_Lj5EE(%"union.KV<hKey, hKey, 4>::Val"* %7, %class.KV.4* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj5EEC2EPKS0_S3_(%class.KV.4* %this, %class.hKey* %key, %class.hKey* %val) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.4*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  store %class.KV.4* %this, %class.KV.4** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  store %class.hKey* %val, %class.hKey** %3, align 8
  %4 = load %class.KV.4*, %class.KV.4** %1, align 8
  %5 = getelementptr inbounds %class.KV.4, %class.KV.4* %4, i32 0, i32 0
  %6 = load %class.hKey*, %class.hKey** %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj5EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 5>::Key"* %5, %class.hKey* %6)
  %7 = getelementptr inbounds %class.KV.4, %class.KV.4* %4, i32 0, i32 1
  %8 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj5EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 5>::Val"* %7, %class.hKey* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj6EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.5* noalias sret %agg.result, i64 %h0, %class.hKey* %k0, %class.hKey* %v0, i64 %h1, %class.hKey* %k1, %class.hKey* %v1) #2 comdat align 2 {
  %1 = alloca i64, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hKey*, align 8
  %6 = alloca %class.hKey*, align 8
  %h0piece = alloca i32, align 4
  %h1piece = alloca i32, align 4
  %childkv = alloca %class.KV.6, align 8
  %node = alloca %class.KV.6*, align 8
  %node1 = alloca %class.KV.6*, align 8
  store i64 %h0, i64* %1, align 8
  store %class.hKey* %k0, %class.hKey** %2, align 8
  store %class.hKey* %v0, %class.hKey** %3, align 8
  store i64 %h1, i64* %4, align 8
  store %class.hKey* %k1, %class.hKey** %5, align 8
  store %class.hKey* %v1, %class.hKey** %6, align 8
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 63
  %9 = urem i64 %8, 63
  %10 = trunc i64 %9 to i32
  store i32 %10, i32* %h0piece, align 4
  %11 = load i64, i64* %4, align 8
  %12 = and i64 %11, 63
  %13 = urem i64 %12, 63
  %14 = trunc i64 %13 to i32
  store i32 %14, i32* %h1piece, align 4
  %15 = load i32, i32* %h0piece, align 4
  %16 = load i32, i32* %h1piece, align 4
  %17 = icmp eq i32 %15, %16
  br i1 %17, label %18, label %39

; <label>:18                                      ; preds = %0
  %19 = load i64, i64* %1, align 8
  %20 = lshr i64 %19, 6
  %21 = load %class.hKey*, %class.hKey** %2, align 8
  %22 = load %class.hKey*, %class.hKey** %3, align 8
  %23 = load i64, i64* %4, align 8
  %24 = lshr i64 %23, 6
  %25 = load %class.hKey*, %class.hKey** %5, align 8
  %26 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj7EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.6* sret %childkv, i64 %20, %class.hKey* %21, %class.hKey* %22, i64 %24, %class.hKey* %25, %class.hKey* %26)
  %27 = call noalias i8* @malloc(i64 16) #8
  %28 = bitcast i8* %27 to %class.KV.6*
  store %class.KV.6* %28, %class.KV.6** %node, align 8
  %29 = load %class.KV.6*, %class.KV.6** %node, align 8
  %30 = getelementptr inbounds %class.KV.6, %class.KV.6* %29, i64 0
  %31 = bitcast %class.KV.6* %30 to i8*
  %32 = bitcast i8* %31 to %class.KV.6*
  call void @_ZN2KVI4hKeyS0_Lj7EEC2ERKS1_(%class.KV.6* %32, %class.KV.6* dereferenceable(16) %childkv)
  %33 = load i32, i32* %h0piece, align 4
  %34 = zext i32 %33 to i64
  %35 = shl i64 1, %34
  %36 = shl i64 %35, 1
  %37 = or i64 %36, 1
  %38 = load %class.KV.6*, %class.KV.6** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE(%class.KV.5* %agg.result, i64 %37, %class.KV.6* %38)
  br label %82

; <label>:39                                      ; preds = %0
  %40 = call noalias i8* @malloc(i64 32) #8
  %41 = bitcast i8* %40 to %class.KV.6*
  store %class.KV.6* %41, %class.KV.6** %node1, align 8
  %42 = load i32, i32* %h1piece, align 4
  %43 = load i32, i32* %h0piece, align 4
  %44 = icmp ult i32 %42, %43
  br i1 %44, label %45, label %58

; <label>:45                                      ; preds = %39
  %46 = load %class.KV.6*, %class.KV.6** %node1, align 8
  %47 = getelementptr inbounds %class.KV.6, %class.KV.6* %46, i64 0
  %48 = bitcast %class.KV.6* %47 to i8*
  %49 = bitcast i8* %48 to %class.KV.6*
  %50 = load %class.hKey*, %class.hKey** %5, align 8
  %51 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj7EEC2EPKS0_S3_(%class.KV.6* %49, %class.hKey* %50, %class.hKey* %51)
  %52 = load %class.KV.6*, %class.KV.6** %node1, align 8
  %53 = getelementptr inbounds %class.KV.6, %class.KV.6* %52, i64 1
  %54 = bitcast %class.KV.6* %53 to i8*
  %55 = bitcast i8* %54 to %class.KV.6*
  %56 = load %class.hKey*, %class.hKey** %2, align 8
  %57 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj7EEC2EPKS0_S3_(%class.KV.6* %55, %class.hKey* %56, %class.hKey* %57)
  br label %71

; <label>:58                                      ; preds = %39
  %59 = load %class.KV.6*, %class.KV.6** %node1, align 8
  %60 = getelementptr inbounds %class.KV.6, %class.KV.6* %59, i64 0
  %61 = bitcast %class.KV.6* %60 to i8*
  %62 = bitcast i8* %61 to %class.KV.6*
  %63 = load %class.hKey*, %class.hKey** %2, align 8
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj7EEC2EPKS0_S3_(%class.KV.6* %62, %class.hKey* %63, %class.hKey* %64)
  %65 = load %class.KV.6*, %class.KV.6** %node1, align 8
  %66 = getelementptr inbounds %class.KV.6, %class.KV.6* %65, i64 1
  %67 = bitcast %class.KV.6* %66 to i8*
  %68 = bitcast i8* %67 to %class.KV.6*
  %69 = load %class.hKey*, %class.hKey** %5, align 8
  %70 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj7EEC2EPKS0_S3_(%class.KV.6* %68, %class.hKey* %69, %class.hKey* %70)
  br label %71

; <label>:71                                      ; preds = %58, %45
  %72 = load i32, i32* %h0piece, align 4
  %73 = zext i32 %72 to i64
  %74 = shl i64 1, %73
  %75 = load i32, i32* %h1piece, align 4
  %76 = zext i32 %75 to i64
  %77 = shl i64 1, %76
  %78 = or i64 %74, %77
  %79 = shl i64 %78, 1
  %80 = or i64 %79, 1
  %81 = load %class.KV.6*, %class.KV.6** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE(%class.KV.5* %agg.result, i64 %80, %class.KV.6* %81)
  br label %82

; <label>:82                                      ; preds = %71, %18
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj6EEC2ERKS1_(%class.KV.5* %this, %class.KV.5* dereferenceable(16) %o) unnamed_addr #0 comdat align 2 {
  %1 = alloca %class.KV.5*, align 8
  %2 = alloca %class.KV.5*, align 8
  store %class.KV.5* %this, %class.KV.5** %1, align 8
  store %class.KV.5* %o, %class.KV.5** %2, align 8
  %3 = load %class.KV.5*, %class.KV.5** %1, align 8
  %4 = getelementptr inbounds %class.KV.5, %class.KV.5* %3, i32 0, i32 0
  %5 = load %class.KV.5*, %class.KV.5** %2, align 8
  %6 = getelementptr inbounds %class.KV.5, %class.KV.5* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<hKey, hKey, 6>::Key"* %4 to i8*
  %8 = bitcast %"union.KV<hKey, hKey, 6>::Key"* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %7, i8* %8, i64 8, i32 8, i1 false)
  %9 = getelementptr inbounds %class.KV.5, %class.KV.5* %3, i32 0, i32 1
  %10 = load %class.KV.5*, %class.KV.5** %2, align 8
  %11 = getelementptr inbounds %class.KV.5, %class.KV.5* %10, i32 0, i32 1
  %12 = bitcast %"union.KV<hKey, hKey, 6>::Val"* %9 to i8*
  %13 = bitcast %"union.KV<hKey, hKey, 6>::Val"* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %13, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE(%class.KV.4* %this, i64 %bm, %class.KV.5* %kv) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.4*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.KV.5*, align 8
  store %class.KV.4* %this, %class.KV.4** %1, align 8
  store i64 %bm, i64* %2, align 8
  store %class.KV.5* %kv, %class.KV.5** %3, align 8
  %4 = load %class.KV.4*, %class.KV.4** %1, align 8
  %5 = getelementptr inbounds %class.KV.4, %class.KV.4* %4, i32 0, i32 0
  %6 = load i64, i64* %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj5EE3KeyC2Em(%"union.KV<hKey, hKey, 5>::Key"* %5, i64 %6)
  %7 = getelementptr inbounds %class.KV.4, %class.KV.4* %4, i32 0, i32 1
  %8 = load %class.KV.5*, %class.KV.5** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj5EE3ValC2EPKS_IS0_S0_Lj6EE(%"union.KV<hKey, hKey, 5>::Val"* %7, %class.KV.5* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj6EEC2EPKS0_S3_(%class.KV.5* %this, %class.hKey* %key, %class.hKey* %val) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.5*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  store %class.KV.5* %this, %class.KV.5** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  store %class.hKey* %val, %class.hKey** %3, align 8
  %4 = load %class.KV.5*, %class.KV.5** %1, align 8
  %5 = getelementptr inbounds %class.KV.5, %class.KV.5* %4, i32 0, i32 0
  %6 = load %class.hKey*, %class.hKey** %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj6EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 6>::Key"* %5, %class.hKey* %6)
  %7 = getelementptr inbounds %class.KV.5, %class.KV.5* %4, i32 0, i32 1
  %8 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj6EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 6>::Val"* %7, %class.hKey* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj7EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.6* noalias sret %agg.result, i64 %h0, %class.hKey* %k0, %class.hKey* %v0, i64 %h1, %class.hKey* %k1, %class.hKey* %v1) #2 comdat align 2 {
  %1 = alloca i64, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hKey*, align 8
  %6 = alloca %class.hKey*, align 8
  %h0piece = alloca i32, align 4
  %h1piece = alloca i32, align 4
  %childkv = alloca %class.KV.7, align 8
  %node = alloca %class.KV.7*, align 8
  %node1 = alloca %class.KV.7*, align 8
  store i64 %h0, i64* %1, align 8
  store %class.hKey* %k0, %class.hKey** %2, align 8
  store %class.hKey* %v0, %class.hKey** %3, align 8
  store i64 %h1, i64* %4, align 8
  store %class.hKey* %k1, %class.hKey** %5, align 8
  store %class.hKey* %v1, %class.hKey** %6, align 8
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 63
  %9 = urem i64 %8, 63
  %10 = trunc i64 %9 to i32
  store i32 %10, i32* %h0piece, align 4
  %11 = load i64, i64* %4, align 8
  %12 = and i64 %11, 63
  %13 = urem i64 %12, 63
  %14 = trunc i64 %13 to i32
  store i32 %14, i32* %h1piece, align 4
  %15 = load i32, i32* %h0piece, align 4
  %16 = load i32, i32* %h1piece, align 4
  %17 = icmp eq i32 %15, %16
  br i1 %17, label %18, label %39

; <label>:18                                      ; preds = %0
  %19 = load i64, i64* %1, align 8
  %20 = lshr i64 %19, 6
  %21 = load %class.hKey*, %class.hKey** %2, align 8
  %22 = load %class.hKey*, %class.hKey** %3, align 8
  %23 = load i64, i64* %4, align 8
  %24 = lshr i64 %23, 6
  %25 = load %class.hKey*, %class.hKey** %5, align 8
  %26 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj8EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.7* sret %childkv, i64 %20, %class.hKey* %21, %class.hKey* %22, i64 %24, %class.hKey* %25, %class.hKey* %26)
  %27 = call noalias i8* @malloc(i64 16) #8
  %28 = bitcast i8* %27 to %class.KV.7*
  store %class.KV.7* %28, %class.KV.7** %node, align 8
  %29 = load %class.KV.7*, %class.KV.7** %node, align 8
  %30 = getelementptr inbounds %class.KV.7, %class.KV.7* %29, i64 0
  %31 = bitcast %class.KV.7* %30 to i8*
  %32 = bitcast i8* %31 to %class.KV.7*
  call void @_ZN2KVI4hKeyS0_Lj8EEC2ERKS1_(%class.KV.7* %32, %class.KV.7* dereferenceable(16) %childkv)
  %33 = load i32, i32* %h0piece, align 4
  %34 = zext i32 %33 to i64
  %35 = shl i64 1, %34
  %36 = shl i64 %35, 1
  %37 = or i64 %36, 1
  %38 = load %class.KV.7*, %class.KV.7** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE(%class.KV.6* %agg.result, i64 %37, %class.KV.7* %38)
  br label %82

; <label>:39                                      ; preds = %0
  %40 = call noalias i8* @malloc(i64 32) #8
  %41 = bitcast i8* %40 to %class.KV.7*
  store %class.KV.7* %41, %class.KV.7** %node1, align 8
  %42 = load i32, i32* %h1piece, align 4
  %43 = load i32, i32* %h0piece, align 4
  %44 = icmp ult i32 %42, %43
  br i1 %44, label %45, label %58

; <label>:45                                      ; preds = %39
  %46 = load %class.KV.7*, %class.KV.7** %node1, align 8
  %47 = getelementptr inbounds %class.KV.7, %class.KV.7* %46, i64 0
  %48 = bitcast %class.KV.7* %47 to i8*
  %49 = bitcast i8* %48 to %class.KV.7*
  %50 = load %class.hKey*, %class.hKey** %5, align 8
  %51 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj8EEC2EPKS0_S3_(%class.KV.7* %49, %class.hKey* %50, %class.hKey* %51)
  %52 = load %class.KV.7*, %class.KV.7** %node1, align 8
  %53 = getelementptr inbounds %class.KV.7, %class.KV.7* %52, i64 1
  %54 = bitcast %class.KV.7* %53 to i8*
  %55 = bitcast i8* %54 to %class.KV.7*
  %56 = load %class.hKey*, %class.hKey** %2, align 8
  %57 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj8EEC2EPKS0_S3_(%class.KV.7* %55, %class.hKey* %56, %class.hKey* %57)
  br label %71

; <label>:58                                      ; preds = %39
  %59 = load %class.KV.7*, %class.KV.7** %node1, align 8
  %60 = getelementptr inbounds %class.KV.7, %class.KV.7* %59, i64 0
  %61 = bitcast %class.KV.7* %60 to i8*
  %62 = bitcast i8* %61 to %class.KV.7*
  %63 = load %class.hKey*, %class.hKey** %2, align 8
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj8EEC2EPKS0_S3_(%class.KV.7* %62, %class.hKey* %63, %class.hKey* %64)
  %65 = load %class.KV.7*, %class.KV.7** %node1, align 8
  %66 = getelementptr inbounds %class.KV.7, %class.KV.7* %65, i64 1
  %67 = bitcast %class.KV.7* %66 to i8*
  %68 = bitcast i8* %67 to %class.KV.7*
  %69 = load %class.hKey*, %class.hKey** %5, align 8
  %70 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj8EEC2EPKS0_S3_(%class.KV.7* %68, %class.hKey* %69, %class.hKey* %70)
  br label %71

; <label>:71                                      ; preds = %58, %45
  %72 = load i32, i32* %h0piece, align 4
  %73 = zext i32 %72 to i64
  %74 = shl i64 1, %73
  %75 = load i32, i32* %h1piece, align 4
  %76 = zext i32 %75 to i64
  %77 = shl i64 1, %76
  %78 = or i64 %74, %77
  %79 = shl i64 %78, 1
  %80 = or i64 %79, 1
  %81 = load %class.KV.7*, %class.KV.7** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE(%class.KV.6* %agg.result, i64 %80, %class.KV.7* %81)
  br label %82

; <label>:82                                      ; preds = %71, %18
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj7EEC2ERKS1_(%class.KV.6* %this, %class.KV.6* dereferenceable(16) %o) unnamed_addr #0 comdat align 2 {
  %1 = alloca %class.KV.6*, align 8
  %2 = alloca %class.KV.6*, align 8
  store %class.KV.6* %this, %class.KV.6** %1, align 8
  store %class.KV.6* %o, %class.KV.6** %2, align 8
  %3 = load %class.KV.6*, %class.KV.6** %1, align 8
  %4 = getelementptr inbounds %class.KV.6, %class.KV.6* %3, i32 0, i32 0
  %5 = load %class.KV.6*, %class.KV.6** %2, align 8
  %6 = getelementptr inbounds %class.KV.6, %class.KV.6* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<hKey, hKey, 7>::Key"* %4 to i8*
  %8 = bitcast %"union.KV<hKey, hKey, 7>::Key"* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %7, i8* %8, i64 8, i32 8, i1 false)
  %9 = getelementptr inbounds %class.KV.6, %class.KV.6* %3, i32 0, i32 1
  %10 = load %class.KV.6*, %class.KV.6** %2, align 8
  %11 = getelementptr inbounds %class.KV.6, %class.KV.6* %10, i32 0, i32 1
  %12 = bitcast %"union.KV<hKey, hKey, 7>::Val"* %9 to i8*
  %13 = bitcast %"union.KV<hKey, hKey, 7>::Val"* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %13, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE(%class.KV.5* %this, i64 %bm, %class.KV.6* %kv) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.5*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.KV.6*, align 8
  store %class.KV.5* %this, %class.KV.5** %1, align 8
  store i64 %bm, i64* %2, align 8
  store %class.KV.6* %kv, %class.KV.6** %3, align 8
  %4 = load %class.KV.5*, %class.KV.5** %1, align 8
  %5 = getelementptr inbounds %class.KV.5, %class.KV.5* %4, i32 0, i32 0
  %6 = load i64, i64* %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj6EE3KeyC2Em(%"union.KV<hKey, hKey, 6>::Key"* %5, i64 %6)
  %7 = getelementptr inbounds %class.KV.5, %class.KV.5* %4, i32 0, i32 1
  %8 = load %class.KV.6*, %class.KV.6** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj6EE3ValC2EPKS_IS0_S0_Lj7EE(%"union.KV<hKey, hKey, 6>::Val"* %7, %class.KV.6* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj7EEC2EPKS0_S3_(%class.KV.6* %this, %class.hKey* %key, %class.hKey* %val) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.6*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  store %class.KV.6* %this, %class.KV.6** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  store %class.hKey* %val, %class.hKey** %3, align 8
  %4 = load %class.KV.6*, %class.KV.6** %1, align 8
  %5 = getelementptr inbounds %class.KV.6, %class.KV.6* %4, i32 0, i32 0
  %6 = load %class.hKey*, %class.hKey** %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj7EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 7>::Key"* %5, %class.hKey* %6)
  %7 = getelementptr inbounds %class.KV.6, %class.KV.6* %4, i32 0, i32 1
  %8 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj7EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 7>::Val"* %7, %class.hKey* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj8EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.7* noalias sret %agg.result, i64 %h0, %class.hKey* %k0, %class.hKey* %v0, i64 %h1, %class.hKey* %k1, %class.hKey* %v1) #2 comdat align 2 {
  %1 = alloca i64, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hKey*, align 8
  %6 = alloca %class.hKey*, align 8
  %h0piece = alloca i32, align 4
  %h1piece = alloca i32, align 4
  %childkv = alloca %class.KV.8, align 8
  %node = alloca %class.KV.8*, align 8
  %node1 = alloca %class.KV.8*, align 8
  store i64 %h0, i64* %1, align 8
  store %class.hKey* %k0, %class.hKey** %2, align 8
  store %class.hKey* %v0, %class.hKey** %3, align 8
  store i64 %h1, i64* %4, align 8
  store %class.hKey* %k1, %class.hKey** %5, align 8
  store %class.hKey* %v1, %class.hKey** %6, align 8
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 63
  %9 = urem i64 %8, 63
  %10 = trunc i64 %9 to i32
  store i32 %10, i32* %h0piece, align 4
  %11 = load i64, i64* %4, align 8
  %12 = and i64 %11, 63
  %13 = urem i64 %12, 63
  %14 = trunc i64 %13 to i32
  store i32 %14, i32* %h1piece, align 4
  %15 = load i32, i32* %h0piece, align 4
  %16 = load i32, i32* %h1piece, align 4
  %17 = icmp eq i32 %15, %16
  br i1 %17, label %18, label %39

; <label>:18                                      ; preds = %0
  %19 = load i64, i64* %1, align 8
  %20 = lshr i64 %19, 6
  %21 = load %class.hKey*, %class.hKey** %2, align 8
  %22 = load %class.hKey*, %class.hKey** %3, align 8
  %23 = load i64, i64* %4, align 8
  %24 = lshr i64 %23, 6
  %25 = load %class.hKey*, %class.hKey** %5, align 8
  %26 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj9EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.8* sret %childkv, i64 %20, %class.hKey* %21, %class.hKey* %22, i64 %24, %class.hKey* %25, %class.hKey* %26)
  %27 = call noalias i8* @malloc(i64 16) #8
  %28 = bitcast i8* %27 to %class.KV.8*
  store %class.KV.8* %28, %class.KV.8** %node, align 8
  %29 = load %class.KV.8*, %class.KV.8** %node, align 8
  %30 = getelementptr inbounds %class.KV.8, %class.KV.8* %29, i64 0
  %31 = bitcast %class.KV.8* %30 to i8*
  %32 = bitcast i8* %31 to %class.KV.8*
  call void @_ZN2KVI4hKeyS0_Lj9EEC2ERKS1_(%class.KV.8* %32, %class.KV.8* dereferenceable(16) %childkv)
  %33 = load i32, i32* %h0piece, align 4
  %34 = zext i32 %33 to i64
  %35 = shl i64 1, %34
  %36 = shl i64 %35, 1
  %37 = or i64 %36, 1
  %38 = load %class.KV.8*, %class.KV.8** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE(%class.KV.7* %agg.result, i64 %37, %class.KV.8* %38)
  br label %82

; <label>:39                                      ; preds = %0
  %40 = call noalias i8* @malloc(i64 32) #8
  %41 = bitcast i8* %40 to %class.KV.8*
  store %class.KV.8* %41, %class.KV.8** %node1, align 8
  %42 = load i32, i32* %h1piece, align 4
  %43 = load i32, i32* %h0piece, align 4
  %44 = icmp ult i32 %42, %43
  br i1 %44, label %45, label %58

; <label>:45                                      ; preds = %39
  %46 = load %class.KV.8*, %class.KV.8** %node1, align 8
  %47 = getelementptr inbounds %class.KV.8, %class.KV.8* %46, i64 0
  %48 = bitcast %class.KV.8* %47 to i8*
  %49 = bitcast i8* %48 to %class.KV.8*
  %50 = load %class.hKey*, %class.hKey** %5, align 8
  %51 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj9EEC2EPKS0_S3_(%class.KV.8* %49, %class.hKey* %50, %class.hKey* %51)
  %52 = load %class.KV.8*, %class.KV.8** %node1, align 8
  %53 = getelementptr inbounds %class.KV.8, %class.KV.8* %52, i64 1
  %54 = bitcast %class.KV.8* %53 to i8*
  %55 = bitcast i8* %54 to %class.KV.8*
  %56 = load %class.hKey*, %class.hKey** %2, align 8
  %57 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj9EEC2EPKS0_S3_(%class.KV.8* %55, %class.hKey* %56, %class.hKey* %57)
  br label %71

; <label>:58                                      ; preds = %39
  %59 = load %class.KV.8*, %class.KV.8** %node1, align 8
  %60 = getelementptr inbounds %class.KV.8, %class.KV.8* %59, i64 0
  %61 = bitcast %class.KV.8* %60 to i8*
  %62 = bitcast i8* %61 to %class.KV.8*
  %63 = load %class.hKey*, %class.hKey** %2, align 8
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj9EEC2EPKS0_S3_(%class.KV.8* %62, %class.hKey* %63, %class.hKey* %64)
  %65 = load %class.KV.8*, %class.KV.8** %node1, align 8
  %66 = getelementptr inbounds %class.KV.8, %class.KV.8* %65, i64 1
  %67 = bitcast %class.KV.8* %66 to i8*
  %68 = bitcast i8* %67 to %class.KV.8*
  %69 = load %class.hKey*, %class.hKey** %5, align 8
  %70 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj9EEC2EPKS0_S3_(%class.KV.8* %68, %class.hKey* %69, %class.hKey* %70)
  br label %71

; <label>:71                                      ; preds = %58, %45
  %72 = load i32, i32* %h0piece, align 4
  %73 = zext i32 %72 to i64
  %74 = shl i64 1, %73
  %75 = load i32, i32* %h1piece, align 4
  %76 = zext i32 %75 to i64
  %77 = shl i64 1, %76
  %78 = or i64 %74, %77
  %79 = shl i64 %78, 1
  %80 = or i64 %79, 1
  %81 = load %class.KV.8*, %class.KV.8** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE(%class.KV.7* %agg.result, i64 %80, %class.KV.8* %81)
  br label %82

; <label>:82                                      ; preds = %71, %18
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj8EEC2ERKS1_(%class.KV.7* %this, %class.KV.7* dereferenceable(16) %o) unnamed_addr #0 comdat align 2 {
  %1 = alloca %class.KV.7*, align 8
  %2 = alloca %class.KV.7*, align 8
  store %class.KV.7* %this, %class.KV.7** %1, align 8
  store %class.KV.7* %o, %class.KV.7** %2, align 8
  %3 = load %class.KV.7*, %class.KV.7** %1, align 8
  %4 = getelementptr inbounds %class.KV.7, %class.KV.7* %3, i32 0, i32 0
  %5 = load %class.KV.7*, %class.KV.7** %2, align 8
  %6 = getelementptr inbounds %class.KV.7, %class.KV.7* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<hKey, hKey, 8>::Key"* %4 to i8*
  %8 = bitcast %"union.KV<hKey, hKey, 8>::Key"* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %7, i8* %8, i64 8, i32 8, i1 false)
  %9 = getelementptr inbounds %class.KV.7, %class.KV.7* %3, i32 0, i32 1
  %10 = load %class.KV.7*, %class.KV.7** %2, align 8
  %11 = getelementptr inbounds %class.KV.7, %class.KV.7* %10, i32 0, i32 1
  %12 = bitcast %"union.KV<hKey, hKey, 8>::Val"* %9 to i8*
  %13 = bitcast %"union.KV<hKey, hKey, 8>::Val"* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %13, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE(%class.KV.6* %this, i64 %bm, %class.KV.7* %kv) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.6*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.KV.7*, align 8
  store %class.KV.6* %this, %class.KV.6** %1, align 8
  store i64 %bm, i64* %2, align 8
  store %class.KV.7* %kv, %class.KV.7** %3, align 8
  %4 = load %class.KV.6*, %class.KV.6** %1, align 8
  %5 = getelementptr inbounds %class.KV.6, %class.KV.6* %4, i32 0, i32 0
  %6 = load i64, i64* %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj7EE3KeyC2Em(%"union.KV<hKey, hKey, 7>::Key"* %5, i64 %6)
  %7 = getelementptr inbounds %class.KV.6, %class.KV.6* %4, i32 0, i32 1
  %8 = load %class.KV.7*, %class.KV.7** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj7EE3ValC2EPKS_IS0_S0_Lj8EE(%"union.KV<hKey, hKey, 7>::Val"* %7, %class.KV.7* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj8EEC2EPKS0_S3_(%class.KV.7* %this, %class.hKey* %key, %class.hKey* %val) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.7*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  store %class.KV.7* %this, %class.KV.7** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  store %class.hKey* %val, %class.hKey** %3, align 8
  %4 = load %class.KV.7*, %class.KV.7** %1, align 8
  %5 = getelementptr inbounds %class.KV.7, %class.KV.7* %4, i32 0, i32 0
  %6 = load %class.hKey*, %class.hKey** %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj8EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 8>::Key"* %5, %class.hKey* %6)
  %7 = getelementptr inbounds %class.KV.7, %class.KV.7* %4, i32 0, i32 1
  %8 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj8EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 8>::Val"* %7, %class.hKey* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj9EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.8* noalias sret %agg.result, i64 %h0, %class.hKey* %k0, %class.hKey* %v0, i64 %h1, %class.hKey* %k1, %class.hKey* %v1) #2 comdat align 2 {
  %1 = alloca i64, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hKey*, align 8
  %6 = alloca %class.hKey*, align 8
  %h0piece = alloca i32, align 4
  %h1piece = alloca i32, align 4
  %childkv = alloca %class.KV.9, align 8
  %node = alloca %class.KV.9*, align 8
  %node1 = alloca %class.KV.9*, align 8
  store i64 %h0, i64* %1, align 8
  store %class.hKey* %k0, %class.hKey** %2, align 8
  store %class.hKey* %v0, %class.hKey** %3, align 8
  store i64 %h1, i64* %4, align 8
  store %class.hKey* %k1, %class.hKey** %5, align 8
  store %class.hKey* %v1, %class.hKey** %6, align 8
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 63
  %9 = urem i64 %8, 63
  %10 = trunc i64 %9 to i32
  store i32 %10, i32* %h0piece, align 4
  %11 = load i64, i64* %4, align 8
  %12 = and i64 %11, 63
  %13 = urem i64 %12, 63
  %14 = trunc i64 %13 to i32
  store i32 %14, i32* %h1piece, align 4
  %15 = load i32, i32* %h0piece, align 4
  %16 = load i32, i32* %h1piece, align 4
  %17 = icmp eq i32 %15, %16
  br i1 %17, label %18, label %39

; <label>:18                                      ; preds = %0
  %19 = load i64, i64* %1, align 8
  %20 = lshr i64 %19, 6
  %21 = load %class.hKey*, %class.hKey** %2, align 8
  %22 = load %class.hKey*, %class.hKey** %3, align 8
  %23 = load i64, i64* %4, align 8
  %24 = lshr i64 %23, 6
  %25 = load %class.hKey*, %class.hKey** %5, align 8
  %26 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj10EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.9* sret %childkv, i64 %20, %class.hKey* %21, %class.hKey* %22, i64 %24, %class.hKey* %25, %class.hKey* %26)
  %27 = call noalias i8* @malloc(i64 16) #8
  %28 = bitcast i8* %27 to %class.KV.9*
  store %class.KV.9* %28, %class.KV.9** %node, align 8
  %29 = load %class.KV.9*, %class.KV.9** %node, align 8
  %30 = getelementptr inbounds %class.KV.9, %class.KV.9* %29, i64 0
  %31 = bitcast %class.KV.9* %30 to i8*
  %32 = bitcast i8* %31 to %class.KV.9*
  call void @_ZN2KVI4hKeyS0_Lj10EEC2ERKS1_(%class.KV.9* %32, %class.KV.9* dereferenceable(16) %childkv)
  %33 = load i32, i32* %h0piece, align 4
  %34 = zext i32 %33 to i64
  %35 = shl i64 1, %34
  %36 = shl i64 %35, 1
  %37 = or i64 %36, 1
  %38 = load %class.KV.9*, %class.KV.9** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE(%class.KV.8* %agg.result, i64 %37, %class.KV.9* %38)
  br label %82

; <label>:39                                      ; preds = %0
  %40 = call noalias i8* @malloc(i64 32) #8
  %41 = bitcast i8* %40 to %class.KV.9*
  store %class.KV.9* %41, %class.KV.9** %node1, align 8
  %42 = load i32, i32* %h1piece, align 4
  %43 = load i32, i32* %h0piece, align 4
  %44 = icmp ult i32 %42, %43
  br i1 %44, label %45, label %58

; <label>:45                                      ; preds = %39
  %46 = load %class.KV.9*, %class.KV.9** %node1, align 8
  %47 = getelementptr inbounds %class.KV.9, %class.KV.9* %46, i64 0
  %48 = bitcast %class.KV.9* %47 to i8*
  %49 = bitcast i8* %48 to %class.KV.9*
  %50 = load %class.hKey*, %class.hKey** %5, align 8
  %51 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj10EEC2EPKS0_S3_(%class.KV.9* %49, %class.hKey* %50, %class.hKey* %51)
  %52 = load %class.KV.9*, %class.KV.9** %node1, align 8
  %53 = getelementptr inbounds %class.KV.9, %class.KV.9* %52, i64 1
  %54 = bitcast %class.KV.9* %53 to i8*
  %55 = bitcast i8* %54 to %class.KV.9*
  %56 = load %class.hKey*, %class.hKey** %2, align 8
  %57 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj10EEC2EPKS0_S3_(%class.KV.9* %55, %class.hKey* %56, %class.hKey* %57)
  br label %71

; <label>:58                                      ; preds = %39
  %59 = load %class.KV.9*, %class.KV.9** %node1, align 8
  %60 = getelementptr inbounds %class.KV.9, %class.KV.9* %59, i64 0
  %61 = bitcast %class.KV.9* %60 to i8*
  %62 = bitcast i8* %61 to %class.KV.9*
  %63 = load %class.hKey*, %class.hKey** %2, align 8
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj10EEC2EPKS0_S3_(%class.KV.9* %62, %class.hKey* %63, %class.hKey* %64)
  %65 = load %class.KV.9*, %class.KV.9** %node1, align 8
  %66 = getelementptr inbounds %class.KV.9, %class.KV.9* %65, i64 1
  %67 = bitcast %class.KV.9* %66 to i8*
  %68 = bitcast i8* %67 to %class.KV.9*
  %69 = load %class.hKey*, %class.hKey** %5, align 8
  %70 = load %class.hKey*, %class.hKey** %6, align 8
  call void @_ZN2KVI4hKeyS0_Lj10EEC2EPKS0_S3_(%class.KV.9* %68, %class.hKey* %69, %class.hKey* %70)
  br label %71

; <label>:71                                      ; preds = %58, %45
  %72 = load i32, i32* %h0piece, align 4
  %73 = zext i32 %72 to i64
  %74 = shl i64 1, %73
  %75 = load i32, i32* %h1piece, align 4
  %76 = zext i32 %75 to i64
  %77 = shl i64 1, %76
  %78 = or i64 %74, %77
  %79 = shl i64 %78, 1
  %80 = or i64 %79, 1
  %81 = load %class.KV.9*, %class.KV.9** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE(%class.KV.8* %agg.result, i64 %80, %class.KV.9* %81)
  br label %82

; <label>:82                                      ; preds = %71, %18
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj9EEC2ERKS1_(%class.KV.8* %this, %class.KV.8* dereferenceable(16) %o) unnamed_addr #0 comdat align 2 {
  %1 = alloca %class.KV.8*, align 8
  %2 = alloca %class.KV.8*, align 8
  store %class.KV.8* %this, %class.KV.8** %1, align 8
  store %class.KV.8* %o, %class.KV.8** %2, align 8
  %3 = load %class.KV.8*, %class.KV.8** %1, align 8
  %4 = getelementptr inbounds %class.KV.8, %class.KV.8* %3, i32 0, i32 0
  %5 = load %class.KV.8*, %class.KV.8** %2, align 8
  %6 = getelementptr inbounds %class.KV.8, %class.KV.8* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<hKey, hKey, 9>::Key"* %4 to i8*
  %8 = bitcast %"union.KV<hKey, hKey, 9>::Key"* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %7, i8* %8, i64 8, i32 8, i1 false)
  %9 = getelementptr inbounds %class.KV.8, %class.KV.8* %3, i32 0, i32 1
  %10 = load %class.KV.8*, %class.KV.8** %2, align 8
  %11 = getelementptr inbounds %class.KV.8, %class.KV.8* %10, i32 0, i32 1
  %12 = bitcast %"union.KV<hKey, hKey, 9>::Val"* %9 to i8*
  %13 = bitcast %"union.KV<hKey, hKey, 9>::Val"* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %13, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE(%class.KV.7* %this, i64 %bm, %class.KV.8* %kv) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.7*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.KV.8*, align 8
  store %class.KV.7* %this, %class.KV.7** %1, align 8
  store i64 %bm, i64* %2, align 8
  store %class.KV.8* %kv, %class.KV.8** %3, align 8
  %4 = load %class.KV.7*, %class.KV.7** %1, align 8
  %5 = getelementptr inbounds %class.KV.7, %class.KV.7* %4, i32 0, i32 0
  %6 = load i64, i64* %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj8EE3KeyC2Em(%"union.KV<hKey, hKey, 8>::Key"* %5, i64 %6)
  %7 = getelementptr inbounds %class.KV.7, %class.KV.7* %4, i32 0, i32 1
  %8 = load %class.KV.8*, %class.KV.8** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj8EE3ValC2EPKS_IS0_S0_Lj9EE(%"union.KV<hKey, hKey, 8>::Val"* %7, %class.KV.8* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj9EEC2EPKS0_S3_(%class.KV.8* %this, %class.hKey* %key, %class.hKey* %val) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.8*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  store %class.KV.8* %this, %class.KV.8** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  store %class.hKey* %val, %class.hKey** %3, align 8
  %4 = load %class.KV.8*, %class.KV.8** %1, align 8
  %5 = getelementptr inbounds %class.KV.8, %class.KV.8* %4, i32 0, i32 0
  %6 = load %class.hKey*, %class.hKey** %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj9EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 9>::Key"* %5, %class.hKey* %6)
  %7 = getelementptr inbounds %class.KV.8, %class.KV.8* %4, i32 0, i32 1
  %8 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj9EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 9>::Val"* %7, %class.hKey* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj10EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.9* noalias sret %agg.result, i64 %h0, %class.hKey* %k0, %class.hKey* %v0, i64 %h1, %class.hKey* %k1, %class.hKey* %v1) #2 comdat align 2 {
  %1 = alloca i64, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hKey*, align 8
  %6 = alloca %class.hKey*, align 8
  %ll1 = alloca %class.LL*, align 8
  %ll0 = alloca %class.LL*, align 8
  store i64 %h0, i64* %1, align 8
  store %class.hKey* %k0, %class.hKey** %2, align 8
  store %class.hKey* %v0, %class.hKey** %3, align 8
  store i64 %h1, i64* %4, align 8
  store %class.hKey* %k1, %class.hKey** %5, align 8
  store %class.hKey* %v1, %class.hKey** %6, align 8
  %7 = call noalias i8* @malloc(i64 24) #8
  %8 = bitcast i8* %7 to %class.LL*
  %9 = bitcast %class.LL* %8 to i8*
  %10 = bitcast i8* %9 to %class.LL*
  %11 = load %class.hKey*, %class.hKey** %2, align 8
  %12 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2LLI4hKeyS0_EC2EPKS0_S3_PKS1_(%class.LL* %10, %class.hKey* %11, %class.hKey* %12, %class.LL* null)
  store %class.LL* %10, %class.LL** %ll1, align 8
  %13 = call noalias i8* @malloc(i64 24) #8
  %14 = bitcast i8* %13 to %class.LL*
  %15 = bitcast %class.LL* %14 to i8*
  %16 = bitcast i8* %15 to %class.LL*
  %17 = load %class.hKey*, %class.hKey** %5, align 8
  %18 = load %class.hKey*, %class.hKey** %6, align 8
  %19 = load %class.LL*, %class.LL** %ll1, align 8
  call void @_ZN2LLI4hKeyS0_EC2EPKS0_S3_PKS1_(%class.LL* %16, %class.hKey* %17, %class.hKey* %18, %class.LL* %19)
  store %class.LL* %16, %class.LL** %ll0, align 8
  %20 = load %class.LL*, %class.LL** %ll0, align 8
  call void @_ZN2KVI4hKeyS0_Lj10EEC2EmPK2LLIS0_S0_E(%class.KV.9* %agg.result, i64 1, %class.LL* %20)
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj10EEC2ERKS1_(%class.KV.9* %this, %class.KV.9* dereferenceable(16) %o) unnamed_addr #0 comdat align 2 {
  %1 = alloca %class.KV.9*, align 8
  %2 = alloca %class.KV.9*, align 8
  store %class.KV.9* %this, %class.KV.9** %1, align 8
  store %class.KV.9* %o, %class.KV.9** %2, align 8
  %3 = load %class.KV.9*, %class.KV.9** %1, align 8
  %4 = getelementptr inbounds %class.KV.9, %class.KV.9* %3, i32 0, i32 0
  %5 = load %class.KV.9*, %class.KV.9** %2, align 8
  %6 = getelementptr inbounds %class.KV.9, %class.KV.9* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<hKey, hKey, 10>::Key"* %4 to i8*
  %8 = bitcast %"union.KV<hKey, hKey, 10>::Key"* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %7, i8* %8, i64 8, i32 8, i1 false)
  %9 = getelementptr inbounds %class.KV.9, %class.KV.9* %3, i32 0, i32 1
  %10 = load %class.KV.9*, %class.KV.9** %2, align 8
  %11 = getelementptr inbounds %class.KV.9, %class.KV.9* %10, i32 0, i32 1
  %12 = bitcast %"union.KV<hKey, hKey, 10>::Val"* %9 to i8*
  %13 = bitcast %"union.KV<hKey, hKey, 10>::Val"* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %13, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE(%class.KV.8* %this, i64 %bm, %class.KV.9* %kv) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.8*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.KV.9*, align 8
  store %class.KV.8* %this, %class.KV.8** %1, align 8
  store i64 %bm, i64* %2, align 8
  store %class.KV.9* %kv, %class.KV.9** %3, align 8
  %4 = load %class.KV.8*, %class.KV.8** %1, align 8
  %5 = getelementptr inbounds %class.KV.8, %class.KV.8* %4, i32 0, i32 0
  %6 = load i64, i64* %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj9EE3KeyC2Em(%"union.KV<hKey, hKey, 9>::Key"* %5, i64 %6)
  %7 = getelementptr inbounds %class.KV.8, %class.KV.8* %4, i32 0, i32 1
  %8 = load %class.KV.9*, %class.KV.9** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj9EE3ValC2EPKS_IS0_S0_Lj10EE(%"union.KV<hKey, hKey, 9>::Val"* %7, %class.KV.9* %8)
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj10EEC2EPKS0_S3_(%class.KV.9* %this, %class.hKey* %key, %class.hKey* %val) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.9*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  store %class.KV.9* %this, %class.KV.9** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  store %class.hKey* %val, %class.hKey** %3, align 8
  %4 = load %class.KV.9*, %class.KV.9** %1, align 8
  %5 = getelementptr inbounds %class.KV.9, %class.KV.9* %4, i32 0, i32 0
  %6 = load %class.hKey*, %class.hKey** %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj10EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 10>::Key"* %5, %class.hKey* %6)
  %7 = getelementptr inbounds %class.KV.9, %class.KV.9* %4, i32 0, i32 1
  %8 = load %class.hKey*, %class.hKey** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj10EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 10>::Val"* %7, %class.hKey* %8)
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2LLI4hKeyS0_EC2EPKS0_S3_PKS1_(%class.LL* %this, %class.hKey* %k, %class.hKey* %v, %class.LL* %next) unnamed_addr #0 comdat align 2 {
  %1 = alloca %class.LL*, align 8
  %2 = alloca %class.hKey*, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca %class.LL*, align 8
  store %class.LL* %this, %class.LL** %1, align 8
  store %class.hKey* %k, %class.hKey** %2, align 8
  store %class.hKey* %v, %class.hKey** %3, align 8
  store %class.LL* %next, %class.LL** %4, align 8
  %5 = load %class.LL*, %class.LL** %1, align 8
  %6 = getelementptr inbounds %class.LL, %class.LL* %5, i32 0, i32 0
  %7 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %7, %class.hKey** %6, align 8
  %8 = getelementptr inbounds %class.LL, %class.LL* %5, i32 0, i32 1
  %9 = load %class.hKey*, %class.hKey** %3, align 8
  store %class.hKey* %9, %class.hKey** %8, align 8
  %10 = getelementptr inbounds %class.LL, %class.LL* %5, i32 0, i32 2
  %11 = load %class.LL*, %class.LL** %4, align 8
  store %class.LL* %11, %class.LL** %10, align 8
  ret void
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj10EEC2EmPK2LLIS0_S0_E(%class.KV.9* %this, i64 %bm, %class.LL* %ll) unnamed_addr #2 comdat align 2 {
  %1 = alloca %class.KV.9*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.LL*, align 8
  store %class.KV.9* %this, %class.KV.9** %1, align 8
  store i64 %bm, i64* %2, align 8
  store %class.LL* %ll, %class.LL** %3, align 8
  %4 = load %class.KV.9*, %class.KV.9** %1, align 8
  %5 = getelementptr inbounds %class.KV.9, %class.KV.9* %4, i32 0, i32 0
  %6 = load i64, i64* %2, align 8
  call void @_ZN2KVI4hKeyS0_Lj10EE3KeyC2Em(%"union.KV<hKey, hKey, 10>::Key"* %5, i64 %6)
  %7 = getelementptr inbounds %class.KV.9, %class.KV.9* %4, i32 0, i32 1
  %8 = load %class.LL*, %class.LL** %3, align 8
  call void @_ZN2KVI4hKeyS0_Lj10EE3ValC2EPK2LLIS0_S0_E(%"union.KV<hKey, hKey, 10>::Val"* %7, %class.LL* %8)
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj10EE3KeyC2Em(%"union.KV<hKey, hKey, 10>::Key"* %this, i64 %bm) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 10>::Key"*, align 8
  %2 = alloca i64, align 8
  store %"union.KV<hKey, hKey, 10>::Key"* %this, %"union.KV<hKey, hKey, 10>::Key"** %1, align 8
  store i64 %bm, i64* %2, align 8
  %3 = load %"union.KV<hKey, hKey, 10>::Key"*, %"union.KV<hKey, hKey, 10>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 10>::Key"* %3 to i64*
  %5 = load i64, i64* %2, align 8
  store i64 %5, i64* %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj10EE3ValC2EPK2LLIS0_S0_E(%"union.KV<hKey, hKey, 10>::Val"* %this, %class.LL* %ll) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 10>::Val"*, align 8
  %2 = alloca %class.LL*, align 8
  store %"union.KV<hKey, hKey, 10>::Val"* %this, %"union.KV<hKey, hKey, 10>::Val"** %1, align 8
  store %class.LL* %ll, %class.LL** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 10>::Val"*, %"union.KV<hKey, hKey, 10>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 10>::Val"* %3 to %class.LL**
  %5 = load %class.LL*, %class.LL** %2, align 8
  store %class.LL* %5, %class.LL** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj9EE3KeyC2Em(%"union.KV<hKey, hKey, 9>::Key"* %this, i64 %bm) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 9>::Key"*, align 8
  %2 = alloca i64, align 8
  store %"union.KV<hKey, hKey, 9>::Key"* %this, %"union.KV<hKey, hKey, 9>::Key"** %1, align 8
  store i64 %bm, i64* %2, align 8
  %3 = load %"union.KV<hKey, hKey, 9>::Key"*, %"union.KV<hKey, hKey, 9>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 9>::Key"* %3 to i64*
  %5 = load i64, i64* %2, align 8
  store i64 %5, i64* %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj9EE3ValC2EPKS_IS0_S0_Lj10EE(%"union.KV<hKey, hKey, 9>::Val"* %this, %class.KV.9* %node) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 9>::Val"*, align 8
  %2 = alloca %class.KV.9*, align 8
  store %"union.KV<hKey, hKey, 9>::Val"* %this, %"union.KV<hKey, hKey, 9>::Val"** %1, align 8
  store %class.KV.9* %node, %class.KV.9** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 9>::Val"*, %"union.KV<hKey, hKey, 9>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 9>::Val"* %3 to %class.KV.9**
  %5 = load %class.KV.9*, %class.KV.9** %2, align 8
  store %class.KV.9* %5, %class.KV.9** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj10EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 10>::Key"* %this, %class.hKey* %key) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 10>::Key"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 10>::Key"* %this, %"union.KV<hKey, hKey, 10>::Key"** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 10>::Key"*, %"union.KV<hKey, hKey, 10>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 10>::Key"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj10EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 10>::Val"* %this, %class.hKey* %val) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 10>::Val"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 10>::Val"* %this, %"union.KV<hKey, hKey, 10>::Val"** %1, align 8
  store %class.hKey* %val, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 10>::Val"*, %"union.KV<hKey, hKey, 10>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 10>::Val"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj8EE3KeyC2Em(%"union.KV<hKey, hKey, 8>::Key"* %this, i64 %bm) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 8>::Key"*, align 8
  %2 = alloca i64, align 8
  store %"union.KV<hKey, hKey, 8>::Key"* %this, %"union.KV<hKey, hKey, 8>::Key"** %1, align 8
  store i64 %bm, i64* %2, align 8
  %3 = load %"union.KV<hKey, hKey, 8>::Key"*, %"union.KV<hKey, hKey, 8>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 8>::Key"* %3 to i64*
  %5 = load i64, i64* %2, align 8
  store i64 %5, i64* %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj8EE3ValC2EPKS_IS0_S0_Lj9EE(%"union.KV<hKey, hKey, 8>::Val"* %this, %class.KV.8* %node) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 8>::Val"*, align 8
  %2 = alloca %class.KV.8*, align 8
  store %"union.KV<hKey, hKey, 8>::Val"* %this, %"union.KV<hKey, hKey, 8>::Val"** %1, align 8
  store %class.KV.8* %node, %class.KV.8** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 8>::Val"*, %"union.KV<hKey, hKey, 8>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 8>::Val"* %3 to %class.KV.8**
  %5 = load %class.KV.8*, %class.KV.8** %2, align 8
  store %class.KV.8* %5, %class.KV.8** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj9EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 9>::Key"* %this, %class.hKey* %key) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 9>::Key"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 9>::Key"* %this, %"union.KV<hKey, hKey, 9>::Key"** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 9>::Key"*, %"union.KV<hKey, hKey, 9>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 9>::Key"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj9EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 9>::Val"* %this, %class.hKey* %val) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 9>::Val"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 9>::Val"* %this, %"union.KV<hKey, hKey, 9>::Val"** %1, align 8
  store %class.hKey* %val, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 9>::Val"*, %"union.KV<hKey, hKey, 9>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 9>::Val"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj7EE3KeyC2Em(%"union.KV<hKey, hKey, 7>::Key"* %this, i64 %bm) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 7>::Key"*, align 8
  %2 = alloca i64, align 8
  store %"union.KV<hKey, hKey, 7>::Key"* %this, %"union.KV<hKey, hKey, 7>::Key"** %1, align 8
  store i64 %bm, i64* %2, align 8
  %3 = load %"union.KV<hKey, hKey, 7>::Key"*, %"union.KV<hKey, hKey, 7>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 7>::Key"* %3 to i64*
  %5 = load i64, i64* %2, align 8
  store i64 %5, i64* %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj7EE3ValC2EPKS_IS0_S0_Lj8EE(%"union.KV<hKey, hKey, 7>::Val"* %this, %class.KV.7* %node) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 7>::Val"*, align 8
  %2 = alloca %class.KV.7*, align 8
  store %"union.KV<hKey, hKey, 7>::Val"* %this, %"union.KV<hKey, hKey, 7>::Val"** %1, align 8
  store %class.KV.7* %node, %class.KV.7** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 7>::Val"*, %"union.KV<hKey, hKey, 7>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 7>::Val"* %3 to %class.KV.7**
  %5 = load %class.KV.7*, %class.KV.7** %2, align 8
  store %class.KV.7* %5, %class.KV.7** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj8EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 8>::Key"* %this, %class.hKey* %key) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 8>::Key"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 8>::Key"* %this, %"union.KV<hKey, hKey, 8>::Key"** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 8>::Key"*, %"union.KV<hKey, hKey, 8>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 8>::Key"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj8EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 8>::Val"* %this, %class.hKey* %val) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 8>::Val"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 8>::Val"* %this, %"union.KV<hKey, hKey, 8>::Val"** %1, align 8
  store %class.hKey* %val, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 8>::Val"*, %"union.KV<hKey, hKey, 8>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 8>::Val"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj6EE3KeyC2Em(%"union.KV<hKey, hKey, 6>::Key"* %this, i64 %bm) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 6>::Key"*, align 8
  %2 = alloca i64, align 8
  store %"union.KV<hKey, hKey, 6>::Key"* %this, %"union.KV<hKey, hKey, 6>::Key"** %1, align 8
  store i64 %bm, i64* %2, align 8
  %3 = load %"union.KV<hKey, hKey, 6>::Key"*, %"union.KV<hKey, hKey, 6>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 6>::Key"* %3 to i64*
  %5 = load i64, i64* %2, align 8
  store i64 %5, i64* %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj6EE3ValC2EPKS_IS0_S0_Lj7EE(%"union.KV<hKey, hKey, 6>::Val"* %this, %class.KV.6* %node) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 6>::Val"*, align 8
  %2 = alloca %class.KV.6*, align 8
  store %"union.KV<hKey, hKey, 6>::Val"* %this, %"union.KV<hKey, hKey, 6>::Val"** %1, align 8
  store %class.KV.6* %node, %class.KV.6** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 6>::Val"*, %"union.KV<hKey, hKey, 6>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 6>::Val"* %3 to %class.KV.6**
  %5 = load %class.KV.6*, %class.KV.6** %2, align 8
  store %class.KV.6* %5, %class.KV.6** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj7EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 7>::Key"* %this, %class.hKey* %key) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 7>::Key"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 7>::Key"* %this, %"union.KV<hKey, hKey, 7>::Key"** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 7>::Key"*, %"union.KV<hKey, hKey, 7>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 7>::Key"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj7EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 7>::Val"* %this, %class.hKey* %val) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 7>::Val"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 7>::Val"* %this, %"union.KV<hKey, hKey, 7>::Val"** %1, align 8
  store %class.hKey* %val, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 7>::Val"*, %"union.KV<hKey, hKey, 7>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 7>::Val"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj5EE3KeyC2Em(%"union.KV<hKey, hKey, 5>::Key"* %this, i64 %bm) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 5>::Key"*, align 8
  %2 = alloca i64, align 8
  store %"union.KV<hKey, hKey, 5>::Key"* %this, %"union.KV<hKey, hKey, 5>::Key"** %1, align 8
  store i64 %bm, i64* %2, align 8
  %3 = load %"union.KV<hKey, hKey, 5>::Key"*, %"union.KV<hKey, hKey, 5>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 5>::Key"* %3 to i64*
  %5 = load i64, i64* %2, align 8
  store i64 %5, i64* %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj5EE3ValC2EPKS_IS0_S0_Lj6EE(%"union.KV<hKey, hKey, 5>::Val"* %this, %class.KV.5* %node) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 5>::Val"*, align 8
  %2 = alloca %class.KV.5*, align 8
  store %"union.KV<hKey, hKey, 5>::Val"* %this, %"union.KV<hKey, hKey, 5>::Val"** %1, align 8
  store %class.KV.5* %node, %class.KV.5** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 5>::Val"*, %"union.KV<hKey, hKey, 5>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 5>::Val"* %3 to %class.KV.5**
  %5 = load %class.KV.5*, %class.KV.5** %2, align 8
  store %class.KV.5* %5, %class.KV.5** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj6EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 6>::Key"* %this, %class.hKey* %key) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 6>::Key"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 6>::Key"* %this, %"union.KV<hKey, hKey, 6>::Key"** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 6>::Key"*, %"union.KV<hKey, hKey, 6>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 6>::Key"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj6EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 6>::Val"* %this, %class.hKey* %val) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 6>::Val"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 6>::Val"* %this, %"union.KV<hKey, hKey, 6>::Val"** %1, align 8
  store %class.hKey* %val, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 6>::Val"*, %"union.KV<hKey, hKey, 6>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 6>::Val"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj4EE3KeyC2Em(%"union.KV<hKey, hKey, 4>::Key"* %this, i64 %bm) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 4>::Key"*, align 8
  %2 = alloca i64, align 8
  store %"union.KV<hKey, hKey, 4>::Key"* %this, %"union.KV<hKey, hKey, 4>::Key"** %1, align 8
  store i64 %bm, i64* %2, align 8
  %3 = load %"union.KV<hKey, hKey, 4>::Key"*, %"union.KV<hKey, hKey, 4>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 4>::Key"* %3 to i64*
  %5 = load i64, i64* %2, align 8
  store i64 %5, i64* %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj4EE3ValC2EPKS_IS0_S0_Lj5EE(%"union.KV<hKey, hKey, 4>::Val"* %this, %class.KV.4* %node) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 4>::Val"*, align 8
  %2 = alloca %class.KV.4*, align 8
  store %"union.KV<hKey, hKey, 4>::Val"* %this, %"union.KV<hKey, hKey, 4>::Val"** %1, align 8
  store %class.KV.4* %node, %class.KV.4** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 4>::Val"*, %"union.KV<hKey, hKey, 4>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 4>::Val"* %3 to %class.KV.4**
  %5 = load %class.KV.4*, %class.KV.4** %2, align 8
  store %class.KV.4* %5, %class.KV.4** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj5EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 5>::Key"* %this, %class.hKey* %key) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 5>::Key"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 5>::Key"* %this, %"union.KV<hKey, hKey, 5>::Key"** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 5>::Key"*, %"union.KV<hKey, hKey, 5>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 5>::Key"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj5EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 5>::Val"* %this, %class.hKey* %val) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 5>::Val"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 5>::Val"* %this, %"union.KV<hKey, hKey, 5>::Val"** %1, align 8
  store %class.hKey* %val, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 5>::Val"*, %"union.KV<hKey, hKey, 5>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 5>::Val"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj3EE3KeyC2Em(%"union.KV<hKey, hKey, 3>::Key"* %this, i64 %bm) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 3>::Key"*, align 8
  %2 = alloca i64, align 8
  store %"union.KV<hKey, hKey, 3>::Key"* %this, %"union.KV<hKey, hKey, 3>::Key"** %1, align 8
  store i64 %bm, i64* %2, align 8
  %3 = load %"union.KV<hKey, hKey, 3>::Key"*, %"union.KV<hKey, hKey, 3>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 3>::Key"* %3 to i64*
  %5 = load i64, i64* %2, align 8
  store i64 %5, i64* %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj3EE3ValC2EPKS_IS0_S0_Lj4EE(%"union.KV<hKey, hKey, 3>::Val"* %this, %class.KV.3* %node) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 3>::Val"*, align 8
  %2 = alloca %class.KV.3*, align 8
  store %"union.KV<hKey, hKey, 3>::Val"* %this, %"union.KV<hKey, hKey, 3>::Val"** %1, align 8
  store %class.KV.3* %node, %class.KV.3** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 3>::Val"*, %"union.KV<hKey, hKey, 3>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 3>::Val"* %3 to %class.KV.3**
  %5 = load %class.KV.3*, %class.KV.3** %2, align 8
  store %class.KV.3* %5, %class.KV.3** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj4EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 4>::Key"* %this, %class.hKey* %key) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 4>::Key"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 4>::Key"* %this, %"union.KV<hKey, hKey, 4>::Key"** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 4>::Key"*, %"union.KV<hKey, hKey, 4>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 4>::Key"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj4EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 4>::Val"* %this, %class.hKey* %val) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 4>::Val"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 4>::Val"* %this, %"union.KV<hKey, hKey, 4>::Val"** %1, align 8
  store %class.hKey* %val, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 4>::Val"*, %"union.KV<hKey, hKey, 4>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 4>::Val"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj2EE3KeyC2Em(%"union.KV<hKey, hKey, 2>::Key"* %this, i64 %bm) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 2>::Key"*, align 8
  %2 = alloca i64, align 8
  store %"union.KV<hKey, hKey, 2>::Key"* %this, %"union.KV<hKey, hKey, 2>::Key"** %1, align 8
  store i64 %bm, i64* %2, align 8
  %3 = load %"union.KV<hKey, hKey, 2>::Key"*, %"union.KV<hKey, hKey, 2>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 2>::Key"* %3 to i64*
  %5 = load i64, i64* %2, align 8
  store i64 %5, i64* %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj2EE3ValC2EPKS_IS0_S0_Lj3EE(%"union.KV<hKey, hKey, 2>::Val"* %this, %class.KV.2* %node) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 2>::Val"*, align 8
  %2 = alloca %class.KV.2*, align 8
  store %"union.KV<hKey, hKey, 2>::Val"* %this, %"union.KV<hKey, hKey, 2>::Val"** %1, align 8
  store %class.KV.2* %node, %class.KV.2** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 2>::Val"*, %"union.KV<hKey, hKey, 2>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 2>::Val"* %3 to %class.KV.2**
  %5 = load %class.KV.2*, %class.KV.2** %2, align 8
  store %class.KV.2* %5, %class.KV.2** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj3EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 3>::Key"* %this, %class.hKey* %key) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 3>::Key"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 3>::Key"* %this, %"union.KV<hKey, hKey, 3>::Key"** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 3>::Key"*, %"union.KV<hKey, hKey, 3>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 3>::Key"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj3EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 3>::Val"* %this, %class.hKey* %val) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 3>::Val"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 3>::Val"* %this, %"union.KV<hKey, hKey, 3>::Val"** %1, align 8
  store %class.hKey* %val, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 3>::Val"*, %"union.KV<hKey, hKey, 3>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 3>::Val"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj1EE3KeyC2Em(%"union.KV<hKey, hKey, 1>::Key"* %this, i64 %bm) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 1>::Key"*, align 8
  %2 = alloca i64, align 8
  store %"union.KV<hKey, hKey, 1>::Key"* %this, %"union.KV<hKey, hKey, 1>::Key"** %1, align 8
  store i64 %bm, i64* %2, align 8
  %3 = load %"union.KV<hKey, hKey, 1>::Key"*, %"union.KV<hKey, hKey, 1>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 1>::Key"* %3 to i64*
  %5 = load i64, i64* %2, align 8
  store i64 %5, i64* %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj1EE3ValC2EPKS_IS0_S0_Lj2EE(%"union.KV<hKey, hKey, 1>::Val"* %this, %class.KV.1* %node) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 1>::Val"*, align 8
  %2 = alloca %class.KV.1*, align 8
  store %"union.KV<hKey, hKey, 1>::Val"* %this, %"union.KV<hKey, hKey, 1>::Val"** %1, align 8
  store %class.KV.1* %node, %class.KV.1** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 1>::Val"*, %"union.KV<hKey, hKey, 1>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 1>::Val"* %3 to %class.KV.1**
  %5 = load %class.KV.1*, %class.KV.1** %2, align 8
  store %class.KV.1* %5, %class.KV.1** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj2EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 2>::Key"* %this, %class.hKey* %key) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 2>::Key"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 2>::Key"* %this, %"union.KV<hKey, hKey, 2>::Key"** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 2>::Key"*, %"union.KV<hKey, hKey, 2>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 2>::Key"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj2EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 2>::Val"* %this, %class.hKey* %val) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 2>::Val"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 2>::Val"* %this, %"union.KV<hKey, hKey, 2>::Val"** %1, align 8
  store %class.hKey* %val, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 2>::Val"*, %"union.KV<hKey, hKey, 2>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 2>::Val"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj0EE3KeyC2Em(%"union.KV<hKey, hKey, 0>::Key"* %this, i64 %bm) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 0>::Key"*, align 8
  %2 = alloca i64, align 8
  store %"union.KV<hKey, hKey, 0>::Key"* %this, %"union.KV<hKey, hKey, 0>::Key"** %1, align 8
  store i64 %bm, i64* %2, align 8
  %3 = load %"union.KV<hKey, hKey, 0>::Key"*, %"union.KV<hKey, hKey, 0>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 0>::Key"* %3 to i64*
  %5 = load i64, i64* %2, align 8
  store i64 %5, i64* %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj0EE3ValC2EPKS_IS0_S0_Lj1EE(%"union.KV<hKey, hKey, 0>::Val"* %this, %class.KV.0* %node) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 0>::Val"*, align 8
  %2 = alloca %class.KV.0*, align 8
  store %"union.KV<hKey, hKey, 0>::Val"* %this, %"union.KV<hKey, hKey, 0>::Val"** %1, align 8
  store %class.KV.0* %node, %class.KV.0** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 0>::Val"*, %"union.KV<hKey, hKey, 0>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 0>::Val"* %3 to %class.KV.0**
  %5 = load %class.KV.0*, %class.KV.0** %2, align 8
  store %class.KV.0* %5, %class.KV.0** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj1EE3KeyC2EPKS0_(%"union.KV<hKey, hKey, 1>::Key"* %this, %class.hKey* %key) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 1>::Key"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 1>::Key"* %this, %"union.KV<hKey, hKey, 1>::Key"** %1, align 8
  store %class.hKey* %key, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 1>::Key"*, %"union.KV<hKey, hKey, 1>::Key"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 1>::Key"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj1EE3ValC2EPKS0_(%"union.KV<hKey, hKey, 1>::Val"* %this, %class.hKey* %val) unnamed_addr #0 comdat align 2 {
  %1 = alloca %"union.KV<hKey, hKey, 1>::Val"*, align 8
  %2 = alloca %class.hKey*, align 8
  store %"union.KV<hKey, hKey, 1>::Val"* %this, %"union.KV<hKey, hKey, 1>::Val"** %1, align 8
  store %class.hKey* %val, %class.hKey** %2, align 8
  %3 = load %"union.KV<hKey, hKey, 1>::Val"*, %"union.KV<hKey, hKey, 1>::Val"** %1, align 8
  %4 = bitcast %"union.KV<hKey, hKey, 1>::Val"* %3 to %class.hKey**
  %5 = load %class.hKey*, %class.hKey** %2, align 8
  store %class.hKey* %5, %class.hKey** %4, align 8
  ret void
}

; Function Attrs: nounwind readnone
declare i64 @llvm.ctpop.i64(i64) #7

; Function Attrs: uwtable
define linkonce_odr %class.KV.0* @_ZN2KVI4hKeyS0_Lj1EE11update_nodeEPKS1_jjRS2_(%class.KV.0* %old, i32 %count, i32 %i, %class.KV.0* dereferenceable(16) %kv) #2 comdat align 2 {
  %1 = alloca %class.KV.0*, align 8
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca %class.KV.0*, align 8
  %copy = alloca %class.KV.0*, align 8
  store %class.KV.0* %old, %class.KV.0** %1, align 8
  store i32 %count, i32* %2, align 4
  store i32 %i, i32* %3, align 4
  store %class.KV.0* %kv, %class.KV.0** %4, align 8
  %5 = load i32, i32* %2, align 4
  %6 = zext i32 %5 to i64
  %7 = mul i64 %6, 16
  %8 = call noalias i8* @malloc(i64 %7) #8
  %9 = bitcast i8* %8 to %class.KV.0*
  store %class.KV.0* %9, %class.KV.0** %copy, align 8
  %10 = load %class.KV.0*, %class.KV.0** %copy, align 8
  %11 = bitcast %class.KV.0* %10 to i8*
  %12 = load %class.KV.0*, %class.KV.0** %1, align 8
  %13 = bitcast %class.KV.0* %12 to i8*
  %14 = load i32, i32* %2, align 4
  %15 = zext i32 %14 to i64
  %16 = mul i64 %15, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %11, i8* %13, i64 %16, i32 8, i1 false)
  %17 = load %class.KV.0*, %class.KV.0** %copy, align 8
  %18 = load i32, i32* %3, align 4
  %19 = zext i32 %18 to i64
  %20 = getelementptr inbounds %class.KV.0, %class.KV.0* %17, i64 %19
  %21 = bitcast %class.KV.0* %20 to i8*
  %22 = bitcast i8* %21 to %class.KV.0*
  %23 = load %class.KV.0*, %class.KV.0** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2ERKS1_(%class.KV.0* %22, %class.KV.0* dereferenceable(16) %23)
  %24 = load %class.KV.0*, %class.KV.0** %copy, align 8
  ret %class.KV.0* %24
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj1EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.0* noalias sret %agg.result, %class.KV.0* dereferenceable(16) %kv, i64 %h, %class.hKey* %key, %class.hKey* %val, i64* %cptr) #2 comdat align 2 {
  %1 = alloca %class.KV.0*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca %class.hKey*, align 8
  %5 = alloca i64*, align 8
  %data = alloca %class.KV.1*, align 8
  %bm = alloca i64, align 8
  %hpiece = alloca i32, align 4
  %count = alloca i32, align 4
  %i = alloca i32, align 4
  %exists = alloca i8, align 1
  %node = alloca %class.KV.1*, align 8
  %6 = alloca %class.KV.1, align 8
  %childkv = alloca %class.KV.1, align 8
  %node1 = alloca %class.KV.1*, align 8
  %childkv2 = alloca %class.KV.1, align 8
  %node3 = alloca %class.KV.1*, align 8
  %node4 = alloca %class.KV.1*, align 8
  store %class.KV.0* %kv, %class.KV.0** %1, align 8
  store i64 %h, i64* %2, align 8
  store %class.hKey* %key, %class.hKey** %3, align 8
  store %class.hKey* %val, %class.hKey** %4, align 8
  store i64* %cptr, i64** %5, align 8
  %7 = load %class.KV.0*, %class.KV.0** %1, align 8
  %8 = getelementptr inbounds %class.KV.0, %class.KV.0* %7, i32 0, i32 1
  %9 = bitcast %"union.KV<hKey, hKey, 1>::Val"* %8 to %class.KV.1**
  %10 = load %class.KV.1*, %class.KV.1** %9, align 8
  store %class.KV.1* %10, %class.KV.1** %data, align 8
  %11 = load %class.KV.0*, %class.KV.0** %1, align 8
  %12 = getelementptr inbounds %class.KV.0, %class.KV.0* %11, i32 0, i32 0
  %13 = bitcast %"union.KV<hKey, hKey, 1>::Key"* %12 to i64*
  %14 = load i64, i64* %13, align 8
  %15 = lshr i64 %14, 1
  store i64 %15, i64* %bm, align 8
  %16 = load i64, i64* %2, align 8
  %17 = and i64 %16, 63
  %18 = urem i64 %17, 63
  %19 = trunc i64 %18 to i32
  store i32 %19, i32* %hpiece, align 4
  %20 = load i64, i64* %bm, align 8
  %21 = call i64 @llvm.ctpop.i64(i64 %20)
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %count, align 4
  %23 = load i64, i64* %bm, align 8
  %24 = shl i64 %23, 1
  %25 = load i32, i32* %hpiece, align 4
  %26 = sub i32 63, %25
  %27 = zext i32 %26 to i64
  %28 = shl i64 %24, %27
  %29 = call i64 @llvm.ctpop.i64(i64 %28)
  %30 = trunc i64 %29 to i32
  store i32 %30, i32* %i, align 4
  %31 = load i64, i64* %bm, align 8
  %32 = load i32, i32* %hpiece, align 4
  %33 = zext i32 %32 to i64
  %34 = shl i64 1, %33
  %35 = and i64 %31, %34
  %36 = icmp ne i64 %35, 0
  %37 = zext i1 %36 to i8
  store i8 %37, i8* %exists, align 1
  %38 = load i8, i8* %exists, align 1
  %39 = trunc i8 %38 to i1
  br i1 %39, label %40, label %131

; <label>:40                                      ; preds = %0
  %41 = load i32, i32* %i, align 4
  %42 = zext i32 %41 to i64
  %43 = load %class.KV.1*, %class.KV.1** %data, align 8
  %44 = getelementptr inbounds %class.KV.1, %class.KV.1* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.1, %class.KV.1* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<hKey, hKey, 2>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %112

; <label>:50                                      ; preds = %40
  %51 = load i32, i32* %i, align 4
  %52 = zext i32 %51 to i64
  %53 = load %class.KV.1*, %class.KV.1** %data, align 8
  %54 = getelementptr inbounds %class.KV.1, %class.KV.1* %53, i64 %52
  %55 = getelementptr inbounds %class.KV.1, %class.KV.1* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<hKey, hKey, 2>::Key"* %55 to %class.hKey**
  %57 = load %class.hKey*, %class.hKey** %56, align 8
  %58 = load %class.hKey*, %class.hKey** %3, align 8
  %59 = call zeroext i1 @_ZNK4hKeyeqERKS_(%class.hKey* %57, %class.hKey* dereferenceable(8) %58)
  br i1 %59, label %60, label %72

; <label>:60                                      ; preds = %50
  %61 = load %class.KV.1*, %class.KV.1** %data, align 8
  %62 = load i32, i32* %count, align 4
  %63 = load i32, i32* %i, align 4
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  %65 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %6, %class.hKey* %64, %class.hKey* %65)
  %66 = call %class.KV.1* @_ZN2KVI4hKeyS0_Lj2EE11update_nodeEPKS1_jjRS2_(%class.KV.1* %61, i32 %62, i32 %63, %class.KV.1* dereferenceable(16) %6)
  store %class.KV.1* %66, %class.KV.1** %node, align 8
  %67 = load %class.KV.0*, %class.KV.0** %1, align 8
  %68 = getelementptr inbounds %class.KV.0, %class.KV.0* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<hKey, hKey, 1>::Key"* %68 to i64*
  %70 = load i64, i64* %69, align 8
  %71 = load %class.KV.1*, %class.KV.1** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %agg.result, i64 %70, %class.KV.1* %71)
  br label %180

; <label>:72                                      ; preds = %50
  %73 = load i64*, i64** %5, align 8
  %74 = load i64, i64* %73, align 8
  %75 = add i64 %74, 1
  store i64 %75, i64* %73, align 8
  %76 = load i32, i32* %i, align 4
  %77 = zext i32 %76 to i64
  %78 = load %class.KV.1*, %class.KV.1** %data, align 8
  %79 = getelementptr inbounds %class.KV.1, %class.KV.1* %78, i64 %77
  %80 = getelementptr inbounds %class.KV.1, %class.KV.1* %79, i32 0, i32 0
  %81 = bitcast %"union.KV<hKey, hKey, 2>::Key"* %80 to %class.hKey**
  %82 = load %class.hKey*, %class.hKey** %81, align 8
  %83 = call i64 @_ZNK4hKey4hashEv(%class.hKey* %82)
  %84 = lshr i64 %83, 16
  %85 = load i32, i32* %i, align 4
  %86 = zext i32 %85 to i64
  %87 = load %class.KV.1*, %class.KV.1** %data, align 8
  %88 = getelementptr inbounds %class.KV.1, %class.KV.1* %87, i64 %86
  %89 = getelementptr inbounds %class.KV.1, %class.KV.1* %88, i32 0, i32 0
  %90 = bitcast %"union.KV<hKey, hKey, 2>::Key"* %89 to %class.hKey**
  %91 = load %class.hKey*, %class.hKey** %90, align 8
  %92 = load i32, i32* %i, align 4
  %93 = zext i32 %92 to i64
  %94 = load %class.KV.1*, %class.KV.1** %data, align 8
  %95 = getelementptr inbounds %class.KV.1, %class.KV.1* %94, i64 %93
  %96 = getelementptr inbounds %class.KV.1, %class.KV.1* %95, i32 0, i32 1
  %97 = bitcast %"union.KV<hKey, hKey, 2>::Val"* %96 to %class.hKey**
  %98 = load %class.hKey*, %class.hKey** %97, align 8
  %99 = load i64, i64* %2, align 8
  %100 = lshr i64 %99, 6
  %101 = load %class.hKey*, %class.hKey** %3, align 8
  %102 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.1* sret %childkv, i64 %84, %class.hKey* %91, %class.hKey* %98, i64 %100, %class.hKey* %101, %class.hKey* %102)
  %103 = load %class.KV.1*, %class.KV.1** %data, align 8
  %104 = load i32, i32* %count, align 4
  %105 = load i32, i32* %i, align 4
  %106 = call %class.KV.1* @_ZN2KVI4hKeyS0_Lj2EE11update_nodeEPKS1_jjRS2_(%class.KV.1* %103, i32 %104, i32 %105, %class.KV.1* dereferenceable(16) %childkv)
  store %class.KV.1* %106, %class.KV.1** %node1, align 8
  %107 = load %class.KV.0*, %class.KV.0** %1, align 8
  %108 = getelementptr inbounds %class.KV.0, %class.KV.0* %107, i32 0, i32 0
  %109 = bitcast %"union.KV<hKey, hKey, 1>::Key"* %108 to i64*
  %110 = load i64, i64* %109, align 8
  %111 = load %class.KV.1*, %class.KV.1** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %agg.result, i64 %110, %class.KV.1* %111)
  br label %180

; <label>:112                                     ; preds = %40
  %113 = load i32, i32* %i, align 4
  %114 = zext i32 %113 to i64
  %115 = load %class.KV.1*, %class.KV.1** %data, align 8
  %116 = getelementptr inbounds %class.KV.1, %class.KV.1* %115, i64 %114
  %117 = load i64, i64* %2, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.hKey*, %class.hKey** %3, align 8
  %120 = load %class.hKey*, %class.hKey** %4, align 8
  %121 = load i64*, i64** %5, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.1* sret %childkv2, %class.KV.1* dereferenceable(16) %116, i64 %118, %class.hKey* %119, %class.hKey* %120, i64* %121)
  %122 = load %class.KV.1*, %class.KV.1** %data, align 8
  %123 = load i32, i32* %count, align 4
  %124 = load i32, i32* %i, align 4
  %125 = call %class.KV.1* @_ZN2KVI4hKeyS0_Lj2EE11update_nodeEPKS1_jjRS2_(%class.KV.1* %122, i32 %123, i32 %124, %class.KV.1* dereferenceable(16) %childkv2)
  store %class.KV.1* %125, %class.KV.1** %node3, align 8
  %126 = load %class.KV.0*, %class.KV.0** %1, align 8
  %127 = getelementptr inbounds %class.KV.0, %class.KV.0* %126, i32 0, i32 0
  %128 = bitcast %"union.KV<hKey, hKey, 1>::Key"* %127 to i64*
  %129 = load i64, i64* %128, align 8
  %130 = load %class.KV.1*, %class.KV.1** %node3, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %agg.result, i64 %129, %class.KV.1* %130)
  br label %180

; <label>:131                                     ; preds = %0
  %132 = load i64*, i64** %5, align 8
  %133 = load i64, i64* %132, align 8
  %134 = add i64 %133, 1
  store i64 %134, i64* %132, align 8
  %135 = load i32, i32* %count, align 4
  %136 = add i32 %135, 1
  %137 = zext i32 %136 to i64
  %138 = mul i64 %137, 16
  %139 = call noalias i8* @malloc(i64 %138) #8
  %140 = bitcast i8* %139 to %class.KV.1*
  store %class.KV.1* %140, %class.KV.1** %node4, align 8
  %141 = load %class.KV.1*, %class.KV.1** %node4, align 8
  %142 = bitcast %class.KV.1* %141 to i8*
  %143 = load %class.KV.1*, %class.KV.1** %data, align 8
  %144 = bitcast %class.KV.1* %143 to i8*
  %145 = load i32, i32* %i, align 4
  %146 = zext i32 %145 to i64
  %147 = mul i64 %146, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %142, i8* %144, i64 %147, i32 8, i1 false)
  %148 = load i32, i32* %i, align 4
  %149 = add i32 %148, 1
  %150 = zext i32 %149 to i64
  %151 = load %class.KV.1*, %class.KV.1** %node4, align 8
  %152 = getelementptr inbounds %class.KV.1, %class.KV.1* %151, i64 %150
  %153 = bitcast %class.KV.1* %152 to i8*
  %154 = load i32, i32* %i, align 4
  %155 = zext i32 %154 to i64
  %156 = load %class.KV.1*, %class.KV.1** %data, align 8
  %157 = getelementptr inbounds %class.KV.1, %class.KV.1* %156, i64 %155
  %158 = bitcast %class.KV.1* %157 to i8*
  %159 = load i32, i32* %count, align 4
  %160 = load i32, i32* %i, align 4
  %161 = sub i32 %159, %160
  %162 = zext i32 %161 to i64
  %163 = mul i64 %162, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %153, i8* %158, i64 %163, i32 8, i1 false)
  %164 = load %class.KV.1*, %class.KV.1** %node4, align 8
  %165 = load i32, i32* %i, align 4
  %166 = zext i32 %165 to i64
  %167 = getelementptr inbounds %class.KV.1, %class.KV.1* %164, i64 %166
  %168 = bitcast %class.KV.1* %167 to i8*
  %169 = bitcast i8* %168 to %class.KV.1*
  %170 = load %class.hKey*, %class.hKey** %3, align 8
  %171 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %169, %class.hKey* %170, %class.hKey* %171)
  %172 = load i64, i64* %bm, align 8
  %173 = load i32, i32* %hpiece, align 4
  %174 = zext i32 %173 to i64
  %175 = shl i64 1, %174
  %176 = or i64 %172, %175
  %177 = shl i64 %176, 1
  %178 = or i64 %177, 1
  %179 = load %class.KV.1*, %class.KV.1** %node4, align 8
  call void @_ZN2KVI4hKeyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %agg.result, i64 %178, %class.KV.1* %179)
  br label %180

; <label>:180                                     ; preds = %131, %112, %72, %60
  ret void
}

; Function Attrs: uwtable
define linkonce_odr %class.KV.1* @_ZN2KVI4hKeyS0_Lj2EE11update_nodeEPKS1_jjRS2_(%class.KV.1* %old, i32 %count, i32 %i, %class.KV.1* dereferenceable(16) %kv) #2 comdat align 2 {
  %1 = alloca %class.KV.1*, align 8
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca %class.KV.1*, align 8
  %copy = alloca %class.KV.1*, align 8
  store %class.KV.1* %old, %class.KV.1** %1, align 8
  store i32 %count, i32* %2, align 4
  store i32 %i, i32* %3, align 4
  store %class.KV.1* %kv, %class.KV.1** %4, align 8
  %5 = load i32, i32* %2, align 4
  %6 = zext i32 %5 to i64
  %7 = mul i64 %6, 16
  %8 = call noalias i8* @malloc(i64 %7) #8
  %9 = bitcast i8* %8 to %class.KV.1*
  store %class.KV.1* %9, %class.KV.1** %copy, align 8
  %10 = load %class.KV.1*, %class.KV.1** %copy, align 8
  %11 = bitcast %class.KV.1* %10 to i8*
  %12 = load %class.KV.1*, %class.KV.1** %1, align 8
  %13 = bitcast %class.KV.1* %12 to i8*
  %14 = load i32, i32* %2, align 4
  %15 = zext i32 %14 to i64
  %16 = mul i64 %15, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %11, i8* %13, i64 %16, i32 8, i1 false)
  %17 = load %class.KV.1*, %class.KV.1** %copy, align 8
  %18 = load i32, i32* %3, align 4
  %19 = zext i32 %18 to i64
  %20 = getelementptr inbounds %class.KV.1, %class.KV.1* %17, i64 %19
  %21 = bitcast %class.KV.1* %20 to i8*
  %22 = bitcast i8* %21 to %class.KV.1*
  %23 = load %class.KV.1*, %class.KV.1** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2ERKS1_(%class.KV.1* %22, %class.KV.1* dereferenceable(16) %23)
  %24 = load %class.KV.1*, %class.KV.1** %copy, align 8
  ret %class.KV.1* %24
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj2EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.1* noalias sret %agg.result, %class.KV.1* dereferenceable(16) %kv, i64 %h, %class.hKey* %key, %class.hKey* %val, i64* %cptr) #2 comdat align 2 {
  %1 = alloca %class.KV.1*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca %class.hKey*, align 8
  %5 = alloca i64*, align 8
  %data = alloca %class.KV.2*, align 8
  %bm = alloca i64, align 8
  %hpiece = alloca i32, align 4
  %count = alloca i32, align 4
  %i = alloca i32, align 4
  %exists = alloca i8, align 1
  %node = alloca %class.KV.2*, align 8
  %6 = alloca %class.KV.2, align 8
  %childkv = alloca %class.KV.2, align 8
  %node1 = alloca %class.KV.2*, align 8
  %childkv2 = alloca %class.KV.2, align 8
  %node3 = alloca %class.KV.2*, align 8
  %node4 = alloca %class.KV.2*, align 8
  store %class.KV.1* %kv, %class.KV.1** %1, align 8
  store i64 %h, i64* %2, align 8
  store %class.hKey* %key, %class.hKey** %3, align 8
  store %class.hKey* %val, %class.hKey** %4, align 8
  store i64* %cptr, i64** %5, align 8
  %7 = load %class.KV.1*, %class.KV.1** %1, align 8
  %8 = getelementptr inbounds %class.KV.1, %class.KV.1* %7, i32 0, i32 1
  %9 = bitcast %"union.KV<hKey, hKey, 2>::Val"* %8 to %class.KV.2**
  %10 = load %class.KV.2*, %class.KV.2** %9, align 8
  store %class.KV.2* %10, %class.KV.2** %data, align 8
  %11 = load %class.KV.1*, %class.KV.1** %1, align 8
  %12 = getelementptr inbounds %class.KV.1, %class.KV.1* %11, i32 0, i32 0
  %13 = bitcast %"union.KV<hKey, hKey, 2>::Key"* %12 to i64*
  %14 = load i64, i64* %13, align 8
  %15 = lshr i64 %14, 1
  store i64 %15, i64* %bm, align 8
  %16 = load i64, i64* %2, align 8
  %17 = and i64 %16, 63
  %18 = urem i64 %17, 63
  %19 = trunc i64 %18 to i32
  store i32 %19, i32* %hpiece, align 4
  %20 = load i64, i64* %bm, align 8
  %21 = call i64 @llvm.ctpop.i64(i64 %20)
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %count, align 4
  %23 = load i64, i64* %bm, align 8
  %24 = shl i64 %23, 1
  %25 = load i32, i32* %hpiece, align 4
  %26 = sub i32 63, %25
  %27 = zext i32 %26 to i64
  %28 = shl i64 %24, %27
  %29 = call i64 @llvm.ctpop.i64(i64 %28)
  %30 = trunc i64 %29 to i32
  store i32 %30, i32* %i, align 4
  %31 = load i64, i64* %bm, align 8
  %32 = load i32, i32* %hpiece, align 4
  %33 = zext i32 %32 to i64
  %34 = shl i64 1, %33
  %35 = and i64 %31, %34
  %36 = icmp ne i64 %35, 0
  %37 = zext i1 %36 to i8
  store i8 %37, i8* %exists, align 1
  %38 = load i8, i8* %exists, align 1
  %39 = trunc i8 %38 to i1
  br i1 %39, label %40, label %131

; <label>:40                                      ; preds = %0
  %41 = load i32, i32* %i, align 4
  %42 = zext i32 %41 to i64
  %43 = load %class.KV.2*, %class.KV.2** %data, align 8
  %44 = getelementptr inbounds %class.KV.2, %class.KV.2* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.2, %class.KV.2* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<hKey, hKey, 3>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %112

; <label>:50                                      ; preds = %40
  %51 = load i32, i32* %i, align 4
  %52 = zext i32 %51 to i64
  %53 = load %class.KV.2*, %class.KV.2** %data, align 8
  %54 = getelementptr inbounds %class.KV.2, %class.KV.2* %53, i64 %52
  %55 = getelementptr inbounds %class.KV.2, %class.KV.2* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<hKey, hKey, 3>::Key"* %55 to %class.hKey**
  %57 = load %class.hKey*, %class.hKey** %56, align 8
  %58 = load %class.hKey*, %class.hKey** %3, align 8
  %59 = call zeroext i1 @_ZNK4hKeyeqERKS_(%class.hKey* %57, %class.hKey* dereferenceable(8) %58)
  br i1 %59, label %60, label %72

; <label>:60                                      ; preds = %50
  %61 = load %class.KV.2*, %class.KV.2** %data, align 8
  %62 = load i32, i32* %count, align 4
  %63 = load i32, i32* %i, align 4
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  %65 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %6, %class.hKey* %64, %class.hKey* %65)
  %66 = call %class.KV.2* @_ZN2KVI4hKeyS0_Lj3EE11update_nodeEPKS1_jjRS2_(%class.KV.2* %61, i32 %62, i32 %63, %class.KV.2* dereferenceable(16) %6)
  store %class.KV.2* %66, %class.KV.2** %node, align 8
  %67 = load %class.KV.1*, %class.KV.1** %1, align 8
  %68 = getelementptr inbounds %class.KV.1, %class.KV.1* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<hKey, hKey, 2>::Key"* %68 to i64*
  %70 = load i64, i64* %69, align 8
  %71 = load %class.KV.2*, %class.KV.2** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %agg.result, i64 %70, %class.KV.2* %71)
  br label %180

; <label>:72                                      ; preds = %50
  %73 = load i64*, i64** %5, align 8
  %74 = load i64, i64* %73, align 8
  %75 = add i64 %74, 1
  store i64 %75, i64* %73, align 8
  %76 = load i32, i32* %i, align 4
  %77 = zext i32 %76 to i64
  %78 = load %class.KV.2*, %class.KV.2** %data, align 8
  %79 = getelementptr inbounds %class.KV.2, %class.KV.2* %78, i64 %77
  %80 = getelementptr inbounds %class.KV.2, %class.KV.2* %79, i32 0, i32 0
  %81 = bitcast %"union.KV<hKey, hKey, 3>::Key"* %80 to %class.hKey**
  %82 = load %class.hKey*, %class.hKey** %81, align 8
  %83 = call i64 @_ZNK4hKey4hashEv(%class.hKey* %82)
  %84 = lshr i64 %83, 22
  %85 = load i32, i32* %i, align 4
  %86 = zext i32 %85 to i64
  %87 = load %class.KV.2*, %class.KV.2** %data, align 8
  %88 = getelementptr inbounds %class.KV.2, %class.KV.2* %87, i64 %86
  %89 = getelementptr inbounds %class.KV.2, %class.KV.2* %88, i32 0, i32 0
  %90 = bitcast %"union.KV<hKey, hKey, 3>::Key"* %89 to %class.hKey**
  %91 = load %class.hKey*, %class.hKey** %90, align 8
  %92 = load i32, i32* %i, align 4
  %93 = zext i32 %92 to i64
  %94 = load %class.KV.2*, %class.KV.2** %data, align 8
  %95 = getelementptr inbounds %class.KV.2, %class.KV.2* %94, i64 %93
  %96 = getelementptr inbounds %class.KV.2, %class.KV.2* %95, i32 0, i32 1
  %97 = bitcast %"union.KV<hKey, hKey, 3>::Val"* %96 to %class.hKey**
  %98 = load %class.hKey*, %class.hKey** %97, align 8
  %99 = load i64, i64* %2, align 8
  %100 = lshr i64 %99, 6
  %101 = load %class.hKey*, %class.hKey** %3, align 8
  %102 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.2* sret %childkv, i64 %84, %class.hKey* %91, %class.hKey* %98, i64 %100, %class.hKey* %101, %class.hKey* %102)
  %103 = load %class.KV.2*, %class.KV.2** %data, align 8
  %104 = load i32, i32* %count, align 4
  %105 = load i32, i32* %i, align 4
  %106 = call %class.KV.2* @_ZN2KVI4hKeyS0_Lj3EE11update_nodeEPKS1_jjRS2_(%class.KV.2* %103, i32 %104, i32 %105, %class.KV.2* dereferenceable(16) %childkv)
  store %class.KV.2* %106, %class.KV.2** %node1, align 8
  %107 = load %class.KV.1*, %class.KV.1** %1, align 8
  %108 = getelementptr inbounds %class.KV.1, %class.KV.1* %107, i32 0, i32 0
  %109 = bitcast %"union.KV<hKey, hKey, 2>::Key"* %108 to i64*
  %110 = load i64, i64* %109, align 8
  %111 = load %class.KV.2*, %class.KV.2** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %agg.result, i64 %110, %class.KV.2* %111)
  br label %180

; <label>:112                                     ; preds = %40
  %113 = load i32, i32* %i, align 4
  %114 = zext i32 %113 to i64
  %115 = load %class.KV.2*, %class.KV.2** %data, align 8
  %116 = getelementptr inbounds %class.KV.2, %class.KV.2* %115, i64 %114
  %117 = load i64, i64* %2, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.hKey*, %class.hKey** %3, align 8
  %120 = load %class.hKey*, %class.hKey** %4, align 8
  %121 = load i64*, i64** %5, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.2* sret %childkv2, %class.KV.2* dereferenceable(16) %116, i64 %118, %class.hKey* %119, %class.hKey* %120, i64* %121)
  %122 = load %class.KV.2*, %class.KV.2** %data, align 8
  %123 = load i32, i32* %count, align 4
  %124 = load i32, i32* %i, align 4
  %125 = call %class.KV.2* @_ZN2KVI4hKeyS0_Lj3EE11update_nodeEPKS1_jjRS2_(%class.KV.2* %122, i32 %123, i32 %124, %class.KV.2* dereferenceable(16) %childkv2)
  store %class.KV.2* %125, %class.KV.2** %node3, align 8
  %126 = load %class.KV.1*, %class.KV.1** %1, align 8
  %127 = getelementptr inbounds %class.KV.1, %class.KV.1* %126, i32 0, i32 0
  %128 = bitcast %"union.KV<hKey, hKey, 2>::Key"* %127 to i64*
  %129 = load i64, i64* %128, align 8
  %130 = load %class.KV.2*, %class.KV.2** %node3, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %agg.result, i64 %129, %class.KV.2* %130)
  br label %180

; <label>:131                                     ; preds = %0
  %132 = load i64*, i64** %5, align 8
  %133 = load i64, i64* %132, align 8
  %134 = add i64 %133, 1
  store i64 %134, i64* %132, align 8
  %135 = load i32, i32* %count, align 4
  %136 = add i32 %135, 1
  %137 = zext i32 %136 to i64
  %138 = mul i64 %137, 16
  %139 = call noalias i8* @malloc(i64 %138) #8
  %140 = bitcast i8* %139 to %class.KV.2*
  store %class.KV.2* %140, %class.KV.2** %node4, align 8
  %141 = load %class.KV.2*, %class.KV.2** %node4, align 8
  %142 = bitcast %class.KV.2* %141 to i8*
  %143 = load %class.KV.2*, %class.KV.2** %data, align 8
  %144 = bitcast %class.KV.2* %143 to i8*
  %145 = load i32, i32* %i, align 4
  %146 = zext i32 %145 to i64
  %147 = mul i64 %146, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %142, i8* %144, i64 %147, i32 8, i1 false)
  %148 = load i32, i32* %i, align 4
  %149 = add i32 %148, 1
  %150 = zext i32 %149 to i64
  %151 = load %class.KV.2*, %class.KV.2** %node4, align 8
  %152 = getelementptr inbounds %class.KV.2, %class.KV.2* %151, i64 %150
  %153 = bitcast %class.KV.2* %152 to i8*
  %154 = load i32, i32* %i, align 4
  %155 = zext i32 %154 to i64
  %156 = load %class.KV.2*, %class.KV.2** %data, align 8
  %157 = getelementptr inbounds %class.KV.2, %class.KV.2* %156, i64 %155
  %158 = bitcast %class.KV.2* %157 to i8*
  %159 = load i32, i32* %count, align 4
  %160 = load i32, i32* %i, align 4
  %161 = sub i32 %159, %160
  %162 = zext i32 %161 to i64
  %163 = mul i64 %162, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %153, i8* %158, i64 %163, i32 8, i1 false)
  %164 = load %class.KV.2*, %class.KV.2** %node4, align 8
  %165 = load i32, i32* %i, align 4
  %166 = zext i32 %165 to i64
  %167 = getelementptr inbounds %class.KV.2, %class.KV.2* %164, i64 %166
  %168 = bitcast %class.KV.2* %167 to i8*
  %169 = bitcast i8* %168 to %class.KV.2*
  %170 = load %class.hKey*, %class.hKey** %3, align 8
  %171 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %169, %class.hKey* %170, %class.hKey* %171)
  %172 = load i64, i64* %bm, align 8
  %173 = load i32, i32* %hpiece, align 4
  %174 = zext i32 %173 to i64
  %175 = shl i64 1, %174
  %176 = or i64 %172, %175
  %177 = shl i64 %176, 1
  %178 = or i64 %177, 1
  %179 = load %class.KV.2*, %class.KV.2** %node4, align 8
  call void @_ZN2KVI4hKeyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %agg.result, i64 %178, %class.KV.2* %179)
  br label %180

; <label>:180                                     ; preds = %131, %112, %72, %60
  ret void
}

; Function Attrs: uwtable
define linkonce_odr %class.KV.2* @_ZN2KVI4hKeyS0_Lj3EE11update_nodeEPKS1_jjRS2_(%class.KV.2* %old, i32 %count, i32 %i, %class.KV.2* dereferenceable(16) %kv) #2 comdat align 2 {
  %1 = alloca %class.KV.2*, align 8
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca %class.KV.2*, align 8
  %copy = alloca %class.KV.2*, align 8
  store %class.KV.2* %old, %class.KV.2** %1, align 8
  store i32 %count, i32* %2, align 4
  store i32 %i, i32* %3, align 4
  store %class.KV.2* %kv, %class.KV.2** %4, align 8
  %5 = load i32, i32* %2, align 4
  %6 = zext i32 %5 to i64
  %7 = mul i64 %6, 16
  %8 = call noalias i8* @malloc(i64 %7) #8
  %9 = bitcast i8* %8 to %class.KV.2*
  store %class.KV.2* %9, %class.KV.2** %copy, align 8
  %10 = load %class.KV.2*, %class.KV.2** %copy, align 8
  %11 = bitcast %class.KV.2* %10 to i8*
  %12 = load %class.KV.2*, %class.KV.2** %1, align 8
  %13 = bitcast %class.KV.2* %12 to i8*
  %14 = load i32, i32* %2, align 4
  %15 = zext i32 %14 to i64
  %16 = mul i64 %15, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %11, i8* %13, i64 %16, i32 8, i1 false)
  %17 = load %class.KV.2*, %class.KV.2** %copy, align 8
  %18 = load i32, i32* %3, align 4
  %19 = zext i32 %18 to i64
  %20 = getelementptr inbounds %class.KV.2, %class.KV.2* %17, i64 %19
  %21 = bitcast %class.KV.2* %20 to i8*
  %22 = bitcast i8* %21 to %class.KV.2*
  %23 = load %class.KV.2*, %class.KV.2** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2ERKS1_(%class.KV.2* %22, %class.KV.2* dereferenceable(16) %23)
  %24 = load %class.KV.2*, %class.KV.2** %copy, align 8
  ret %class.KV.2* %24
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj3EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.2* noalias sret %agg.result, %class.KV.2* dereferenceable(16) %kv, i64 %h, %class.hKey* %key, %class.hKey* %val, i64* %cptr) #2 comdat align 2 {
  %1 = alloca %class.KV.2*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca %class.hKey*, align 8
  %5 = alloca i64*, align 8
  %data = alloca %class.KV.3*, align 8
  %bm = alloca i64, align 8
  %hpiece = alloca i32, align 4
  %count = alloca i32, align 4
  %i = alloca i32, align 4
  %exists = alloca i8, align 1
  %node = alloca %class.KV.3*, align 8
  %6 = alloca %class.KV.3, align 8
  %childkv = alloca %class.KV.3, align 8
  %node1 = alloca %class.KV.3*, align 8
  %childkv2 = alloca %class.KV.3, align 8
  %node3 = alloca %class.KV.3*, align 8
  %node4 = alloca %class.KV.3*, align 8
  store %class.KV.2* %kv, %class.KV.2** %1, align 8
  store i64 %h, i64* %2, align 8
  store %class.hKey* %key, %class.hKey** %3, align 8
  store %class.hKey* %val, %class.hKey** %4, align 8
  store i64* %cptr, i64** %5, align 8
  %7 = load %class.KV.2*, %class.KV.2** %1, align 8
  %8 = getelementptr inbounds %class.KV.2, %class.KV.2* %7, i32 0, i32 1
  %9 = bitcast %"union.KV<hKey, hKey, 3>::Val"* %8 to %class.KV.3**
  %10 = load %class.KV.3*, %class.KV.3** %9, align 8
  store %class.KV.3* %10, %class.KV.3** %data, align 8
  %11 = load %class.KV.2*, %class.KV.2** %1, align 8
  %12 = getelementptr inbounds %class.KV.2, %class.KV.2* %11, i32 0, i32 0
  %13 = bitcast %"union.KV<hKey, hKey, 3>::Key"* %12 to i64*
  %14 = load i64, i64* %13, align 8
  %15 = lshr i64 %14, 1
  store i64 %15, i64* %bm, align 8
  %16 = load i64, i64* %2, align 8
  %17 = and i64 %16, 63
  %18 = urem i64 %17, 63
  %19 = trunc i64 %18 to i32
  store i32 %19, i32* %hpiece, align 4
  %20 = load i64, i64* %bm, align 8
  %21 = call i64 @llvm.ctpop.i64(i64 %20)
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %count, align 4
  %23 = load i64, i64* %bm, align 8
  %24 = shl i64 %23, 1
  %25 = load i32, i32* %hpiece, align 4
  %26 = sub i32 63, %25
  %27 = zext i32 %26 to i64
  %28 = shl i64 %24, %27
  %29 = call i64 @llvm.ctpop.i64(i64 %28)
  %30 = trunc i64 %29 to i32
  store i32 %30, i32* %i, align 4
  %31 = load i64, i64* %bm, align 8
  %32 = load i32, i32* %hpiece, align 4
  %33 = zext i32 %32 to i64
  %34 = shl i64 1, %33
  %35 = and i64 %31, %34
  %36 = icmp ne i64 %35, 0
  %37 = zext i1 %36 to i8
  store i8 %37, i8* %exists, align 1
  %38 = load i8, i8* %exists, align 1
  %39 = trunc i8 %38 to i1
  br i1 %39, label %40, label %131

; <label>:40                                      ; preds = %0
  %41 = load i32, i32* %i, align 4
  %42 = zext i32 %41 to i64
  %43 = load %class.KV.3*, %class.KV.3** %data, align 8
  %44 = getelementptr inbounds %class.KV.3, %class.KV.3* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.3, %class.KV.3* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<hKey, hKey, 4>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %112

; <label>:50                                      ; preds = %40
  %51 = load i32, i32* %i, align 4
  %52 = zext i32 %51 to i64
  %53 = load %class.KV.3*, %class.KV.3** %data, align 8
  %54 = getelementptr inbounds %class.KV.3, %class.KV.3* %53, i64 %52
  %55 = getelementptr inbounds %class.KV.3, %class.KV.3* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<hKey, hKey, 4>::Key"* %55 to %class.hKey**
  %57 = load %class.hKey*, %class.hKey** %56, align 8
  %58 = load %class.hKey*, %class.hKey** %3, align 8
  %59 = call zeroext i1 @_ZNK4hKeyeqERKS_(%class.hKey* %57, %class.hKey* dereferenceable(8) %58)
  br i1 %59, label %60, label %72

; <label>:60                                      ; preds = %50
  %61 = load %class.KV.3*, %class.KV.3** %data, align 8
  %62 = load i32, i32* %count, align 4
  %63 = load i32, i32* %i, align 4
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  %65 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %6, %class.hKey* %64, %class.hKey* %65)
  %66 = call %class.KV.3* @_ZN2KVI4hKeyS0_Lj4EE11update_nodeEPKS1_jjRS2_(%class.KV.3* %61, i32 %62, i32 %63, %class.KV.3* dereferenceable(16) %6)
  store %class.KV.3* %66, %class.KV.3** %node, align 8
  %67 = load %class.KV.2*, %class.KV.2** %1, align 8
  %68 = getelementptr inbounds %class.KV.2, %class.KV.2* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<hKey, hKey, 3>::Key"* %68 to i64*
  %70 = load i64, i64* %69, align 8
  %71 = load %class.KV.3*, %class.KV.3** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %agg.result, i64 %70, %class.KV.3* %71)
  br label %180

; <label>:72                                      ; preds = %50
  %73 = load i64*, i64** %5, align 8
  %74 = load i64, i64* %73, align 8
  %75 = add i64 %74, 1
  store i64 %75, i64* %73, align 8
  %76 = load i32, i32* %i, align 4
  %77 = zext i32 %76 to i64
  %78 = load %class.KV.3*, %class.KV.3** %data, align 8
  %79 = getelementptr inbounds %class.KV.3, %class.KV.3* %78, i64 %77
  %80 = getelementptr inbounds %class.KV.3, %class.KV.3* %79, i32 0, i32 0
  %81 = bitcast %"union.KV<hKey, hKey, 4>::Key"* %80 to %class.hKey**
  %82 = load %class.hKey*, %class.hKey** %81, align 8
  %83 = call i64 @_ZNK4hKey4hashEv(%class.hKey* %82)
  %84 = lshr i64 %83, 28
  %85 = load i32, i32* %i, align 4
  %86 = zext i32 %85 to i64
  %87 = load %class.KV.3*, %class.KV.3** %data, align 8
  %88 = getelementptr inbounds %class.KV.3, %class.KV.3* %87, i64 %86
  %89 = getelementptr inbounds %class.KV.3, %class.KV.3* %88, i32 0, i32 0
  %90 = bitcast %"union.KV<hKey, hKey, 4>::Key"* %89 to %class.hKey**
  %91 = load %class.hKey*, %class.hKey** %90, align 8
  %92 = load i32, i32* %i, align 4
  %93 = zext i32 %92 to i64
  %94 = load %class.KV.3*, %class.KV.3** %data, align 8
  %95 = getelementptr inbounds %class.KV.3, %class.KV.3* %94, i64 %93
  %96 = getelementptr inbounds %class.KV.3, %class.KV.3* %95, i32 0, i32 1
  %97 = bitcast %"union.KV<hKey, hKey, 4>::Val"* %96 to %class.hKey**
  %98 = load %class.hKey*, %class.hKey** %97, align 8
  %99 = load i64, i64* %2, align 8
  %100 = lshr i64 %99, 6
  %101 = load %class.hKey*, %class.hKey** %3, align 8
  %102 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.3* sret %childkv, i64 %84, %class.hKey* %91, %class.hKey* %98, i64 %100, %class.hKey* %101, %class.hKey* %102)
  %103 = load %class.KV.3*, %class.KV.3** %data, align 8
  %104 = load i32, i32* %count, align 4
  %105 = load i32, i32* %i, align 4
  %106 = call %class.KV.3* @_ZN2KVI4hKeyS0_Lj4EE11update_nodeEPKS1_jjRS2_(%class.KV.3* %103, i32 %104, i32 %105, %class.KV.3* dereferenceable(16) %childkv)
  store %class.KV.3* %106, %class.KV.3** %node1, align 8
  %107 = load %class.KV.2*, %class.KV.2** %1, align 8
  %108 = getelementptr inbounds %class.KV.2, %class.KV.2* %107, i32 0, i32 0
  %109 = bitcast %"union.KV<hKey, hKey, 3>::Key"* %108 to i64*
  %110 = load i64, i64* %109, align 8
  %111 = load %class.KV.3*, %class.KV.3** %node1, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %agg.result, i64 %110, %class.KV.3* %111)
  br label %180

; <label>:112                                     ; preds = %40
  %113 = load i32, i32* %i, align 4
  %114 = zext i32 %113 to i64
  %115 = load %class.KV.3*, %class.KV.3** %data, align 8
  %116 = getelementptr inbounds %class.KV.3, %class.KV.3* %115, i64 %114
  %117 = load i64, i64* %2, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.hKey*, %class.hKey** %3, align 8
  %120 = load %class.hKey*, %class.hKey** %4, align 8
  %121 = load i64*, i64** %5, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.3* sret %childkv2, %class.KV.3* dereferenceable(16) %116, i64 %118, %class.hKey* %119, %class.hKey* %120, i64* %121)
  %122 = load %class.KV.3*, %class.KV.3** %data, align 8
  %123 = load i32, i32* %count, align 4
  %124 = load i32, i32* %i, align 4
  %125 = call %class.KV.3* @_ZN2KVI4hKeyS0_Lj4EE11update_nodeEPKS1_jjRS2_(%class.KV.3* %122, i32 %123, i32 %124, %class.KV.3* dereferenceable(16) %childkv2)
  store %class.KV.3* %125, %class.KV.3** %node3, align 8
  %126 = load %class.KV.2*, %class.KV.2** %1, align 8
  %127 = getelementptr inbounds %class.KV.2, %class.KV.2* %126, i32 0, i32 0
  %128 = bitcast %"union.KV<hKey, hKey, 3>::Key"* %127 to i64*
  %129 = load i64, i64* %128, align 8
  %130 = load %class.KV.3*, %class.KV.3** %node3, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %agg.result, i64 %129, %class.KV.3* %130)
  br label %180

; <label>:131                                     ; preds = %0
  %132 = load i64*, i64** %5, align 8
  %133 = load i64, i64* %132, align 8
  %134 = add i64 %133, 1
  store i64 %134, i64* %132, align 8
  %135 = load i32, i32* %count, align 4
  %136 = add i32 %135, 1
  %137 = zext i32 %136 to i64
  %138 = mul i64 %137, 16
  %139 = call noalias i8* @malloc(i64 %138) #8
  %140 = bitcast i8* %139 to %class.KV.3*
  store %class.KV.3* %140, %class.KV.3** %node4, align 8
  %141 = load %class.KV.3*, %class.KV.3** %node4, align 8
  %142 = bitcast %class.KV.3* %141 to i8*
  %143 = load %class.KV.3*, %class.KV.3** %data, align 8
  %144 = bitcast %class.KV.3* %143 to i8*
  %145 = load i32, i32* %i, align 4
  %146 = zext i32 %145 to i64
  %147 = mul i64 %146, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %142, i8* %144, i64 %147, i32 8, i1 false)
  %148 = load i32, i32* %i, align 4
  %149 = add i32 %148, 1
  %150 = zext i32 %149 to i64
  %151 = load %class.KV.3*, %class.KV.3** %node4, align 8
  %152 = getelementptr inbounds %class.KV.3, %class.KV.3* %151, i64 %150
  %153 = bitcast %class.KV.3* %152 to i8*
  %154 = load i32, i32* %i, align 4
  %155 = zext i32 %154 to i64
  %156 = load %class.KV.3*, %class.KV.3** %data, align 8
  %157 = getelementptr inbounds %class.KV.3, %class.KV.3* %156, i64 %155
  %158 = bitcast %class.KV.3* %157 to i8*
  %159 = load i32, i32* %count, align 4
  %160 = load i32, i32* %i, align 4
  %161 = sub i32 %159, %160
  %162 = zext i32 %161 to i64
  %163 = mul i64 %162, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %153, i8* %158, i64 %163, i32 8, i1 false)
  %164 = load %class.KV.3*, %class.KV.3** %node4, align 8
  %165 = load i32, i32* %i, align 4
  %166 = zext i32 %165 to i64
  %167 = getelementptr inbounds %class.KV.3, %class.KV.3* %164, i64 %166
  %168 = bitcast %class.KV.3* %167 to i8*
  %169 = bitcast i8* %168 to %class.KV.3*
  %170 = load %class.hKey*, %class.hKey** %3, align 8
  %171 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %169, %class.hKey* %170, %class.hKey* %171)
  %172 = load i64, i64* %bm, align 8
  %173 = load i32, i32* %hpiece, align 4
  %174 = zext i32 %173 to i64
  %175 = shl i64 1, %174
  %176 = or i64 %172, %175
  %177 = shl i64 %176, 1
  %178 = or i64 %177, 1
  %179 = load %class.KV.3*, %class.KV.3** %node4, align 8
  call void @_ZN2KVI4hKeyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %agg.result, i64 %178, %class.KV.3* %179)
  br label %180

; <label>:180                                     ; preds = %131, %112, %72, %60
  ret void
}

; Function Attrs: uwtable
define linkonce_odr %class.KV.3* @_ZN2KVI4hKeyS0_Lj4EE11update_nodeEPKS1_jjRS2_(%class.KV.3* %old, i32 %count, i32 %i, %class.KV.3* dereferenceable(16) %kv) #2 comdat align 2 {
  %1 = alloca %class.KV.3*, align 8
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca %class.KV.3*, align 8
  %copy = alloca %class.KV.3*, align 8
  store %class.KV.3* %old, %class.KV.3** %1, align 8
  store i32 %count, i32* %2, align 4
  store i32 %i, i32* %3, align 4
  store %class.KV.3* %kv, %class.KV.3** %4, align 8
  %5 = load i32, i32* %2, align 4
  %6 = zext i32 %5 to i64
  %7 = mul i64 %6, 16
  %8 = call noalias i8* @malloc(i64 %7) #8
  %9 = bitcast i8* %8 to %class.KV.3*
  store %class.KV.3* %9, %class.KV.3** %copy, align 8
  %10 = load %class.KV.3*, %class.KV.3** %copy, align 8
  %11 = bitcast %class.KV.3* %10 to i8*
  %12 = load %class.KV.3*, %class.KV.3** %1, align 8
  %13 = bitcast %class.KV.3* %12 to i8*
  %14 = load i32, i32* %2, align 4
  %15 = zext i32 %14 to i64
  %16 = mul i64 %15, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %11, i8* %13, i64 %16, i32 8, i1 false)
  %17 = load %class.KV.3*, %class.KV.3** %copy, align 8
  %18 = load i32, i32* %3, align 4
  %19 = zext i32 %18 to i64
  %20 = getelementptr inbounds %class.KV.3, %class.KV.3* %17, i64 %19
  %21 = bitcast %class.KV.3* %20 to i8*
  %22 = bitcast i8* %21 to %class.KV.3*
  %23 = load %class.KV.3*, %class.KV.3** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EEC2ERKS1_(%class.KV.3* %22, %class.KV.3* dereferenceable(16) %23)
  %24 = load %class.KV.3*, %class.KV.3** %copy, align 8
  ret %class.KV.3* %24
}

; Function Attrs: uwtable
define linkonce_odr void @_ZN2KVI4hKeyS0_Lj4EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.3* noalias sret %agg.result, %class.KV.3* dereferenceable(16) %kv, i64 %h, %class.hKey* %key, %class.hKey* %val, i64* %cptr) #2 comdat align 2 {
  %1 = alloca %class.KV.3*, align 8
  %2 = alloca i64, align 8
  %3 = alloca %class.hKey*, align 8
  %4 = alloca %class.hKey*, align 8
  %5 = alloca i64*, align 8
  %data = alloca %class.KV.4*, align 8
  %bm = alloca i64, align 8
  %hpiece = alloca i32, align 4
  %count = alloca i32, align 4
  %i = alloca i32, align 4
  %exists = alloca i8, align 1
  %node = alloca %class.KV.4*, align 8
  %6 = alloca %class.KV.4, align 8
  %childkv = alloca %class.KV.4, align 8
  %node1 = alloca %class.KV.4*, align 8
  %childkv2 = alloca %class.KV.4, align 8
  %node3 = alloca %class.KV.4*, align 8
  %node4 = alloca %class.KV.4*, align 8
  store %class.KV.3* %kv, %class.KV.3** %1, align 8
  store i64 %h, i64* %2, align 8
  store %class.hKey* %key, %class.hKey** %3, align 8
  store %class.hKey* %val, %class.hKey** %4, align 8
  store i64* %cptr, i64** %5, align 8
  %7 = load %class.KV.3*, %class.KV.3** %1, align 8
  %8 = getelementptr inbounds %class.KV.3, %class.KV.3* %7, i32 0, i32 1
  %9 = bitcast %"union.KV<hKey, hKey, 4>::Val"* %8 to %class.KV.4**
  %10 = load %class.KV.4*, %class.KV.4** %9, align 8
  store %class.KV.4* %10, %class.KV.4** %data, align 8
  %11 = load %class.KV.3*, %class.KV.3** %1, align 8
  %12 = getelementptr inbounds %class.KV.3, %class.KV.3* %11, i32 0, i32 0
  %13 = bitcast %"union.KV<hKey, hKey, 4>::Key"* %12 to i64*
  %14 = load i64, i64* %13, align 8
  %15 = lshr i64 %14, 1
  store i64 %15, i64* %bm, align 8
  %16 = load i64, i64* %2, align 8
  %17 = and i64 %16, 63
  %18 = urem i64 %17, 63
  %19 = trunc i64 %18 to i32
  store i32 %19, i32* %hpiece, align 4
  %20 = load i64, i64* %bm, align 8
  %21 = call i64 @llvm.ctpop.i64(i64 %20)
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %count, align 4
  %23 = load i64, i64* %bm, align 8
  %24 = shl i64 %23, 1
  %25 = load i32, i32* %hpiece, align 4
  %26 = sub i32 63, %25
  %27 = zext i32 %26 to i64
  %28 = shl i64 %24, %27
  %29 = call i64 @llvm.ctpop.i64(i64 %28)
  %30 = trunc i64 %29 to i32
  store i32 %30, i32* %i, align 4
  %31 = load i64, i64* %bm, align 8
  %32 = load i32, i32* %hpiece, align 4
  %33 = zext i32 %32 to i64
  %34 = shl i64 1, %33
  %35 = and i64 %31, %34
  %36 = icmp ne i64 %35, 0
  %37 = zext i1 %36 to i8
  store i8 %37, i8* %exists, align 1
  %38 = load i8, i8* %exists, align 1
  %39 = trunc i8 %38 to i1
  br i1 %39, label %40, label %131

; <label>:40                                      ; preds = %0
  %41 = load i32, i32* %i, align 4
  %42 = zext i32 %41 to i64
  %43 = load %class.KV.4*, %class.KV.4** %data, align 8
  %44 = getelementptr inbounds %class.KV.4, %class.KV.4* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.4, %class.KV.4* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<hKey, hKey, 5>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %112

; <label>:50                                      ; preds = %40
  %51 = load i32, i32* %i, align 4
  %52 = zext i32 %51 to i64
  %53 = load %class.KV.4*, %class.KV.4** %data, align 8
  %54 = getelementptr inbounds %class.KV.4, %class.KV.4* %53, i64 %52
  %55 = getelementptr inbounds %class.KV.4, %class.KV.4* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<hKey, hKey, 5>::Key"* %55 to %class.hKey**
  %57 = load %class.hKey*, %class.hKey** %56, align 8
  %58 = load %class.hKey*, %class.hKey** %3, align 8
  %59 = call zeroext i1 @_ZNK4hKeyeqERKS_(%class.hKey* %57, %class.hKey* dereferenceable(8) %58)
  br i1 %59, label %60, label %72

; <label>:60                                      ; preds = %50
  %61 = load %class.KV.4*, %class.KV.4** %data, align 8
  %62 = load i32, i32* %count, align 4
  %63 = load i32, i32* %i, align 4
  %64 = load %class.hKey*, %class.hKey** %3, align 8
  %65 = load %class.hKey*, %class.hKey** %4, align 8
  call void @_ZN2KVI4hKeyS0_Lj5EEC2EPKS0_S3_(%class.KV.4* %6, %class.hKey* %64, %class.hKey* %65)
  %66 = call %class.KV.4* @_ZN2KVI4hKeyS0_Lj5EE11update_nodeEPKS1_jjRS2_(%class.KV.4* %61, i32 %62, i32 %63, %class.KV.4* dereferenceable(16) %6)
  store %class.KV.4* %66, %class.KV.4** %node, align 8
  %67 = load %class.KV.3*, %class.KV.3** %1, align 8
  %68 = getelementptr inbounds %class.KV.3, %class.KV.3* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<hKey, hKey, 4>::Key"* %68 to i64*
  %70 = load i64, i64* %69, align 8
  %71 = load %class.KV.4*, %class.KV.4** %node, align 8
  call void @_ZN2KVI4hKeyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3* %agg.result, i64 %70, %class.KV.4* %71)
  br label %180

; <label>:72                                      ; preds = %50
  %73 = load i64*, i64**

;;;;;;

