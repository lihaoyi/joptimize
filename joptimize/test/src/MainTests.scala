package test
import utest._
import MainTestUtils.annotatedTest
object MainTests extends TestSuite {
  def tests = Tests {
    test("simple"){
      test("Hello"){
        test("incrI") - annotatedTest
        test("incrJ") - annotatedTest
        test("incrS") - annotatedTest
        test("incrF") - annotatedTest
        test("incrD") - annotatedTest
        test("addI") - annotatedTest
        test("addJ") - annotatedTest
        test("addS") - annotatedTest
        test("addF") - annotatedTest
        test("addD") - annotatedTest
        test("localI") - annotatedTest
        test("localJ") - annotatedTest
        test("localS") - annotatedTest
        test("localF") - annotatedTest
        test("localD") - annotatedTest
        test("overrideArg1") - annotatedTest
        test("overrideArg2") - annotatedTest
        test("overrideArg3") - annotatedTest
        test("overrideArg4") - annotatedTest
        test("overrideArg5") - annotatedTest
        test("overrideArg6") - annotatedTest
      }
      test("IfElse"){
        test("basicIf") - annotatedTest
        test("ifAssign") - annotatedTest
        test("ifNonIntZero") - annotatedTest
        test("ifNonIntBinary") - annotatedTest
        test("ifElseIf") - annotatedTest
        test("ifElseIfBig") - annotatedTest
      }
      test("Loops"){
        test("basicFor") - annotatedTest
        test("liftableFor") - annotatedTest
        test("unliftableFor") - annotatedTest
        test("mixedLiftableFor") - annotatedTest
        test("nestedLiftableFor") - annotatedTest
        test("nestedLiftableFor2") - annotatedTest
        test("nestedUnliftableFor") - annotatedTest
        test("nestedMixedLiftableFor") - annotatedTest
        test("nestedFor") - annotatedTest
        test("nestedFor2") - annotatedTest
        test("basicWhile") - annotatedTest
        test("alternatingWhile") - annotatedTest
        test("loopReturn1") - annotatedTest
        test("loopReturn2") - annotatedTest
        test("loopReturn3") - annotatedTest
        test("sqrtFinder") - annotatedTest
        test("twoLoops") - annotatedTest
      }

      test("Switches"){
        test("smallSwitch") - annotatedTest
        test("smallSwitchNoDefault") - annotatedTest
        test("bigDenseSwitch") - annotatedTest
        test("bigSparseSwitch") - annotatedTest
        test("charSwitch") - annotatedTest
        test("byteSwitch") - annotatedTest
        test("stringSwitch") - annotatedTest
        test("stringSwitchTwo") - annotatedTest
      }

      test("Arrays"){
        test("length") - annotatedTest
        test("get1") - annotatedTest
        test("get2") - annotatedTest
        test("get3") - annotatedTest
        test("get4") - annotatedTest
        test("get5") - annotatedTest
        test("set1") - annotatedTest
        test("set2") - annotatedTest
        test("set3") - annotatedTest
        test("setIf") - annotatedTest
        test("getAndSet1") - annotatedTest
        test("getAndSet2") - annotatedTest
        test("getAndSet3") - annotatedTest
        test("getAndSetLoop1") - annotatedTest
        test("getAndSetLoop2") - annotatedTest
        test("getAndSetLoop3") - annotatedTest
      }
      test("Statics"){
        test("helloWorld") - annotatedTest
        test("timesTwo") - annotatedTest
        test("helloWorld2") - annotatedTest
        test("timesTwo2") - annotatedTest
        test("call") - annotatedTest
        test("callAtPhiBoundary") - annotatedTest
        test("voidCall") - annotatedTest
        test("tailFactorial") - annotatedTest
        test("tailFactorialLong") - annotatedTest
        test("tailFactorialVoid") - annotatedTest
        test("fibonacci") - annotatedTest
        test("staticInit") - annotatedTest
        test("regression") - annotatedTest

      }
      test("InvokeDynamic"){
//        test("lambda") - annotatedTest
//        test("lambdaArg") - annotatedTest
//        test("lambdaBoxed") - annotatedTest
//        test("lambdaBoxedArgs") - annotatedTest
        test("concat") - annotatedTest
      }
      test("ScalaInvokeDynamic"){
        test("hello") - annotatedTest
        test("hello2") - annotatedTest
        test("lambda") - annotatedTest
        test("lambdaSpecialized") - annotatedTest
        test("lambdaBoxed") - annotatedTest
        test("concat") - annotatedTest
      }
      test("Inheritance"){
        test("hello") - annotatedTest
        test("strings") - annotatedTest
        test("implement") - annotatedTest
        test("abstractClass") - annotatedTest
        test("shadowedInheritedGet") - annotatedTest
        test("shadowedInheritedSet") - annotatedTest
        test("superMethod") - annotatedTest
        test("staticInheritance") - annotatedTest
        test("staticInheritanceMethod") - annotatedTest
        test("moreStrings") - annotatedTest
        test("lambdaSpecialized0") - annotatedTest
        test("lambdaSpecialized") - annotatedTest
      }
//      test("Exceptions"){
//        test("throwCatch0") - annotatedTest
//        test("throwCatch1") - annotatedTest
//        test("throwCatch2") - annotatedTest
//        test("throwCatch3") - annotatedTest
//        test("throwCatch4") - annotatedTest
//        test("multiCatch") - annotatedTest
//        test("nullPointer") - annotatedTest
//        test("arrayIndexOutOfBounds") - annotatedTest
//      }
      test("Sudoku"){
        test("quick") - annotatedTest
        test("quick2") - annotatedTest
        test("quick3") - annotatedTest
//        test("run") - annotatedTest
      }
    }
    test("simplify"){
      test("Folding"){
        test("iadd") - annotatedTest
        test("isub") - annotatedTest
        test("imul") - annotatedTest
        test("idiv") - annotatedTest
        test("irem") - annotatedTest
        test("ishl") - annotatedTest
        test("ishr") - annotatedTest
        test("iushr") - annotatedTest
        test("iand") - annotatedTest
        test("ior") - annotatedTest
        test("ixor") - annotatedTest

        test("jadd") - annotatedTest
        test("jsub") - annotatedTest
        test("jmul") - annotatedTest
        test("jdiv") - annotatedTest
        test("jrem") - annotatedTest
        test("jshl") - annotatedTest
        test("jshr") - annotatedTest
        test("jushr") - annotatedTest
        test("jand") - annotatedTest
        test("jor") - annotatedTest
        test("jxor") - annotatedTest

        test("fadd") - annotatedTest
        test("fsub") - annotatedTest
        test("fmul") - annotatedTest
        test("fdiv") - annotatedTest
        test("frem") - annotatedTest

        test("dadd") - annotatedTest
        test("dsub") - annotatedTest
        test("dmul") - annotatedTest
        test("ddiv") - annotatedTest
        test("drem") - annotatedTest

        test("lcmp0") - annotatedTest
        test("lcmp1") - annotatedTest
        test("lcmp2") - annotatedTest

        test("fcmp0") - annotatedTest
        test("fcmp1") - annotatedTest
        test("fcmp2") - annotatedTest

        test("dcmp0") - annotatedTest
        test("dcmp1") - annotatedTest
        test("dcmp2") - annotatedTest

        test("jump0") - annotatedTest
        test("jump1") - annotatedTest
        test("jump2") - annotatedTest
        test("jump3") - annotatedTest

        test("switch0") - annotatedTest
        test("switch1") - annotatedTest
        test("switch2") - annotatedTest
        test("switch3") - annotatedTest
      }
      test("SimpleDce"){
        test("main") - annotatedTest
        test("deadLoopVariable") - annotatedTest
        test("deadLocal") - annotatedTest
      }
      test("ConstantMethod"){
        test("intMain0") - annotatedTest
        test("intMain") - annotatedTest
        test("nonFoldedIntMain") - annotatedTest
        test("boolMain") - annotatedTest
        test("impureMain") - annotatedTest
      }
      test("BooleanJumpFlatten"){
        test("simpleTrue") - annotatedTest
        test("simpleFalse") - annotatedTest
        test("nested1") - annotatedTest
        test("nested2") - annotatedTest
        test("nested3") - annotatedTest
        test("nested4") - annotatedTest
      }

      test("InstanceofJumpFlatten"){
        test("simpleBar") - annotatedTest
        test("simpleBaz") - annotatedTest
        test("simpleQux") - annotatedTest
        test("simpleBarMatch") - annotatedTest
        test("simpleBazMatch") - annotatedTest
        test("simpleQuxMatch") - annotatedTest
      }
      test("BooleanWidening"){
        test("simple") - annotatedTest
      }
      test("InstanceDce"){
        test("simple1") - annotatedTest
        test("simple2") - annotatedTest
        test("simple3") - annotatedTest

        test("single1") - annotatedTest
        test("single2") - annotatedTest
        test("single3") - annotatedTest
        test("single4") - annotatedTest

        test("unknown1") - annotatedTest
        test("unknown2") - annotatedTest
        test("unknown3") - annotatedTest
      }
      test("InterfacePreservation"){
        test("shallow") - annotatedTest
        test("deep") - annotatedTest
      }

      test("Liveness"){
        test("entrypointUnused0") - annotatedTest
        test("entrypointUnused1") - annotatedTest
        test("entrypointUnused2") - annotatedTest

        test("trivialUnused") - annotatedTest
        test("longUnusedRight") - annotatedTest
        test("longUnusedLeft") - annotatedTest

        test("simple") - annotatedTest
        test("simple2a") - annotatedTest
        test("simple2b") - annotatedTest

        test("chained") - annotatedTest
        test("chained2a") - annotatedTest
        test("chained2b") - annotatedTest

        test("implement0") - annotatedTest
        test("implement1") - annotatedTest
        test("implement2a") - annotatedTest
        test("implement2b") - annotatedTest
        test("implement3") - annotatedTest

        test("override0") - annotatedTest
        test("override1") - annotatedTest
        test("override2") - annotatedTest
        test("override3") - annotatedTest
        test("override4") - annotatedTest

        test("deadLoopCounter") - annotatedTest
      }

    }

    test("narrow"){
      test("Supertype"){
        test("main") - annotatedTest
        test("mainDeep") - annotatedTest
      }
      test("Parametric"){
        test("main") - annotatedTest
        test("mainDeep") - annotatedTest
      }
      test("NarrowReturn"){
        test("main") - annotatedTest
      }
      test("MergeReturn"){
        test("main") - annotatedTest
      }
//      test("IntersectionReturn"){
//        test("main") - annotatedTest
//      }
      test("ForceWide"){
        test("main") - annotatedTest
      }
    }
    test("optimistic"){
      test("Simple"){
        test("trivial") - annotatedTest
        test("loopConstant") - annotatedTest
        test("branchConstant") - annotatedTest
      }
      test("Recursive"){
        test("recursivePureConstant") - annotatedTest
        test("mutualRecursivePureConstant") - annotatedTest
        test("generalRecursive") - annotatedTest
      }
      test("ClassLoading"){
        test("monomorphicOptimize") - annotatedTest
        test("preBimorphicUnoptimize") - annotatedTest
        test("postBimorphicDeoptimize") - annotatedTest
        test("prePostBimorphicDeoptimize") - annotatedTest
        // These tests cannot pass until we have optimistic inter-procedural
        // liveness analysis, rather than the simple intra-procedural liveness
        // analysis that we have now
        //        test("recursivePureDeadArg") - annotatedTest
        //        test("mutualRecursivePureDeadArg") - annotatedTest
      }

    }

    test("scalalib"){
      test("Small"){
        test("equals") - annotatedTest
        test("arraycopy") - annotatedTest
        test("arraysCopyOf") - annotatedTest
        test("createArrayInStaticInitializer1") - annotatedTest
        test("createArrayInStaticInitializer2") - annotatedTest
        test("createArrayInStaticInitializer3") - annotatedTest
        test("createNestedArrayInStaticInitializer") - annotatedTest
        test("loopAtStartOfMethod") - annotatedTest
      }

      test("Throwing"){
        test("tryCatch") - annotatedTest
        test("lazyVal") - annotatedTest
        test("throwingInBranch1") - annotatedTest
        test("throwingInBranch2") - annotatedTest
      }
      test("Methods"){
        test("multipleSubtypesOfGeneric") - annotatedTest
        test("multipleTypesOfGeneric") - annotatedTest
        test("superDefineSubImplement") - annotatedTest
        test("narrowingAbstractMethod") - annotatedTest
        test("inferredHighestDefinerReturnDiffer") - annotatedTest
        test("inheritFromOutsideHierarchy") - annotatedTest
      }
      test("FauxLibrary"){

        test("testManifestFactory") - annotatedTest

        test("simpleLinkedListForeach") - annotatedTest
        test("simpleArraySeqForeach") - annotatedTest
        test("minimizedIterator") - annotatedTest
        test("staticSpecialInterfaceMethods") - annotatedTest

        test("manualIterator") - annotatedTest
        test("manualIterator2") - annotatedTest

        test("manualIterator3") - annotatedTest
      }
      test("Library"){
        test("arrayBuilderOfInt") - annotatedTest
        test("touchManifestFactory") - annotatedTest
        test("touchScalaPackage") - annotatedTest
      }
    }
    test("inlining"){

      test("Inlining"){
        test("simple") - annotatedTest
        test("conditional") - annotatedTest
        test("conditionalIf") - annotatedTest
        test("conditionalVoid") - annotatedTest
        test("conditionalIfVoid") - annotatedTest
        test("thrower") - annotatedTest
        test("throwerComplex") - annotatedTest
        test("throwerPartialMerge") - annotatedTest
        test("throwerFullMerge") - annotatedTest
      }
      test("Chaining"){
        test("noneGet") - annotatedTest
        test("someGet") - annotatedTest
        test("castSomeGet") - annotatedTest
        test("get") - annotatedTest
        test("map") - annotatedTest
        test("mapInnerClass") - annotatedTest
        test("flatMap") - annotatedTest
        test("flatMapMapSimple") - annotatedTest
        test("flatMapMap") - annotatedTest
        test("mapTwice") - annotatedTest
        test("mapTwice2") - annotatedTest
        test("mapTwice3") - annotatedTest
        test("flatMapMapChain") - annotatedTest
      }
      test("Demo"){


        test("mainA") - annotatedTest

      }
    }
  }
}
