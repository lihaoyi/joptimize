package joptimize
import utest._
import MainTestUtils.annotatedTest
object MainTests extends TestSuite{
  def tests = Tests{
    'simple - {
      'Hello - {
        'incrI - annotatedTest
        'incrJ - annotatedTest
        'incrS - annotatedTest
        'incrF - annotatedTest
        'incrD - annotatedTest
        'addI - annotatedTest
        'addJ - annotatedTest
        'addS - annotatedTest
        'addF - annotatedTest
        'addD - annotatedTest
        'localI - annotatedTest
        'localJ - annotatedTest
        'localS - annotatedTest
        'localF - annotatedTest
        'localD - annotatedTest
      }
      'IfElse - {
        'basicIf - annotatedTest
        'ifAssign - annotatedTest
        'ifNonIntZero - annotatedTest
        'ifNonIntBinary - annotatedTest
        'ifElseIf - annotatedTest
        'ifElseIfBig - annotatedTest
      }
      'Loops - {
        'basicFor - annotatedTest
        'liftableFor - annotatedTest
        'unliftableFor - annotatedTest
        'mixedLiftableFor - annotatedTest
        'nestedLiftableFor - annotatedTest
        'nestedLiftableFor2 - annotatedTest
        'nestedUnliftableFor - annotatedTest
        'nestedMixedLiftableFor - annotatedTest
        'nestedFor - annotatedTest
        'nestedFor2 - annotatedTest
        'basicWhile - annotatedTest
        'alternatingWhile - annotatedTest
        'loopReturn1 - annotatedTest
        'loopReturn2 - annotatedTest
        'loopReturn3 - annotatedTest
        'sqrtFinder - annotatedTest
        'twoLoops - annotatedTest
      }

      'Switches - {
        'smallSwitch - annotatedTest
        'smallSwitchNoDefault - annotatedTest
        'bigDenseSwitch - annotatedTest
        'bigSparseSwitch - annotatedTest
        'charSwitch - annotatedTest
        'byteSwitch - annotatedTest
        'stringSwitch - annotatedTest
        'stringSwitchTwo - annotatedTest
      }
      'Statics - {
        'helloWorld - annotatedTest
        'timesTwo - annotatedTest
        'helloWorld2 - annotatedTest
        'timesTwo2 - annotatedTest
        'call - annotatedTest
        'callAtPhiBoundary - annotatedTest
        'voidCall - annotatedTest
        'tailFactorial - annotatedTest
        'tailFactorialLong - annotatedTest
        'tailFactorialVoid - annotatedTest
        'fibonacci - annotatedTest
        'staticInit - annotatedTest
        'regression - annotatedTest

      }
      'Arrays - {
        'length - annotatedTest
        'get1 - annotatedTest
        'get2 - annotatedTest
        'get3 - annotatedTest
        'get4 - annotatedTest
        'get5 - annotatedTest
        'set1 - annotatedTest
        'set2 - annotatedTest
        'set3 - annotatedTest
        'setIf - annotatedTest
        'getAndSet1 - annotatedTest
        'getAndSet2 - annotatedTest
        'getAndSet3 - annotatedTest
        'getAndSetLoop1 - annotatedTest
        'getAndSetLoop2 - annotatedTest
        'getAndSetLoop3 - annotatedTest
      }
//      'InvokeDynamic - {
//        'lambda - annotatedTest
//        'lambdaArg - annotatedTest
//        'lambdaBoxed - annotatedTest
//        'lambdaBoxedArgs - annotatedTest
//        'concat - annotatedTest
//      }
//      'ScalaInvokeDynamic - {
//        'hello - annotatedTest
//        'hello2 - annotatedTest
//        'lambda - annotatedTest
//        'lambdaBoxed - annotatedTest
//        'concat - annotatedTest
//      }
      'Inheritance - {
        'hello - annotatedTest
        'strings - annotatedTest
        'implement - annotatedTest
        'abstractClass - annotatedTest
        'shadowedInheritedGet - annotatedTest
        'shadowedInheritedSet - annotatedTest
        'superMethod - annotatedTest
        'staticInheritance - annotatedTest
        'staticInheritanceMethod - annotatedTest
        'moreStrings - annotatedTest
      }
//      'Exceptions - {
//        'throwCatch0 - annotatedTest
//        'throwCatch1 - annotatedTest
//        'throwCatch2 - annotatedTest
//        'throwCatch3 - annotatedTest
//        'throwCatch4 - annotatedTest
//        'multiCatch - annotatedTest
//        'nullPointer - annotatedTest
//        'arrayIndexOutOfBounds - annotatedTest
//      }
      'Sudoku - {
        'quick - annotatedTest
        'quick2 - annotatedTest
//        'run - annotatedTest
      }
    }
    'opt - {
      'Folding - {
        'iadd - annotatedTest
        'isub - annotatedTest
        'imul - annotatedTest
        'idiv - annotatedTest
        'irem - annotatedTest
        'ishl - annotatedTest
        'ishr - annotatedTest
        'iushr - annotatedTest
        'iand - annotatedTest
        'ior - annotatedTest
        'ixor - annotatedTest

        'jadd - annotatedTest
        'jsub - annotatedTest
        'jmul - annotatedTest
        'jdiv - annotatedTest
        'jrem - annotatedTest
        'jshl - annotatedTest
        'jshr - annotatedTest
        'jushr - annotatedTest
        'jand - annotatedTest
        'jor - annotatedTest
        'jxor - annotatedTest

        'fadd - annotatedTest
        'fsub - annotatedTest
        'fmul - annotatedTest
        'fdiv - annotatedTest
        'frem - annotatedTest

        'dadd - annotatedTest
        'dsub - annotatedTest
        'dmul - annotatedTest
        'ddiv - annotatedTest
        'drem - annotatedTest

        'lcmp0 - annotatedTest
        'lcmp1 - annotatedTest
        'lcmp2 - annotatedTest

        'fcmp0 - annotatedTest
        'fcmp1 - annotatedTest
        'fcmp2 - annotatedTest

        'dcmp0 - annotatedTest
        'dcmp1 - annotatedTest
        'dcmp2 - annotatedTest

        'jump0 - annotatedTest
        'jump1 - annotatedTest
        'jump2 - annotatedTest
        'jump3 - annotatedTest

        'switch0 - annotatedTest
        'switch1 - annotatedTest
        'switch2 - annotatedTest
        'switch3 - annotatedTest
      }
      'SimpleDce - {
        'main - annotatedTest
        'deadLoopVariable - annotatedTest
        'deadLocal - annotatedTest
      }
      'Optimistic - {
        'trivial - annotatedTest
        'loopConstant - annotatedTest
        'branchConstant - annotatedTest
        'recursivePureConstant - annotatedTest
        'mutualRecursivePureConstant - annotatedTest
        'generalRecursive - annotatedTest

        'implementLate1 - annotatedTest
        'implementLate2 - annotatedTest
        'implementLate3 - annotatedTest
        'implementLate4 - annotatedTest
        // These tests cannot pass until we have optimistic inter-procedural
        // liveness analysis, rather than the simple intra-procedural liveness
        // analysis that we have now
        //        'recursivePureDeadArg - annotatedTest
        //        'mutualRecursivePureDeadArg - annotatedTest
      }
      'ConstantMethod - {
        'intMain0 - annotatedTest
        'intMain - annotatedTest
        'nonFoldedIntMain - annotatedTest
        'boolMain - annotatedTest
        'impureMain - annotatedTest
      }
      'BooleanJumpFlatten - {
        'simpleTrue - annotatedTest
        'simpleFalse - annotatedTest
      }

      'InstanceofJumpFlatten - {
        'simpleBar - annotatedTest
        'simpleBaz - annotatedTest
        'simpleQux - annotatedTest
        'simpleBarMatch - annotatedTest
        'simpleBazMatch - annotatedTest
        'simpleQuxMatch - annotatedTest
      }
      'BooleanWidening - {
        'simple - annotatedTest
      }
      'InstanceDce - {
        'simple1 - annotatedTest
        'simple2 - annotatedTest
        'simple3 - annotatedTest

        'single1 - annotatedTest
        'single2 - annotatedTest
        'single3 - annotatedTest
        'single4 - annotatedTest

//        'unknown1 - annotatedTest
//        'unknown2 - annotatedTest
//        'unknown3 - annotatedTest
      }
      'InterfacePreservation - {
        'shallow - annotatedTest
        'deep - annotatedTest
      }


      'Liveness - {
        'entrypointUnused0 - annotatedTest
        'entrypointUnused1 - annotatedTest
        'entrypointUnused2 - annotatedTest

        'trivialUnused - annotatedTest
        'longUnusedRight - annotatedTest
        'longUnusedLeft - annotatedTest

        'simple - annotatedTest
        'simple2a - annotatedTest
        'simple2b - annotatedTest

        'chained - annotatedTest
        'chained2a - annotatedTest
        'chained2b - annotatedTest

        'implement0 - annotatedTest
        'implement1 - annotatedTest
        'implement2a - annotatedTest
        'implement2b - annotatedTest
        'implement3 - annotatedTest

        'override0 - annotatedTest
        'override1 - annotatedTest
        'override2 - annotatedTest
        'override3 - annotatedTest
        'override4 - annotatedTest

        'deadLoopCounter - annotatedTest
      }

    }

    'narrow - {
      'Supertype - {
        'main - annotatedTest
        'mainDeep - annotatedTest
      }
      'Parametric - {
        'main - annotatedTest
        'mainDeep - annotatedTest
      }
      'NarrowReturn - {
        'main - annotatedTest
      }
      'MergeReturn - {
        'main - annotatedTest
      }
//      'IntersectionReturn - {
//        'main - annotatedTest
//      }
      'ForceWide - {
        'main - annotatedTest
      }
    }
  }
}
