package joptimize.model

import java.util


case class Program(allTerminals: Seq[SSA],
                   phiMerges:  Map[SSA.Phi, Set[(SSA.Control, SSA)]],
                   regionMerges: Map[SSA.Region, Set[SSA.Control]]){
  /**
    * Transforms this program by trying to apply a callback on every node.
    *
    * If the callback is successfully applied, it is applied repeatedly until
    * it no longer matches the node
    */
  def transform(onValue: PartialFunction[SSA, SSA] = PartialFunction.empty,
                onControl: PartialFunction[SSA.Control, SSA.Control] = PartialFunction.empty): Program = {
    val current = new util.IdentityHashMap[SSA, SSA]()
    val current2 = new util.IdentityHashMap[SSA.Control, SSA.Control]()
    def rec(x: SSA): SSA = {
      if (current.containsKey(x)) current.get(x)
      else {
        val res: SSA = x match{
          case phi: SSA.Phi => phi
          case SSA.Arg(index, typeSize) => SSA.Arg(index, typeSize)
          case SSA.BinOp(a, b, opcode) => SSA.BinOp(rec(a), rec(b), opcode)
          case SSA.UnaOp(a, opcode) => SSA.UnaOp(rec(a), opcode)
          case SSA.UnaBranch(ctrl, a, opcode) => SSA.UnaBranch(rec2(ctrl), rec(a), opcode)
          case SSA.BinBranch(ctrl, a, b, opcode) =>
            a match{
              case p: SSA.Phi => pprint.log(p -> phiMerges(p))
              case _ =>
            }
            b match{
              case p: SSA.Phi => pprint.log(p -> phiMerges(p))
              case _ =>
            }
            SSA.BinBranch(rec2(ctrl), rec(a), rec(b), opcode)
          case SSA.ReturnVal(ctrl, a) => SSA.ReturnVal(rec2(ctrl), rec(a))
          case SSA.Return(ctrl) => SSA.Return(rec2(ctrl))
          case SSA.AThrow(src) => SSA.AThrow(rec(src))
          case SSA.TableSwitch(src, min, max) => SSA.TableSwitch(rec(src), min, max)
          case SSA.LookupSwitch(src, keys) => SSA.LookupSwitch(rec(src), keys)
          case SSA.CheckCast(src, desc) => SSA.CheckCast(rec(src), desc)
          case SSA.ArrayLength(src) => SSA.ArrayLength(rec(src))
          case SSA.InstanceOf(src, desc) => SSA.InstanceOf(rec(src), desc)
          case SSA.PushI(value) => SSA.PushI(value)
          case SSA.PushJ(value) => SSA.PushJ(value)
          case SSA.PushF(value) => SSA.PushF(value)
          case SSA.PushD(value) => SSA.PushD(value)
          case SSA.PushS(value) => SSA.PushS(value)
          case SSA.PushNull() => SSA.PushNull()
          case SSA.PushCls(value) => SSA.PushCls(value)
          case SSA.InvokeStatic(srcs, cls, name, desc) => SSA.InvokeStatic(srcs.map(rec), cls, name, desc)
          case SSA.InvokeSpecial(srcs, cls, name, desc) => SSA.InvokeSpecial(srcs.map(rec), cls, name, desc)
          case SSA.InvokeVirtual(srcs, cls, name, desc) => SSA.InvokeVirtual(srcs.map(rec), cls, name, desc)
          case SSA.InvokeDynamic(name, desc, bsTag, bsOwner, bsName, bsDesc, bsArgs) => SSA.InvokeDynamic(name, desc, bsTag, bsOwner, bsName, bsDesc, bsArgs)
          case SSA.NewArray(src, typeRef) => SSA.NewArray(rec(src), typeRef)
          case SSA.MultiANewArray(desc, dims) => SSA.MultiANewArray(desc, dims)
          case SSA.PutStatic(src, cls, name, desc) => SSA.PutStatic(rec(src), cls, name, desc)
          case SSA.GetStatic(cls, name, desc) => SSA.GetStatic(cls, name, desc)
          case SSA.PutField(src, obj, owner, name, desc) => SSA.PutField(rec(src), rec(obj), owner, name, desc)
          case SSA.GetField(obj, owner, name, desc) => SSA.GetField(rec(obj), owner, name, desc)
          case SSA.PutArray(src, indexSrc, array) => SSA.PutArray(rec(src), rec(indexSrc), rec(array))
          case SSA.GetArray(indexSrc, array, typeSize) => SSA.GetArray(rec(indexSrc), rec(array), typeSize)
          case SSA.MonitorEnter(indexSrc) => SSA.MonitorEnter(indexSrc)
          case SSA.MonitorExit(indexSrc) => SSA.MonitorExit(indexSrc)
        }
        val res2 = if (onValue.isDefinedAt(res)) rec(onValue(res)) else res
        current.put(x, res2)
        res2
      }
    }
    def rec2(x: SSA.Control): SSA.Control = {
      val res = x match{
        case r: SSA.Region => r
        case SSA.True(inner) => SSA.True(rec(inner))
        case SSA.False(inner) => SSA.False(rec(inner))
      }

      val res2 = if (onControl.isDefinedAt(res)) rec2(onControl(res)) else res
      current2.put(x, res2)
      res2
    }
    Program(
      allTerminals.map(rec),
      phiMerges.map{case (k, v) => (k, v.map(x => (rec2(x._1), rec(x._2))))},
      regionMerges.map{case (k, v) => (k, v.map(rec2))}
    )
  }
}