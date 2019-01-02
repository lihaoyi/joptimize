package joptimize.model

import java.util


/**
  * Represents the combined control/data-flow graph of a SSA program.
  *
  * Note that the [[SSA]] data structure is acyclic; thus in order to model
  * cycles in the graph, we maintain separate lookaside tables [[phiMerges]]
  * and [[regionMerges]] to handle the possibly backwards edges caused by jumps
  * and merges.
  */
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
    val seenSSA = new util.IdentityHashMap[SSA, SSA]()
    val seenControl = new util.IdentityHashMap[SSA.Control, SSA.Control]()
    def recSSA(x: SSA): SSA = {
      if (seenSSA.containsKey(x)) seenSSA.get(x)
      else {
        val res: SSA = x match{
          case phi: SSA.Phi => phi
          case SSA.Arg(index, typeSize) => SSA.Arg(index, typeSize)
          case SSA.BinOp(a, b, opcode) => SSA.BinOp(recSSA(a), recSSA(b), opcode)
          case SSA.UnaOp(a, opcode) => SSA.UnaOp(recSSA(a), opcode)
          case SSA.UnaBranch(ctrl, a, opcode) => SSA.UnaBranch(recControl(ctrl), recSSA(a), opcode)
          case SSA.BinBranch(ctrl, a, b, opcode) =>
            SSA.BinBranch(recControl(ctrl), recSSA(a), recSSA(b), opcode)
          case SSA.ReturnVal(ctrl, a) => SSA.ReturnVal(recControl(ctrl), recSSA(a))
          case SSA.Return(ctrl) => SSA.Return(recControl(ctrl))
          case SSA.AThrow(src) => SSA.AThrow(recSSA(src))
          case SSA.TableSwitch(src, min, max) => SSA.TableSwitch(recSSA(src), min, max)
          case SSA.LookupSwitch(src, keys) => SSA.LookupSwitch(recSSA(src), keys)
          case SSA.CheckCast(src, desc) => SSA.CheckCast(recSSA(src), desc)
          case SSA.ArrayLength(src) => SSA.ArrayLength(recSSA(src))
          case SSA.InstanceOf(src, desc) => SSA.InstanceOf(recSSA(src), desc)
          case SSA.PushI(value) => SSA.PushI(value)
          case SSA.PushJ(value) => SSA.PushJ(value)
          case SSA.PushF(value) => SSA.PushF(value)
          case SSA.PushD(value) => SSA.PushD(value)
          case SSA.PushS(value) => SSA.PushS(value)
          case SSA.PushNull() => SSA.PushNull()
          case SSA.PushCls(value) => SSA.PushCls(value)
          case SSA.InvokeStatic(srcs, cls, name, desc) => SSA.InvokeStatic(srcs.map(recSSA), cls, name, desc)
          case SSA.InvokeSpecial(srcs, cls, name, desc) => SSA.InvokeSpecial(srcs.map(recSSA), cls, name, desc)
          case SSA.InvokeVirtual(srcs, cls, name, desc) => SSA.InvokeVirtual(srcs.map(recSSA), cls, name, desc)
          case SSA.InvokeDynamic(name, desc, bsTag, bsOwner, bsName, bsDesc, bsArgs) => SSA.InvokeDynamic(name, desc, bsTag, bsOwner, bsName, bsDesc, bsArgs)
          case SSA.NewArray(src, typeRef) => SSA.NewArray(recSSA(src), typeRef)
          case SSA.MultiANewArray(desc, dims) => SSA.MultiANewArray(desc, dims)
          case SSA.PutStatic(src, cls, name, desc) => SSA.PutStatic(recSSA(src), cls, name, desc)
          case SSA.GetStatic(cls, name, desc) => SSA.GetStatic(cls, name, desc)
          case SSA.PutField(src, obj, owner, name, desc) => SSA.PutField(recSSA(src), recSSA(obj), owner, name, desc)
          case SSA.GetField(obj, owner, name, desc) => SSA.GetField(recSSA(obj), owner, name, desc)
          case SSA.PutArray(src, indexSrc, array) => SSA.PutArray(recSSA(src), recSSA(indexSrc), recSSA(array))
          case SSA.GetArray(indexSrc, array, typeSize) => SSA.GetArray(recSSA(indexSrc), recSSA(array), typeSize)
          case SSA.MonitorEnter(indexSrc) => SSA.MonitorEnter(indexSrc)
          case SSA.MonitorExit(indexSrc) => SSA.MonitorExit(indexSrc)
        }
        val res2 = if (onValue.isDefinedAt(res)) recSSA(onValue(res)) else res
        seenSSA.put(x, res2)
        res2
      }
    }
    def recControl(x: SSA.Control): SSA.Control = {
      val res = x match{
        case r: SSA.Region => r
        case SSA.True(inner) => SSA.True(recSSA(inner).asInstanceOf[SSA.Controlled])
        case SSA.False(inner) => SSA.False(recSSA(inner).asInstanceOf[SSA.Controlled])
      }

      val res2 = if (onControl.isDefinedAt(res)) recControl(onControl(res)) else res
      seenControl.put(x, res2)
      res2
    }
    Program(
      allTerminals.map(recSSA),
      phiMerges.map{case (k, v) => (k, v.map(x => (recControl(x._1), recSSA(x._2))))},
      regionMerges.map{case (k, v) => (k, v.map(recControl))}
    )
  }
}