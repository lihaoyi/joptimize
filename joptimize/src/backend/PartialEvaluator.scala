package joptimize.backend

import joptimize.Logger
import joptimize.model.SSA

object PartialEvaluator {

  def replaceJump(branch: SSA.Jump,
                  directNext: SSA.Block,
                  liveBlocks: SSA.Block => Boolean,
                  log: Logger.InferredMethod) = {
    //                     c
    //                    /
    //       a        TRUE -- d
    //        \      /    \     \
    // block - branch      ---- phi
    //        /      \          / |
    //       b        false ---  |
    //                     \     /
    //                      e----

    directNext.next.replaceUpstream(directNext, branch.block)
    for(phi <- directNext.nextPhis){
      phi.replaceUpstream(directNext, branch.block)
      branch.block.nextPhis ++= Seq(phi)
    }

    for(up <- branch.upstreamVals){
      up.downstreamRemoveAll(branch)
    }
    //       --------------------
    //      /      a        TRUE \--- d
    //     /        \      /      \    \
    // block ------- branch        --- phi
    //     \        /      \          / |
    //      c      b        false ----  |
    //                           \      /
    //                            e ----
    branch.block.next = directNext.next
    //       --------------------
    //      /      a        TRUE \--- d
    //     /        \      /      \    \
    // block         branch        -- phi
    //     \        /      \          / |
    //      c      b        false ----  |
    //                           \      /
    //                            e ----
    val branchBlocks = branch.downstreamList.toSet

    val upState = branch.state
    val downState = directNext.nextState
    upState.next = downState
    downState.parent = upState

    branchBlocks.flatMap(_.downstreamList).collect {
      case phi: SSA.Phi =>
        phi.incoming = phi.incoming.flatMap(x =>
          if (x._1 == directNext) Some(branch.block -> x._2)
          else if (branchBlocks(x._1)) {
            x._1.next = null
            x._2.downstreamRemove(phi)
            None
          }
          else {
            Some(x)
          }
        )
        phi

      case r: SSA.Merge =>
        r.incoming = r.incoming.flatMap { x =>
          if (x._1 == directNext) Some(branch.block -> branch.block.nextState)
          else if (branchBlocks(x._1)) {
            x._1.next = null
            None
          }
          else Some(x)
        }
        r
    }
    //       ---------------------
    //      /      a        TRUE  \-- d
    //     /        \      /       \    \
    // block         branch         --- phi
    //     \        /      \
    //      c      b        false - e
    //
    //      d --- phi
    //     /     /
    // block -----
    //     \
    //      c
  }
}
