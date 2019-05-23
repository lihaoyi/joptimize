package joptimize.backend

import joptimize.Logger
import joptimize.model.SSA

object PartialEvaluator {

  def replaceJump(
    branch: SSA.Jump,
    directNext: SSA.Block,
    liveBlocks: SSA.Block => Boolean,
    log: Logger.InferredMethod
  ) = {
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
    for (phi <- directNext.nextPhis) {
      phi.replaceUpstream(directNext, branch.block)
      branch.block.nextPhis ++= Seq(phi)
    }

    for (up <- branch.upstreamVals) {
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
        val arr = phi.incoming.toArray
        phi.incoming.clear()
        for ((k, v) <- arr) {
          if (k == directNext) phi.incoming(branch.block) = v
          else if (branchBlocks(k)) {
            k.next = null
            v.downstreamRemove(phi)
          } else phi.incoming(k) = v

        }
        phi

      case r: SSA.Merge =>
        val arr = r.incoming.toArray
        r.incoming.clear()
        for ((k, v) <- arr) {
          if (k == directNext) r.incoming(branch.block) = branch.block.nextState
          else if (branchBlocks(k)) {
            k.next = null
          } else r.incoming(k) = v
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
