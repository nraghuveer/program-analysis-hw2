package hwk

import scala.collection.mutable
import scala.collection.mutable.Queue;

case class Analysis(statement: Statement) extends ControlFlowBuilder {
  type Pair = (String, Long)
  val variables: Set[String] = vars(statement);
  // all variables assigned in this program
  private def vars(stmt: Statement): Set[String] = {
    stmt match {
      case Script(stmts) => vars(stmts)
      case BlockStmt(stmts) => vars(stmts)
      case VarDeclListStmt(stmts) => vars(stmts)
      case VarDeclStmt(x, e) => Set(x.str)
      case ExprStmt(AssignExpr(_, LVarRef(n), _)) => Set(n)
      case IfStmt(_, t, e) => vars(t).union(vars(e))
      case WhileStmt(_, b) => vars(b)
      case DoWhileStmt(_, b) => vars (b)
      case SwitchStmt(_, cases, d) => (d match { case Some(c) => c::cases case None => cases }).map(c => vars(c)).reduce((a,b)=>a.union(b))
      case CaseStmt(_, s) => vars(s)
      case _ => Set()
    }
  }

  this.build(statement, StartStatement())
  this.generateCodeLabels(statement)

  var succ: Map[Long, Set[Long]] = this.flow.map(_._1).map(p => (p ->
    this.flow.filter(_._1 == p).map(_._2).map(_.asInstanceOf[Long]).toSet
    )).toMap
  var pred: Map[Long, Set[Long]] = this.flow.map(_._2).map(p => (p ->
    this.flow.filter(_._2 == p).map(_._1).map(_.asInstanceOf[Long]).toSet
    )).toMap


  var worklist_queue = new mutable.Queue[Long]();
  worklist_queue = worklist_queue ++ this.succ.map(_._1).map(_.asInstanceOf[Long]).toSeq.sorted.toList
  val default_entry: List[Pair] = variables.map(t => (t, -1).asInstanceOf[Pair]).toList
  var rdExit: Map[Long, List[Pair]] = worklist_queue.map((_ -> default_entry)).toMap
  rdExit = rdExit + (-1.asInstanceOf[Long] -> List[Pair]())
  var rdEntry: Map[Long, List[Pair]] = worklist_queue.map((_ -> default_entry)).toMap
  rdEntry = rdEntry + (-1.asInstanceOf[Long] -> List[Pair]())

  override def getXLabel(id: Long): String = {
    if(id == -1){
      return ""
    }
    rdEntry(id).mkString(",") +"\n" + rdExit(id).mkString(",")
  }

  private def vars(stmts: List[Statement]): Set[String] = stmts.map(s => vars(s)).reduce((a, b)=> a.union(b))

  private def pairUnion(pairs: List[List[Pair]]): List[Pair] = {
    pairs.flatten.distinct.toList
  }

  private def removeKillPairs(rdSet: List[Pair], killVariables: List[String]): List[Pair] = {
    rdSet.filter(p => !killVariables.contains(p._1)).toList
  }

  private def genEntryExit(name: String, curid: Long): Tuple2[List[Pair], List[Pair]] = {
    val entry = if (pred(curid).size > 0) pairUnion(pred(curid).map(rdExit(_)).toList) else rdEntry(curid)
    val genset = List((name, curid))
    // remove all the pairs with this variable
    (entry, entry.filter(p => p._1 != name).toList :+ (name, curid))
  }

  def InitRdExit = {
    // Init the exit of each node with bottom
    worklist_queue.map(id => {
      stmtIdMap(id) match {
        case VarDeclStmt(x, e) => {
          val exit = rdExit(id).filter(p => p._1 != x.str) :+ (x.str, id)
          rdExit = rdExit.updated(id, exit)
        }
        case ExprStmt(AssignExpr(_, LVarRef(n), _)) => {
          val exit = rdExit(id).filter(p => p._1 != n) :+ (n, id)
          rdExit = rdExit.updated(id, exit)
        }
        case _ => Set()
      }
    })

  }

  def worklist: Unit = {
    println(worklist_queue)
    println(succ)
    println(pred)
    // dequeue a node, until list is empty
    while(worklist_queue.nonEmpty) {
      // calculate entry and exit of the current node
      val curid = worklist_queue.dequeue()
      // get the statement related to this id
      val cur_stmt = this.stmtIdMap.getOrElse(curid, throw new RuntimeException("id not found in the stmtIdMap!"))

      // calculate the entry of n
      // entry is union of exit of pred

      // calculate the exit
        // (entry - kill set) union genset

      cur_stmt match {
        case varDeclStmt: VarDeclStmt => {
          val (entry, exit) = genEntryExit(varDeclStmt.name.str, curid)
          if(exit == rdExit(curid)){
            println("No difference!")
          }
          else{
            println(s"difference! Adding ${succ(curid).toList}")
            worklist_queue = worklist_queue ++ succ(curid).toList
          }

          rdEntry = rdEntry.updated(curid, entry)
          rdExit = rdExit.updated(curid, exit)

        }
        case ExprStmt(AssignExpr(_, LVarRef(n), _)) => {
          val (entry, exit) = genEntryExit(n, curid)
          if(exit == rdExit(curid)){
            println("No difference!")
          }
          else{
            println(s"difference! Adding ${succ(curid).toList}")
            worklist_queue = worklist_queue ++ succ(curid).toList
          }

          rdEntry = rdEntry.updated(curid, entry)
          rdExit = rdExit.updated(curid, exit)
        }
        case startStatement: StartStatement => {}
        case _ => {
          // Attach the preds' exit to entry and entry to exit
          val entry = pairUnion(pred(cur_stmt.id).map(rdExit(_)).toList)
          val exit = entry
          rdEntry = rdEntry.updated(curid, entry)
          if(exit != rdExit(curid)){
            println(s"Difference! Adding ${succ(curid).toList}")
            worklist_queue= worklist_queue ++ succ(curid).toList
          }
          rdExit = rdExit.updated(curid, exit)
      }
    }
  }

    println("Entry")
    rdEntry.toSeq.sortBy(_._1).foreach(p => println(p._1 + " -> " + p._2))
    println("++++++++++++++")
    println("Exit")
    rdExit.toSeq.sortBy(_._1).foreach(p => println(p._1 + " -> " + p._2))


  }


}
