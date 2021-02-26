package hwk

import scala.collection.mutable
import scala.collection.mutable.Queue;

case class Analysis(statement: Statement) extends ControlFlowBuilder {
  type Pair = (String, Long)

  var rdEntry = Map[Long, List[Pair]]();
  var rdExit = Map[Long, List[Pair]]();

  // all variables assigned in this program
  val variables: Set[String] = vars(statement);
  val rdInitExit = variables.map(t => (t, -1).asInstanceOf[Pair]).toList
  val rdInitEntry = variables.map(t => (t, -1).asInstanceOf[Pair]).toList
  this.generateCodeLabels(statement)
  this.build_program_flow(statement, StartStatement());
  var succ: Map[Long, Set[Long]] = this.flow.map(_._1).map(p => (p ->
    this.flow.filter(_._1 == p).map(_._2).map(_.asInstanceOf[Long]).toSet
    )).toMap
  var pred: Map[Long, Set[Long]] = this.flow.map(_._2).map(p => (p ->
    this.flow.filter(_._2 == p).map(_._1).map(_.asInstanceOf[Long]).toSet
    )).toMap

  // remove -1 from the pred and succ
  succ = succ.filter(_._1 != -1)
  pred = pred.filter(!_._2.contains(-1))


  var worklist_queue = new mutable.Queue[Long]();
  this.worklist_queue = worklist_queue ++ this.succ.map(_._1).map(_.asInstanceOf[Long]).toList

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

  private def vars(stmts: List[Statement]): Set[String] = stmts.map(s => vars(s)).reduce((a, b)=> a.union(b))

//  def worklist: Unit = {
//    while(work_list.nonEmpty) {
//      var cur = work_list.dequeue();
//
//    }
//  }


}
