
package hwk;

import java.io.File
 

object Main {
  def main(args: Array[String]) { 
    // val ast = GenerateAST(new File("test/simple.js"))
    val ast = GenerateAST(new File("D:\\mscs\\738\\hw\\2\\hwk2_solution\\hwk2\\test\\while.js"))
    ast.prep
    
    print(ast)
    
    ast.buildGraph(ast)

  }
}
