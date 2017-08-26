import scala.io.Source
object Huffman {
  //trait of a tree which can be extended later
  trait Tree[+A]

  //case classes of leaf and branch which extends trait Tree
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
 
  // make a binary tree by merging 2 lowest trees and then dropping two trees which have been merged into a new tree, recursively
  def merge_trees(tree_List: List[(Tree[Char], Int)]): List[(Tree[Char], Int)] = {
    if (tree_List.length != 1) {
      //left node of a tree
      val left = tree_List.head
      //right node of a tree
      val right = tree_List.tail.head  
      //merged two lowest trees into a single tree
      val merged_trees = (Branch(left._1, right._1), left._2 + right._2)
      //dropping the trees that have just been merged 
      merge_trees((merged_trees :: tree_List.drop(2)).sortBy(_._2))
    } 
    //if tree length is 1, return
    else tree_List  
 }
  // checkif the required character is present in the tree, search for it recursively

  // match is used for pattern matching
  def contain(tree: Tree[Char], char: Char): Boolean = tree match {
    //if leaf has a char, return true or false
    case Leaf(c) => if (c == char) true else false
    //if leaf hasnt, so search in the branches further for that character, left node or right node of the tree
    case Branch(left, right) => contain(left, char) || contain(right, char)
  }
 
  // traversing from the root of the tree, to get a required code for every alphabet, encoding the alphabets 
  // and then recursively making a string of the code representation
  //when a left branch is chosen, a 0 is added to the string and 
  //when a right branch is choses a 1 is added

  def encode(tree: Tree[Char], char: Char): String = {
    //match is used for pattern matching
    def build_string(tree: Tree[Char], char: Char, code: String): String = tree match {
      //if leaf node becomes, return node
      case Leaf(_) => code
      //if left, add 0, if right node, add 1 to the string
      case Branch(left, right) => if (contain(left, char)) build_string(left, char, code + '0') else build_string(right, char, code + '1')
    }
    //recursive calls
    build_string(tree, char, "")
  }
 
  def main(args: Array[String]) {
  
  //reading the file meno.1b.txt 
  val line = io.Source.fromFile("meno.1b.txt").getLines.toList
  //it gives us a List[String], so converting it into a String
	val string = line.mkString(" ")
  //converting the text into LowerCase
	val text = string.toLowerCase
	
	
	// making the string into a list of tuple
    

    val frequencies = text.groupBy(chars => chars).mapValues(group => group.length).toList.map(x => (Leaf(x._1), x._2)).sortBy(_._2)
    // build the Huffman Tree for this 
    val huffmanTree = merge_trees(frequencies).head._1
    // output character codes
    println("Char\tFrequency\tCode")

    frequencies.foreach(x => println(x._1.value  + "\t" + x._2 + s"\t${encode(huffmanTree, x._1.value)}"))
  }
}