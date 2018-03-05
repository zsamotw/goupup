package varia

object graph {
	case class Node[A](value: A, nodes: Set[A], color: String)
	case class Graph[A](nodes: List[Node[A]])
	
	def simpleBsf[A](graph: Graph[A], start: A): List[Node[A]] = {
		def loop(elem: A, visited: List[Node[A]]): List[Node[A]] = {
			val node = graph.nodes.find(nd => nd.value == elem).get
		 	if(visited contains node) visited
		 	else {
		 		node.nodes.foldLeft(node :: visited)((nds, nd) => loop(nd, nds))
		 	}
		}
		loop(start, List[Node[A]]())
	}                                         //> simpleBsf: [A](graph: varia.graph.Graph[A], start: A)List[varia.graph.Node[A
                                                  //| ]]
	
	//def shortestPath[A](graph: Graph[A], start: A, finish: A): List[Node[A]] = {
	//	def loop(graph: Graph[A], start: A, finish: A): List[List[Node[A]]] = {
	//		val node = graph.nodes.find(_.value == start).get
	//		if(node.value == finish || node.nodes == Set.empty) Nil
	//		else {
	//			node.nodes.toList.map(v => node :: loop(graph, v, finish))
	//		}
	//	}
	//}
	
	
	
	val graph = Graph[Int](List(Node(1, Set(2,3), "white"), Node(2, Set(1,3), "white"), Node(3, Set(1,2,4), "white"), Node(4, Set.empty, "white"), Node(5, Set.empty, "white")))
                                                  //> graph  : varia.graph.Graph[Int] = Graph(List(Node(1,Set(2, 3),white), Node(
                                                  //| 2,Set(1, 3),white), Node(3,Set(1, 2, 4),white), Node(4,Set(),white), Node(5
                                                  //| ,Set(),white)))
	
	
	val res = simpleBsf(graph, 1)             //> res  : List[varia.graph.Node[Int]] = List(Node(4,Set(),white), Node(3,Set(1
                                                  //| , 2, 4),white), Node(2,Set(1, 3),white), Node(1,Set(2, 3),white))
}