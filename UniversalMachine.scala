package um

class UniversalMachine(program: Array[Int]) {
    private var registers = new Array[Int](8)
    private var arrays = new Array[Array[Int]](1)
    private var freeList: List[Int] = Nil
    private var finger: Int = 0
    private var isRunning: Boolean = true

    //private val opCounts = new Array[Long](14)

    arrays(0) = program

    private def fail(error: String) {
	Console.err.println(error)
	isRunning = false
    }

    def run() {
	while (isRunning) {
	    val operator = arrays(0)(finger)
	    val opcode = operator >>> 28
	    val a = (operator >> 6) & 7
	    val b = (operator >> 3) & 7 
	    val c = operator & 7
	
	    //opCounts(opcode) += 1
	    //if (opCounts(opcode) == java.lang.Long.MAX_VALUE) isRunning = false
	    opcode match {
		case 0 =>
			if (registers(c) != 0) registers(a) = registers(b)
			finger += 1
		case 1 =>
			registers(a) = arrays(registers(b))(registers(c))
			finger += 1
		case 2 =>
			arrays(registers(a))(registers(b)) = registers(c)
			finger += 1
		case 3 =>
			registers(a) = registers(b) + registers(c)
			finger += 1
		case 4 =>
			registers(a) = registers(b) * registers(c)
			finger += 1
		case 5 =>
			//Console.err.println("blub")
			if (registers(c) == 0) fail("ERROR: Division by zero.")
			val unsigned_b = (registers(b).toLong << 32) >>> 32
			val unsigned_c = (registers(c).toLong << 32) >>> 32
			val result = unsigned_b / unsigned_c
			registers(a) = result.toInt
			finger += 1
		case 6 =>
			registers(a) = ~(registers(b) & registers(c))
			finger += 1
		case 7 =>
			Console.err.println("Halting the Universal Machine.\n")
			isRunning = false
			finger += 1
		case 8 =>
			if (freeList == Nil) {
			    val box = new Array[Array[Int]](1)
			    box(0) = new Array[Int](registers(c))
			    registers(b) = arrays.length
			    arrays = arrays ++ box
			}
			else {
			    arrays(freeList.head) = new Array[Int](registers(c))
			    registers(b) = freeList.head
			    freeList = freeList.tail
			}
			finger += 1
		case 9 =>
			arrays(registers(c)) = null
			freeList = registers(c)::freeList
			finger += 1
		case 10 =>
			if (c < 0 || c > 255) fail("ERROR: Illegal output.")
			System.out.write(registers(c))
			System.out.flush
			finger += 1
		case 11 =>
			registers(c) = System.in.read
			finger += 1
		case 12 =>
			if (registers(b) != 0) {
			    arrays(0) = new Array[Int](arrays(registers(b)).length)
			    Array.copy(arrays(registers(b)), 0, arrays(0), 0, arrays(0).length)
			}
			finger = registers(c)
		case 13 =>
			val a2 = (operator >> 25) & 7
			registers(a2) = operator & 0x01ffffff
			finger += 1
		case _ => fail("ERROR: Unkown operation.")
	    }
	    
	    //for (register <- registers) {
	    //	Console.withOut(Console.err)(printf("%08x\n", register))
	    //}
	    //Console.withOut(Console.err)(println("\n"))
	    //*/
	}

	//var i = 0
	//while (i < 14) {
		//Console.err.printf("Operation %d was executed %d times in %d ms.\n", i, opCounts(i), opTimes(i))
	//	Console.err.print(i + ", ")
	//	Console.err.println(opCounts(i))
	//	i += 1
	//}
    }
}
