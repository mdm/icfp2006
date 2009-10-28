class UniversalMachine {
    private var registers = new Array[Int](8)
    private var arrays = new Array[Array[Int]](1)
    private var finger: Int = 0
    private var isRunning

    private def opConditionalMove(a: Int, b: Int, c:Int):Int = {
	if (registers(c) != 0) registers(a) = registers(b)
	++finger
    }

    private def opArrayIndex(a: Int, b: Int, c:Int):Int = {
	registers(a) = arrays(registers(b))(registers(c))
	++finger
    }

    private def opArrayAmendment(a: Int, b: Int, c:Int):Int = {
	arrays(registers(a))(registers(b)) = registers(c)
	++finger
    }

    def run() {
	while (isRunning) {
	    val operator = arrays(0)(finger)
	    val opcode = operator >>> 28
	    val a = operator << 1 >> 7 
	    val b = operator << 1 >> 4 
	    val c = operator << 1 >> 1 
	    
	    opcode match {
		case 0 => opConditionalMove(a, b, c)
		case 1 => opArrayIndex(a, b, c)
		case 2 => opArrayAmendment(a, b, c)
		case 3 => opAddition(a, b, c)
		case 4 => opMultiplication(a, b, c)
		case 5 => opDivision(a, b, c)
		case 6 => opNotAnd(a, b, c)
		case 7 => opHalt()
		case 8 => opAllocation(a, b, c)
		case 9 => opAbandonment(c)
		case 10 => opOutput(c)
		case 11 => opInput(c)
		case 12 => opLoadProgram(b, c)
		case 13 => opOrthography(operator)
		case _ => fail("ERROR: Unkown operation.")
	    }
	}
    }
}
