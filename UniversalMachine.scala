package um

class UniversalMachine(program: Array[Int]) {
    private var registers = new Array[Int](8)
    private var arrays = new Array[Array[Int]](1)
    private var freeList: List[Int] = Nil
    private var finger: Int = 0
    private var isRunning: Boolean = true

    arrays(0) = program

    private def opConditionalMove(a: Int, b: Int, c:Int) {
	//Console.withOut(Console.err)(printf("Copy register %d (%08x) to register %d, if register %d (%08x) is not 0. \n\n", b, registers(b), a, c, registers(c)))
	if (registers(c) != 0) registers(a) = registers(b)
	finger += 1
    }

    private def opArrayIndex(a: Int, b: Int, c:Int) {
	//Console.withOut(Console.err)(printf("Copy array location \"%08x[%08x]\" (%08x) to register %d\n\n", registers(b), registers(c), arrays(registers(b))(registers(c)), a))
	registers(a) = arrays(registers(b))(registers(c))
	finger += 1
    }

    private def opArrayAmendment(a: Int, b: Int, c:Int) {
	//Console.withOut(Console.err)(printf("Copy register %d (%08x) to array location \"%08x[%08x]\"\n\n", c, registers(c), registers(b), registers(c)))
	arrays(registers(a))(registers(b)) = registers(c)
	finger += 1
    }

    private def opAddition(a: Int, b: Int, c:Int) {
	//Console.withOut(Console.err)(printf("Add register %d (%08x) to register %d (%08x) and store the result (modulo 2^32) in register %d\n\n", b, registers(b), c, registers(c), a))
	registers(a) = registers(b) + registers(c)
	finger += 1
    }

    private def opMultiplication(a: Int, b: Int, c:Int) {
	//Console.withOut(Console.err)(printf("Multiply register %d (%08x) with register %d (%08x) and store the result (modulo 2^32) in register %d\n\n", b, registers(b), c, registers(c), a))
	registers(a) = registers(b) * registers(c)
	finger += 1
    }

    private def opDivision(a: Int, b: Int, c:Int) {
	//Console.withOut(Console.err)(printf("Divide register %d (%08x) by register %d (%08x) and store the result (modulo 2^32) in register %d\n\n", b, registers(b), c, registers(c), a))
	if (registers(c) == 0) fail("ERROR: Division by zero.")
	val unsigned_b = (registers(b).toLong << 32) >>> 32
	val unsigned_c = (registers(c).toLong << 32) >>> 32
	val result = unsigned_b / unsigned_c
	registers(a) = result.toInt
	//Console.withOut(Console.err)(printf("Result %d (%08x)\n\n", registers(a), registers(a)))
	finger += 1
    }

    private def opNotAnd(a: Int, b: Int, c:Int) {
	//Console.withOut(Console.err)(printf("Calculate \"register %d (%08x) NAND register %d (%08x)\" and store the result in register %d\n\n", b, registers(b), c, registers(c), a))
	registers(a) = ~(registers(b) & registers(c))
	finger += 1
    }

    private def opHalt() {
	Console.withOut(Console.err)(printf("Halting the Universal Machine.\n\n"))
	isRunning = false
	finger += 1
    }

    private def opAllocation(b: Int, c:Int) {
	//Console.withOut(Console.err)(printf("Allocate an array of the size given in register %d (%08x) and store its identifier in register %d\n\n", c, registers(c), b))
	//Console.withOut(Console.err)(println(arrays.length))
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
	//Console.withOut(Console.err)(printf("# allocated arrays: %d\n", arrays.length))
	finger += 1
    }

    private def opAbandonment(c:Int) {
	//Console.withOut(Console.err)(printf("Free array with the identifier given in register %d (%08x)\n\n", c, registers(c)))
	arrays(registers(c)) = null
	freeList = registers(c)::freeList
	finger += 1
    }

    private def opOutput(c:Int) {
	//Console.withOut(Console.err)(printf("Write character given in register %d (%08x) to the console\n\n", c, registers(c)))
	if (c < 0 || c > 255) fail("ERROR: Illegal output.")
	print(registers(c).toChar)
	finger += 1
    }

    private def opInput(c:Int) {
	//Console.withOut(Console.err)(printf("Read a character from the console and store it in register %d\n\n", c))
	registers(c) = System.in.read
	finger += 1
    }

    private def opLoadProgram(b: Int, c:Int) {
	//Console.withOut(Console.err)(printf("Load program from array with identifier given in register %d (%08x) and start execution at offset given in register %d (%08x)\n\n", b, registers(b), c, registers(c)))
	if (registers(b) != 0) {
	    arrays(0) = arrays(registers(b))
	    arrays(0) = new Array[Int](arrays(registers(b)).length)
	    Array.copy(arrays(registers(b)), 0, arrays(0), 0, arrays(0).length)
	}
	finger = registers(c)
	//Console.withOut(Console.err)(printf("Execution finger at: #%d (%08x)\n\n", finger, finger))
    }

    private def opOrthography(operator:Int) {
	val a = (operator >> 25) & 7
	registers(a) = operator & 0x01ffffff
	//Console.withOut(Console.err)(printf("Store value \"%08x\" in register %d\n\n", registers(a), a))
	finger += 1
    }

    private def fail(error: String) {
	println(error)
	opHalt();
    }

    def run() {
	var counter: Long = 0
	val start: Long = System.currentTimeMillis

	while (isRunning) {
	    val operator = arrays(0)(finger)
	    val opcode = operator >>> 28
	    val a = (operator >> 6) & 7
	    val b = (operator >> 3) & 7 
	    val c = operator & 7

	    //Console.withOut(Console.err)(printf("Operator #%d: %d (%08x)\nOpcode: %d, Registers: %d (%08x), %d (%08x), %d (%08x)\n", finger, operator, operator, opcode, a, registers(a), b, registers(b), c, registers(c)))
	    
	    opcode match {
		case 0 => opConditionalMove(a, b, c)
		case 1 => opArrayIndex(a, b, c)
		case 2 => opArrayAmendment(a, b, c)
		case 3 => opAddition(a, b, c)
		case 4 => opMultiplication(a, b, c)
		case 5 => opDivision(a, b, c)
		case 6 => opNotAnd(a, b, c)
		case 7 => opHalt()
		case 8 => opAllocation(b, c)
		case 9 => opAbandonment(c)
		case 10 => opOutput(c)
		case 11 => opInput(c)
		case 12 => opLoadProgram(b, c)
		case 13 => opOrthography(operator)
		case _ => fail("ERROR: Unkown operation.")
	    }
	    
	    //for (register <- registers) {
	    //	Console.withOut(Console.err)(printf("%08x\n", register))
	    //}
	    //Console.withOut(Console.err)(println("\n"))
	    //*/


	    counter += 1
	}
	val time: Float = (System.currentTimeMillis - start) / 1000.0f
	Console.err.printf("%f ops/sec on average\n", float2Float(counter / time))
    }
}
