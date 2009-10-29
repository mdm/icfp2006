package um
import java.io.{ByteArrayOutputStream, FileInputStream}

object UM {
    def main(args: Array[String]) {
	if (args.length == 0) {
	    println("Usage: scala um.UM <filename>")
	}
	else {
	    val um = new UniversalMachine(pack(args(0)))
	    um.run()
	}
    }

    def pack(filename: String): Array[Int] = {
	val buffer = new Array[Byte](2048)
	val is = new FileInputStream(filename)
	val os = new ByteArrayOutputStream

	var n: Int = 0
	while (n != -1) {
	    os.write(buffer, 0, n)
	    n = is.read(buffer)
	}

	val bytes = os.toByteArray
	val words = bytes.length / 4
	val program = new Array[Int](words)
	
	var word = 0
	while (word < words) {
	    val offset = 4 * word
	    program(word) = (bytes(offset) << 24) & 0xff000000
	    program(word) |= (bytes(offset + 1) << 16) & 0x00ff0000
	    program(word) |= (bytes(offset + 2) << 8) & 0x0000ff00
	    program(word) |= bytes(offset + 3) & 0x000000ff

	    if (word == 0) {
		Console.withOut(Console.err)(printf("Bytes: %2x, %2x, %2x, %2x\n Operator: %d (%08x)\n\n", bytes(offset), bytes(offset + 1), bytes(offset + 2), bytes(offset + 3), program(word), program(word)))
	    }

	    word += 1
	}
	
	println(words)
	return program
    }
}
