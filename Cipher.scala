object Cipher{
  /** Bit-wise exclusive-or of two characters */
  def xor(a: Char, b: Char) : Char = (a.toInt ^ b.toInt).toChar

  /** Print ciphertext in octal */
  def showCipher(cipher: Array[Char]) =
    for(c <- cipher){ print(c/64); print(c%64/8); print(c%8); print(" ") }

  /** Read file into array */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray

  /** Read from stdin in a similar manner */
  def readStdin() = scala.io.Source.stdin.toArray

  /* ----- Functions below here need to be implemented ----- */

	/* Task 1 */

  /** Encrypt plain using key; can also be used for decryption */
  def encrypt(key: Array[Char], plain: Array[Char]) : Array[Char] = {
	
	var len= plain.size
	var cipher = new Array[Char](len)
	var i = 0
	
	while(i<len){
		cipher(i)= xor(plain(i),key(i%key.size))
		i+=1
	}
	cipher
}
	/* Task 2 */

  /* Try to decrypt ciphertext, using crib as a crib */
  def tryCrib(crib: Array[Char], ciphertext: Array[Char]) = {
	var start = 0
	var j = 0
	var foundKey = 0
	var k = crib.size
	var keyChars = new Array[Char](k)
	while(foundKey==0){
		/* Step 1 : Creating a sequence that may contain our key */
		j=start+1
		while(j < k+start-1) keyChars(j%k) = xor(ciphertext(j),crib(j%k))

		/* Step 2 : Finding if there is a key */
		j=1
		while(foundKey == 0){
			if(keyChars(0)==keyChars(j)) foundKey = findKey(keyChars,j)
			j+=1
		}
		start+=1
	}
	var key = new Array[Char](j-1)
	/* The loop stops when we found a key */
	/* Step 3 : Printing */
	start=0
	do{	
		key(start) = keyChars(start)
		start+=1
	}while(start<j)
	println(Array[Char] key)
	print(new String (encrypt(key,ciphertext)))
  }


  /* Checks if we found the start of the repetition */
  def findKey(keyChars: Array[Char],j: Int) : Int = {
	var start = 1
	var k = keyChars.size
	j+=1
	while(keyChars(start) == keyChars(j) && j<k){
		j+=1
		start+=1
	}
	if(j==k) return 1
	else return 0
  }


  /** The first optional statistical test, to guess the length of the key */
  def crackKeyLen(ciphertext: Array[Char]) = ???

  /** The second optional statistical test, to guess characters of the key. */
  def crackKey(klen: Int, ciphertext: Array[Char]) = ???

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {
    // string to print if error occurs
    val errString = 
      "Usage: scala Cipher (-encrypt|-decrypt) key [file]\n"+
      "     | scala Cipher -crib crib [file]\n"+
      "     | scala Cipher -crackKeyLen [file]\n"+
      "     | scala Cipher -crackKey len [file]"

    // Get the plaintext, either from the file whose name appears in position
    // pos, or from standard input
    def getPlain(pos: Int) = 
      if(args.length==pos+1) readFile(args(pos)) else readStdin

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
    val command = args(0)
    if(command=="-encrypt" || command=="-decrypt"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      print(new String (encrypt(key,plain)))
    }
    else if(command=="-crib"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      tryCrib(key, plain)
    }
    else if(command=="-crackKeyLen"){
      checkNumArgs(1); val plain = getPlain(1)
      crackKeyLen(plain)
    }      
    else if(command=="-crackKey"){
      checkNumArgs(2); val klen = args(1).toInt; val plain = getPlain(2)
      crackKey(klen, plain)
    }
    else println(errString)
  }
}
