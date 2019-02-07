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
	var start = 0 				/* Represents the position from which we seek the keyword */
	var j = 0
	var foundKey = 0			/* Counter for checking if we found the key*/
	var k = crib.size			/* Size of the memorised keyword*/
	var keyChars = new Array[Char](k)	/* Array where we store decrypted parts of the whole text */
	while(foundKey==0){
		/* Step 1 : Creating a sequence that may contain our key */
		j=start
		while(j < k+start){
			keyChars(j%k) = xor(ciphertext(j),crib(j%k))
			j+=1
		}/*Generates key from decrypting coded text with crib(remembered word) */

		/* Step 2 : Finding if there is a key */
		j=1
		while(foundKey == 0 && j < k){
			if(keyChars(0)==keyChars(j)) foundKey = findKey(keyChars,j)
			j+=1
			/* The loop stops when:
			1) we found a key and j stores the size of the key or
			2) there is no repetition and no key*/
		}
		start+=1 
	}
	var key = new Array[Char](j-1)

	/* Step 3 : Printing */
	start=0
	do{	
		key(start) = keyChars(start)
		start+=1
	}while(start<j)
	println(Array[Char] key)
	print(new String (encrypt(key,ciphertext)))
  }


  /* Checks if we found the start of the repetition (key) */
  def findKey(keyChars: Array[Char],j: Int) : Int = {
	var start = 1
	var k = keyChars.size
	j+=1
	while(keyChars(start) == keyChars(j) && start<k){
		j+=1
		start+=1
		/* Compares all the characters of the word */
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
