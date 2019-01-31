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

/** Try to decrypt ciphertext, using crib as a crib */
def tryCrib(crib: Array[Char], ciphertext: Array[Char]) = ???

/** The first optional statistical test, to guess the length of the key */
def crackKeyLen(ciphertext: Array[Char]) = ???

/** The second optional statistical test, to guess characters of the key. */
def crackKey(klen: Int, ciphertext: Array[Char]) = ???

/** The main method just selects which piece of functionality to run */
def main(args: Array[String]) = {
// string to print if error occurs
val errString =
"Usage: scala Cipher (-encrypt|-decrypt) key [file]\n"+
" | scala Cipher -crib crib [file]\n"+
" | scala Cipher -crackKeyLen [file]\n"+
" | scala Cipher -crackKey len [file]"

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
