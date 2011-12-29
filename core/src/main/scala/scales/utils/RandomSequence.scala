package scales.utils

/**
 * Currently thread safe, java doc issue pending http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6498354
 */
object RandomSequence {
  import java.security.NoSuchAlgorithmException;
  import java.security.SecureRandom;
  private var keyGen = SecureRandom.getInstance("SHA1PRNG");

  /**
   * Gens a sequence of up to len bytes and converts to base 64
   */
  def next64Sequence(len: Int): String = {
    val bytes = new Array[Byte](len)
    keyGen.nextBytes(bytes)

    // turn the bytes to base64
    new String(org.apache.commons.codec.binary.Base64.encodeBase64(bytes))
  }

  /**
   * Returns 16 bytes of base64 random sequence
   */
  def next64Sequence: String = next64Sequence(16)
}
