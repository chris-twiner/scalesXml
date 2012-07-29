package scales

package object utils
{
  def error(str : String) = Predef.error(str)

  /**
   * Simple grabber of resources
   */ 
  def resource(a : AnyRef, path : String) =
    a.getClass.getResource(path)
}
