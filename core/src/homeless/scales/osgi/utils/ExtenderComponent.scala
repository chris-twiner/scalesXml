package org.scales.osgi.utils

import java.util.List
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue

import org.osgi.framework.Bundle
import org.osgi.framework.BundleContext
import org.osgi.framework.BundleEvent
import org.osgi.framework.BundleException
import org.osgi.framework.BundleListener
import org.osgi.framework.ServiceReference
import org.osgi.service.component.ComponentContext

import org.scales.utils.ConcurrentMapUtils._
import org.scales.utils.IterableConversions._
import org.scales.utils.Logs

/*
 * This class provides simple base functionality for the Extender Pattern and for DS whiteboard based
 * inclusion.  It manages a simple list of appropriate components.  NOTE that this list may vary between
 * calls and should be assumed to change.
 * 
 * Developers should subclass this and add an OSGi DS Component declaration.  Developers can also 
 * change constructor behaviour by overriding newObject.
 * 
 * Each type T can optionally have Bundles injected after creation by overriding the newObject with a bundle parameter function 
 *
 * Type C is used to pass through object creation code, allowing extra information to be stored during creation processs.
 */
abstract class ExtenderComponent[T, C](val extension: String) extends BundleListener with Logs {

  /**
   * From extension pattern, items in the Manifest
   */
  protected val bundlesToObject = new ConcurrentHashMap[Bundle, ConcurrentLinkedQueue[T]]()

  // shouldn't have 1000's of objects
  protected val objects = new ConcurrentLinkedQueue[T]()

  protected var context: BundleContext = null

  /**
   * List of ServiceReference that are added before the component is activated.  These are then pumped through.
   * This is required as DS will try to call activate at the last possible stage.
   * 
   * Private as its not actually required by any other implementation to ever look at this info.
   */
  private val beforeActivation = new ConcurrentLinkedQueue[ServiceReference]

  /**
   * Seperate activator for components to override.  When this has returned all bundles will be added.
   * 
   * @param cc
   */
  protected def activateComponent(cc: ComponentContext) {
  }

  final protected def activate(cc: ComponentContext) {
    context = cc.getBundleContext();

    // call parent "constructor"
    activateComponent(cc);

    // now we are activated we can flood the service references before the activation
    for (ref <- beforeActivation) {

      val res = getObjectAndBundle(ref)
      if (res.isDefined) {
        var (obj, bundle) = res.get
        addObject(obj, bundle)
      }

    }
    beforeActivation.clear // not needed anymore

    context.addBundleListener(this);

    // add all registered ones....
    val bundles = context.getBundles()
    for { bundle <- bundles } {
      if ((bundle.getState() & (Bundle.STARTING | Bundle.ACTIVE)) != 0) {
        registerBundle(bundle);
      } else {
        startBundle(bundle);
      }
    }

  }

  /**
   * Seperate destructor for components to override.  It is called after this service has been removed from the bundle listeners
   * 
   * @param cc
   */
  protected def deActivateComponent(cc: ComponentContext) {
  }

  protected def deactivate(cc: ComponentContext) {
    context.removeBundleListener(this);
    deActivateComponent(cc);
  }

  /**
   * If the bundle is not required this is reasonable function to use as a setter
   * 
   * @param object
   */
  def addObject(obj: T) {
    addObject(obj, null, null)
  }

  /**
   * If the bundle is not required this is reasonable function to use as a setter
   * 
   * @param object
   */
  def addObject(obj: T, capturedInfo: (String, C)) {
    addObject(obj, null, capturedInfo)
  }

  /**
   * Override if the bundle is required
   * 
   * @param object
   * @param bundle
   */
  def addObject(obj: T, bundle: Bundle) {
    addObject(obj, bundle, null.asInstanceOf[(String, C)])
  }

  /**
   * Override if the bundle is required
   * 
   * @param object
   * @param bundle
   */
  def addObject(obj: T, bundle: Bundle, capturedInfo: (String, C)) {
    if (objects.contains(obj)) {
      return ;
    }
    objects.add(obj);
  }

  private def getObjectAndBundle(reference: ServiceReference): Option[(T, Bundle)] = {
    val obj = try {
      context.getService(reference).asInstanceOf[T with AnyRef];
    } catch {
      case t: Throwable => {
        log error ("getService threw with reference " + reference.toString, t)
        throw t
      }
    }
    // the reference is good but something can't actually be created.
    if (obj eq null) {
      log error "Could not getService from reference " + reference.toString
      None
    } else Some((obj, reference.getBundle()))
  }

  /**
   * Can be called instead of addObject to allow gaining the bundle 
   * 
   * @param reference
   */
  def addObjectReference(reference: ServiceReference) {
    if (context eq null) {
      beforeActivation add reference
    } else {
      val res = getObjectAndBundle(reference)
      if (res.isDefined) {
        var (obj, bundle) = res.get
        addObject(obj, bundle)
      }
    }
  }

  def removeObject(obj: T) {
    objects.remove(obj);
  }

  def bundleChanged(event: BundleEvent) {
    val bundle = event.getBundle();
    event.getType() match {
      case BundleEvent.STARTED => registerBundle(bundle)
      case BundleEvent.RESOLVED => startBundle(bundle)
      case BundleEvent.STOPPED => {
        val itemsInBundle = removeList(bundle, bundlesToObject)

        for (item <- itemsInBundle) {
          removeObject(item)
        }
      }
      case _ => {}
    }
  }

  private[ExtenderComponent] def startBundle(bundle: Bundle) {
    try {
      /* as we are registered as a listener we should get the events that follow, leave it to the bundle changed
			 this is required as only in active state are the declaritive services initiated.  IF we load something
			 that is not already active and requires something from ds then it will npe */
      if (bundleShouldBeStarted(bundle)) {
        bundle.start();
      }
    } catch {
      case e: BundleException =>
        e printStackTrace ()
        log.error("exception trying to start bundle " + bundle.getSymbolicName(), e)
    }
  }

  /**
   * Returns a new instance of this object using clazz.newInstance, or null if there was a problem.
   * 
   * @param clazz
   * @param bundle 
   * @return
   */
  protected def newObject(clazz: Class[_], bundle: Bundle, c: C): T =
    clazz.newInstance().asInstanceOf[T]

  /**
   * By default it just returns the matchingHeader
   * 
   * (classname to load, useful information to pass)
   */
  protected def getClassName(matchingHeaderEntry: String): (String, C) =
    (matchingHeaderEntry, null.asInstanceOf[C])

  /**
   * By default just calls bundle.loadClass on the classname
   */
  protected def loadClass(bundle: Bundle, capturedInfo: (String, C)): Class[_] =
    bundle.loadClass(capturedInfo._1)

  private[ExtenderComponent] def registerBundle(bundle: Bundle) {
    val classnames = ExtenderComponent.getClasses(bundle, extension)
    for (matchingHeaderEntry <- classnames) {
      val (realClassName, c) = getClassName(matchingHeaderEntry)

      try {
        val clazz = loadClass(bundle, (realClassName, c))
        val objectList = getList(bundle, bundlesToObject)
        if (clazz != null) {
          val newObj = newObject(clazz, bundle, c)
          if (newObj != null) {
            addObject(newObj, bundle, (realClassName, c))
            objectList.add(newObj)
          }
        } else
          throw new IllegalArgumentException("Can not find class "
            + realClassName);
      } catch {
        case t: Throwable =>
          t printStackTrace ()
          log.error("Could not add Tab " + realClassName + " from bundle "
            + bundle.getLocation(), t);
      }
    }
  }

  /**
   * Can be treated as a final function but allows custom startup behaviour
   * @param bundle
   * @return
   */
  protected def bundleShouldBeStarted(bundle: Bundle): Boolean = {
    if (ExtenderComponent.getClasses(bundle, extension).size() > 0)
      true
    else false
  }

}

object ExtenderComponent {
  private[ExtenderComponent] def getClasses(bundle: Bundle, name: String) = {
    val classes = new java.util.ArrayList[String]()
    val header = bundle.getHeaders().get(name).asInstanceOf[String]
    if (header != null) {
      val clauses = header.split(",")
      for (clause <- clauses) {
        classes.add(clause)
      }
    }
    classes
  }

}